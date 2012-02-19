{-# LANGUAGE DeriveDataTypeable, Rank2Types, ScopedTypeVariables #-}
module Control.Pipe.Common (
  BrokenDownstreamPipe,
  BrokenUpstreamPipe,
  PipeF(..),
  Pipe,
  throw,
  catchP,
  catch,
  catchM,
  catch_,
  onException,
  finally,
  bracket,
  await,
  yield,
  lift,
  MaskState(..),
  lift_,
  tryAwait,
  pipe,
  idP,
  discard,
  (>+>), (<+<),
  runPipe
  ) where

import Control.Category
import Control.Exception (SomeException, Exception)
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Free
import Data.Typeable
import Data.Void
import Prelude hiding (id, (.), catch)

data BrokenDownstreamPipe = BrokenDownstreamPipe
  deriving (Show, Typeable)

brokenDownstreamPipe :: SomeException
brokenDownstreamPipe = E.toException BrokenDownstreamPipe

instance Exception BrokenDownstreamPipe

data BrokenUpstreamPipe = BrokenUpstreamPipe
  deriving (Show, Typeable)

instance Exception BrokenUpstreamPipe

brokenUpstreamPipe :: SomeException
brokenUpstreamPipe = E.toException BrokenUpstreamPipe

data MaskState = Masked | Unmasked

data PipeF a b m x
  = M (m x) MaskState
  | Await (a -> x)
  | Yield b x
  | Catch (PipeF a b m x) (SomeException -> m x)
  | Throw SomeException

instance Monad m => Functor (PipeF a b m) where
  fmap f (M m s) = M (liftM f m) s
  fmap f (Await k) = Await (f . k)
  fmap f (Yield b c) = Yield b (f c)
  fmap f (Catch e h) = Catch (fmap f e) (liftM f . h)
  fmap _ (Throw e) = Throw e

type Pipe a b m r = Free (PipeF a b m) r

catch :: (Monad m, Exception e)
      => Pipe a b m r
      -> (e -> m (Pipe a b m r))
      -> Pipe a b m r
catch p h = catchP p $ \e -> case E.fromException e of
  Nothing -> return $ throw e
  Just e' -> h e'

catchM :: (Monad m, Exception e)
       => Pipe a b m r
       -> (e -> m r)
       -> Pipe a b m r
catchM p h = catch p (liftM return . h)

catch_ :: (Monad m, Exception e)
       => Pipe a b m r
       -> (e -> Pipe a b m r)
       -> Pipe a b m r
catch_ p h = catch p (return . h)

throw :: (Monad m, Exception e) => e -> Pipe a b m r
throw e = liftF . Throw . E.toException $ e

onException :: Monad m
            => Pipe a b m r
            -> m (Pipe a b m s)
            -> Pipe a b m r
onException p w = catchP p h
  where
    h e = w >>= \p' -> return $ p' >> throw e

finally :: Monad m
        => Pipe a b m r
        -> m s
        -> Pipe a b m r
finally p w = do
  r <- onException p (liftM return w)
  lift_ Masked w
  return r

bracket :: Monad m
        => m r
        -> (r -> m y)
        -> (r -> Pipe a b m x)
        -> Pipe a b m x
bracket open close run = do
  r <- lift_ Masked open
  x <- onException (run r) (liftM return (close r))
  lift_ Masked $ close r
  return x

catchP :: Monad m
       => Pipe a b m r
       -> (SomeException -> m (Pipe a b m r))
       -> Pipe a b m r
catchP p h = go p
  where
    go (Pure r) = return r
    go (Free c) = Free $ Catch (fmap go c) h

await :: Monad m => Pipe a b m a
await = liftF $ Await id

tryAwait :: Monad m => Pipe a b m (Maybe a)
tryAwait = catch_ (liftM Just await) $ \(_ :: BrokenUpstreamPipe) -> return Nothing

yield :: Monad m => b -> Pipe a b m ()
yield x = liftF $ Yield x ()

lift_ :: Monad m => MaskState -> m r -> Pipe a b m r
lift_ s m = Free $ M (liftM Pure m) s

lift :: Monad m => m r -> Pipe a b m r
lift m = Free $ Catch (M (liftM Pure m) Unmasked) (return . throw)

pipe :: Monad m => (a -> b) -> Pipe a b m r
pipe f = forever $ await >>= yield . f

idP :: Monad m => Pipe a a m r
idP = pipe id

discard :: Monad m => Pipe a b m r
discard = forever await

compose :: Monad m
   => PipeF a b m x
   -> PipeF b c m y
   -> Free (PipeF a c m)
        (Free (PipeF a b m) x,
         Free (PipeF b c m) y)

-- second pipe running
compose p1 (Yield c y) = yield c >> return (liftF p1, return y)
compose p1 (M m s) = lift_ s m >>= \y -> return (liftF p1, return y)
compose p1 (Catch p2 h) = catchP (compose p1 p2) (h >=> k)
  where k y = return (return (liftF p1, return y))
compose (Catch _ h) p2@(Throw e) = lift_ Masked (h e) >>= k
  where k x = return (return x, liftF p2)
compose _ (Throw e) = throw e

-- first pipe running
compose (Yield b x) (Await k) = return (return x, return (k b))
compose (M m s) p2 = lift_ s m >>= \x -> return (return x, liftF p2)
compose (Catch p1 h) p2 = catchP (compose p1 p2) (h >=> k)
  where k x = return (return (return x, liftF p2))
compose (Throw e) _ = throw e

-- both pipes awaiting
compose (Await k) p2 = await >>= \a -> return (return (k a), liftF p2)

finalizeR :: Monad m
  => x
  -> PipeF b c m x
  -> Pipe a c m x

-- first pipe terminated
finalizeR r (Await _) = return r
finalizeR _ (Yield c x) = yield c >> return x
finalizeR _ (M m s) = lift_ s m
finalizeR _ (Catch (Await _) h) = lift_ Masked (h brokenUpstreamPipe)
finalizeR r (Catch p2 h) = catchP (finalizeR r p2) (h >=> return . Pure)
finalizeR _ (Throw e) = throw e

finalizeL :: Monad m
  => PipeF a b m x
  -> x
  -> Pipe a c m x

-- second pipe terminated
finalizeL (Catch p1 h) r = finalizeL p1 r >> lift_ Masked (h brokenDownstreamPipe)
finalizeL _ r = return r

infixl 9 >+>
(>+>) :: Monad m => Pipe a b m r -> Pipe b c m r -> Pipe a c m r
Free c1 >+> Free c2 = compose c1 c2 >>= \(p1', p2') -> join p1' >+> join p2'
p1@(Pure r) >+> (Free c) = finalizeR (Pure r) c >>= \p2 -> p1 >+> p2
Free c >+> p2@(Pure r) = finalizeL c (Pure r) >>= \p1 -> p1 >+> p2
_ >+> Pure r = return r

infixr 9 <+<
(<+<) :: Monad m => Pipe b c m r -> Pipe a b m r -> Pipe a c m r
p2 <+< p1 = p1 >+> p2

runPipe :: Pipe () Void IO r -> IO r
runPipe p = E.mask $ \restore -> go p restore
  where
    go (Pure r) _ = return r
    go (Free c) restore = step c restore >>= \x -> case x of
      Left e -> E.throwIO e
      Right p' -> go p' restore

    step (M m Unmasked) restore = E.try (restore m)
    step (M m Masked) _ = E.try m
    step (Await k) _ = return . Right . k $ ()
    step (Yield x _) _ = absurd x
    step (Catch c h) restore = step c restore >>= \x -> case x of
      Left e -> liftM Right $ h e
      Right p' -> return (Right p')
    step (Throw e) _ = return $ Left e
