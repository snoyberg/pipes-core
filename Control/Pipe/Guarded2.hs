{-# LANGUAGE DeriveDataTypeable, Rank2Types #-}
module Control.Pipe.Guarded2 where

import Control.Exception (SomeException, Exception)
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Free
import Data.Typeable
import Data.Void

data BrokenPipe = BrokenPipe
  deriving (Show, Typeable)

brokenPipe :: SomeException
brokenPipe = E.toException BrokenPipe

instance Exception BrokenPipe

data BrokenUpstreamPipe = BrokenUpstreamPipe
  deriving (Show, Typeable)

instance Exception BrokenUpstreamPipe

brokenUpstreamPipe :: SomeException
brokenUpstreamPipe = E.toException BrokenUpstreamPipe

data PipeF a b m x
  = M (m x)
  | Await (a -> x)
  | Yield b x
  | Catch (PipeF a b m x) (SomeException -> m x)
  | Throw SomeException

instance Monad m => Functor (PipeF a b m) where
  fmap f (M m) = M $ liftM f m
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

throw :: (Monad m, Exception e) => e -> Pipe a b m r
throw e = liftF . Throw . E.toException $ e

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

yield :: Monad m => b -> Pipe a b m ()
yield x = liftF $ Yield x ()

lift_ :: Monad m => m r -> Pipe a b m r
lift_ m = Free $ M (liftM Pure m)

lift :: Monad m => m r -> Pipe a b m r
lift m = Free $ Catch (M (liftM Pure m)) (return . throw)

pipe :: Monad m => (a -> b) -> Pipe a b m r
pipe f = forever $ await >>= yield . f

compose :: Monad m
   => PipeF a b m x
   -> PipeF b c m y
   -> Free (PipeF a c m)
        (Free (PipeF a b m) x,
         Free (PipeF b c m) y)

-- second pipe running
compose p1 (Yield c y) = yield c >> return (liftF p1, return y)
compose p1 (M m) = lift_ m >>= \y -> return (liftF p1, return y)
compose p1 (Catch p2 h) = catchP (compose p1 p2) (h >=> k)
  where k y = return (return (liftF p1, return y))
compose (Catch _ h) p2@(Throw e) = lift_ (h e) >>= k
  where k x = return (return x, liftF p2)
compose _ (Throw e) = throw e

-- first pipe running
compose (Yield b x) (Await k) = return (return x, return (k b))
compose (M m) p2 = lift_ m >>= \x -> return (return x, liftF p2)
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
finalizeR _ (M m) = lift_ m
finalizeR _ (Catch (Await _) h) = lift_ (h brokenUpstreamPipe)
finalizeR r (Catch p2 h) = catchP (finalizeR r p2) (h >=> return . Pure)
finalizeR _ (Throw e) = throw e

finalizeL :: Monad m
  => PipeF a b m x
  -> x
  -> Pipe a c m x

-- second pipe terminated
finalizeL (Catch p1 h) r = finalizeL p1 r >> lift_ (h brokenPipe)
finalizeL _ r = return r

(>+>) :: Monad m => Pipe a b m r -> Pipe b c m r -> Pipe a c m r
Free c1 >+> Free c2 = compose c1 c2 >>= \(p1', p2') -> join p1' >+> join p2'
p1@(Pure r) >+> (Free c) = finalizeR (Pure r) c >>= \p2 -> p1 >+> p2
Free c >+> p2@(Pure r) = finalizeL c (Pure r) >>= \p1 -> p1 >+> p2
_ >+> Pure r = return r

type SafeWrapper m = forall r . m r -> m (Either SomeException r)

runPipeW :: Monad m => SafeWrapper m -> Pipe () Void m r -> m (Either SomeException r)
runPipeW _ (Pure r) = return (Right r)
runPipeW safe (Free c) = step c >>= \x -> case x of
  Left e -> return (Left e)
  Right p -> runPipeW safe p
  where
    step (M m) = safe m
    step (Await k) = return . Right . k $ ()
    step (Yield x _) = absurd x
    step (Catch p h) = step p >>= \x -> case x of
      Left e -> liftM Right $ h e
      Right x' -> return (Right x')
    step (Throw e) = return $ Left e

runPipe :: Pipe () Void IO r -> IO r
runPipe p = runPipeW E.try p >>= \x -> case x of
  Left e -> E.throwIO e
  Right r -> return r
