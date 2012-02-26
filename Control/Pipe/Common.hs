{-# LANGUAGE DeriveDataTypeable, Rank2Types, ScopedTypeVariables, FlexibleContexts #-}
module Control.Pipe.Common (
  BrokenDownstreamPipe,
  BrokenUpstreamPipe,
  PipeF(..),
  Pipe,
  Producer,
  Consumer,
  Pipeline,
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
  runPipe,
  runPurePipe,
  runPurePipe_,
  Void,
  ) where

import Control.Category
import Control.Exception (SomeException, Exception)
import qualified Control.Exception.Lifted as E
import Control.Monad
import Control.Monad.Free
import Control.Monad.Trans.Control
import Data.Typeable
import Data.Void
import Prelude hiding (id, (.), catch)

data BrokenDownstreamPipe = BrokenDownstreamPipe
  deriving (Show, Typeable)

instance Exception BrokenDownstreamPipe

data BrokenUpstreamPipe = BrokenUpstreamPipe
  deriving (Show, Typeable)

instance Exception BrokenUpstreamPipe

data MaskState = Masked | Unmasked | Strict

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

type Pipe a b m = Free (PipeF a b m)
type Producer b m = Pipe () b m
type Consumer a m = Pipe a Void m
type Pipeline m = Pipe () Void m

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
  lift_ Strict w
  return r

bracket :: Monad m
        => m r
        -> (r -> m y)
        -> (r -> Pipe a b m x)
        -> Pipe a b m x
bracket open close run = do
  r <- lift_ Masked open
  x <- onException (run r) (liftM return (close r))
  lift_ Strict $ close r
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
lift = lift_ Unmasked

pipe :: Monad m => (a -> b) -> Pipe a b m r
pipe f = forever $ await >>= yield . f

idP :: Monad m => Pipe a a m r
idP = pipe id

discard :: Monad m => Pipe a b m r
discard = forever await

data CompositionKind
  = AdvanceFirst
  | AdvanceSecond
  | AdvanceBoth

data Composition a b c m x y = Composition
  CompositionKind
  (Pipe a c m (Pipe a b m x, Pipe b c m y))

cresult :: Composition a b c m x y
        -> Pipe a c m (Pipe a b m x, Pipe b c m y)
cresult (Composition _ r) = r

advanceFirst :: Monad m => Pipe a c m x -> PipeF b c m y -> Composition a b c m x y
advanceFirst p1 p2 = Composition AdvanceFirst $
  liftM (\x -> (return x, liftF p2)) p1

advanceSecond :: Monad m => PipeF a b m x -> Pipe a c m y -> Composition a b c m x y
advanceSecond p1 p2 = Composition AdvanceSecond $
  liftM (\y -> (liftF p1, return y)) p2

advanceBoth :: Monad m => Pipe a c m x -> Pipe a c m y -> Composition a b c m x y
advanceBoth p1 p2 = Composition AdvanceBoth $
  liftM2 (\x y -> (return x, return y)) p1 p2

compose :: Monad m
   => PipeF a b m x
   -> PipeF b c m y
   -> Composition a b c m x y

-- catch
compose p1 p2@(Catch s h) =
  let Composition k result = compose p1 s
      result' = result >>= \(p1', _) -> return (p1', liftF p2)
  in Composition k $ case k of
    AdvanceFirst -> catchP result' $ \e ->
      h e >>= \y -> return (return (throw e, return y))
    AdvanceSecond -> catchP result $ \e ->
      h e >>= \y -> return (return (liftF p1, return y))
    AdvanceBoth -> result
compose p1@(Catch s h) p2 =
  let Composition k result = compose s p2
      result' = result >>= \(_, p2') -> return (liftF p1, p2')
  in Composition k $ case k of
    AdvanceFirst -> catchP result $ \e ->
      h e >>= \x -> return (return (return x, liftF p2))
    AdvanceSecond -> catchP result' $ \e ->
      h e >>= \x -> return (return (return x, throw e))
    AdvanceBoth -> result

-- first pipe running
compose (Yield b x) (Await k) = advanceBoth (return x) (return (k b))
compose (M m s) p2@(Await _) = advanceFirst (lift_ s m) p2
compose (M m Strict) p2 = advanceFirst (lift_ Strict m) p2
compose (Throw e) p2@(Await _) = advanceFirst (throw e) p2
compose (Await k) p2@(Await _) = advanceFirst (liftM k await) p2

-- second pipe running
compose p1 (Yield c y) = advanceSecond p1 (yield c >> return y)
compose p1 (M m s) = advanceSecond p1 (lift_ s m)
compose p1 (Throw e) = advanceSecond p1 (throw e)

finalizeR :: Monad m
  => x
  -> PipeF b c m x
  -> Pipe a c m x

-- first pipe terminated
finalizeR r c = let result = go c in case result of
  Free (Throw e) -> case E.fromException e :: Maybe BrokenUpstreamPipe of
    Nothing -> throw e
    Just _  -> return r
  _ -> result
  where go (Await _) = throw BrokenUpstreamPipe
        go (Yield z x) = yield z >> return x
        go (M m s) = lift_ s m
        go (Catch p2 h) = catchP (go p2) (liftM return . h)
        go (Throw e) = throw e

finalizeL :: Monad m
  => PipeF a b m x
  -> x
  -> Pipe a c m x

-- second pipe terminated
finalizeL c r = let result = go c in case result of
  Free (Throw e) -> case E.fromException e :: Maybe BrokenDownstreamPipe of
    Nothing -> throw e
    Just _  -> return r
  _ -> result
  where
    go (Catch p1 h) = catchP (go p1) (liftM return . h)
    go (M m Strict) = lift_ Masked m
    go _ = throw BrokenDownstreamPipe

infixl 9 >+>
(>+>) :: Monad m => Pipe a b m r -> Pipe b c m r -> Pipe a c m r
Free c1 >+> Free c2 = cresult (compose c1 c2) >>= \(p1', p2') -> join p1' >+> join p2'
p1@(Pure r) >+> (Free c) = finalizeR (Pure r) c >>= \p2 -> p1 >+> p2
Free c >+> p2@(Pure r) = finalizeL c (Pure r) >>= \p1 -> p1 >+> p2
_ >+> Pure r = return r

infixr 9 <+<
(<+<) :: Monad m => Pipe b c m r -> Pipe a b m r -> Pipe a c m r
p2 <+< p1 = p1 >+> p2

stepPipe :: Monad m
         => (m r -> MaskState -> m (Either SomeException r))
         -> PipeF () Void m r
         -> m (Either SomeException r)
stepPipe try (M m s) = try m s
stepPipe try (Await k) = return . Right . k $ ()
stepPipe try (Yield x _) = absurd x
stepPipe try (Catch c h) = stepPipe try c >>= \x -> case x of
  Left e -> liftM Right $ h e
  Right p' -> return (Right p')
stepPipe try (Throw e) = return $ Left e

runPipe :: MonadBaseControl IO m => Pipeline m r -> m r
runPipe p = E.mask $ \restore -> run p restore
  where
    run p restore = go p
      where
        try m s = case s of
          Unmasked -> E.try (restore m)
          _        -> E.try m
        go (Pure r) = return r
        go (Free c) = stepPipe try c >>= either E.throwIO go

runPurePipe :: Monad m => Pipeline m r -> m (Either SomeException r)
runPurePipe (Pure r) = return $ Right r
runPurePipe (Free c) = stepPipe try c >>= either (return . Left) runPurePipe
  where try m _ = liftM Right m

runPurePipe_ :: Monad m => Pipeline m r -> m r
runPurePipe_ p = runPurePipe p >>= either E.throw return
