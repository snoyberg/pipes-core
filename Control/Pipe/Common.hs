{-# LANGUAGE DeriveDataTypeable, Rank2Types, ScopedTypeVariables, FlexibleContexts #-}
module Control.Pipe.Common (
  BrokenDownstreamPipe,
  BrokenUpstreamPipe,
  PipeF(..),
  Pipe(..),
  Producer,
  Consumer,
  Pipeline,
  throw,
  catchP,
  catch,
  catchM,
  onException,
  finally,
  bracket,
  await,
  yield,
  lift,
  masked,
  ensure,
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

import Control.Applicative
import Control.Category
import Control.Exception (SomeException, Exception)
import qualified Control.Exception.Lifted as E
import Control.Monad
import Control.Monad.Trans (MonadTrans, lift)
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

data MaskState = Masked | Unmasked | Ensure

data PipeF a b m x
  = M (m x) MaskState
  | Await (a -> x)
  | Yield b x

instance Monad m => Functor (PipeF a b m) where
  fmap f (M m s) = M (liftM f m) s
  fmap f (Await k) = Await (f . k)
  fmap f (Yield b c) = Yield b (f c)

data Pipe a b m r
  = Pure r
  | Free (PipeF a b m (Pipe a b m r))
         (SomeException -> Pipe a b m r)
  | Throw SomeException

type Producer b m = Pipe () b m
type Consumer a m = Pipe a Void m
type Pipeline m = Pipe () Void m

instance Monad m => Monad (Pipe a b m) where
  return = Pure
  Pure r >>= f = f r
  Free c h >>= f = Free (fmap (>>= f) c)
                        (h >=> f)
  Throw e >>= _ = Throw e

instance Monad m => Functor (Pipe a b m) where
  fmap = liftM

instance Monad m => Applicative (Pipe a b m) where
  pure = return
  (<*>) = ap

liftF :: Monad m => PipeF a b m r -> Pipe a b m r
liftF c = Free (fmap return c) throw

catch :: (Monad m, Exception e)
      => Pipe a b m r
      -> (e -> Pipe a b m r)
      -> Pipe a b m r
catch p h = catchP p $ \e -> case E.fromException e of
  Nothing -> throw e
  Just e' -> h e'

catchM :: (Monad m, Exception e)
       => Pipe a b m r
       -> (e -> m r)
       -> Pipe a b m r
catchM p h = catch p $ \e -> ensure (h e)

throw :: (Monad m, Exception e) => e -> Pipe a b m r
throw e = Throw . E.toException $ e

onException :: Monad m
            => Pipe a b m r
            -> Pipe a b m s
            -> Pipe a b m r
onException p w = catchP p $ \e -> w >> throw e

finally :: Monad m
        => Pipe a b m r
        -> m s
        -> Pipe a b m r
finally p w = do
  r <- onException p (ensure w)
  ensure w
  return r

bracket :: Monad m
        => m r
        -> (r -> m y)
        -> (r -> Pipe a b m x)
        -> Pipe a b m x
bracket open close run = do
  r <- lift_ Masked open
  x <- onException (run r) (ensure (close r))
  ensure $ close r
  return x

catchP :: Monad m
       => Pipe a b m r
       -> (SomeException -> Pipe a b m r)
       -> Pipe a b m r
catchP (Pure r) _ = return r
catchP (Free c h1) h2 = Free c $ \e -> catchP (h1 e) h2
catchP (Throw e) h = h e

await :: Monad m => Pipe a b m a
await = liftF $ Await id

tryAwait :: Monad m => Pipe a b m (Maybe a)
tryAwait = catch (liftM Just await) $ \(_ :: BrokenUpstreamPipe) -> return Nothing

yield :: Monad m => b -> Pipe a b m ()
yield x = liftF $ Yield x ()

lift_ :: Monad m => MaskState -> m r -> Pipe a b m r
lift_ s m = liftF (M m s)

instance MonadTrans (Pipe a b) where
  lift = lift_ Unmasked

masked :: Monad m => m r -> Pipe a b m r
masked = lift_ Masked

ensure :: Monad m => m r -> Pipe a b m r
ensure = lift_ Ensure

pipe :: Monad m => (a -> b) -> Pipe a b m r
pipe f = forever $ await >>= yield . f

idP :: Monad m => Pipe a a m r
idP = pipe id

discard :: Monad m => Pipe a b m r
discard = forever await

data Composition a b c m x y
  = AdvanceFirst (Pipe a c m x)
  | AdvanceSecond (Pipe a c m y)
  | AdvanceBoth x y

compose :: Monad m
   => PipeF a b m x
   -> PipeF b c m y
   -> Composition a b c m x y
compose (Yield b x) (Await k) = AdvanceBoth x (k b)
compose (M m Ensure) _ = AdvanceFirst (lift_ Ensure m)
compose _ (Yield c y) = AdvanceSecond (yield c >> return y)
compose _ (M m s) = AdvanceSecond (lift_ s m)
compose (M m s) _ = AdvanceFirst (lift_ s m)
compose (Await k) _ = AdvanceFirst (liftM k await)

finalize2 :: Monad m
          => PipeF b c m r
          -> Maybe (Pipe a c m r)
finalize2 (Await _) = Nothing
finalize2 (M m s) = Just $ lift_ s m
finalize2 (Yield c r) = Just $ yield c >> return r

finalize1 :: Monad m
          => PipeF a b m r
          -> Maybe (Pipe a c m r)
finalize1 (M m Ensure) = Just $ ensure m
finalize1 _ = Nothing

infixl 9 >+>
(>+>) :: Monad m => Pipe a b m r -> Pipe b c m r -> Pipe a c m r
p1 >+> p2 = case (p1, p2) of
  (Free c1 h1, Free c2 h2) -> case compose c1 c2 of
    AdvanceFirst comp -> catchP comp (return . h1) >>= \p1' -> p1' >+> p2
    AdvanceSecond comp -> catchP comp (return . h2) >>= \p2' -> p1 >+> p2'
    AdvanceBoth p1' p2' -> p1' >+> p2'
  (Throw e, Free c h) -> case finalize2 c of
    Nothing   -> p1 >+> h e
    Just comp -> comp >>= \p2' -> p1 >+> p2'
  (Pure r, Free c h) -> case finalize2 c of
    Nothing   -> p1 >+> h (E.toException BrokenUpstreamPipe)
    Just comp -> comp >>= \p2' -> p1 >+> p2'
  (Free c h, Throw e) -> case finalize1 c of
    Nothing   -> h e >+> p2
    Just comp -> comp >>= \p1' -> p1' >+> p2
  (Free c h, Pure r) -> case finalize1 c of
    Nothing   -> h (E.toException BrokenDownstreamPipe) >+> p2
    Just comp -> comp >>= \p1' -> p1' >+> p2
  (Pure r, Throw e) -> case (E.fromException e :: Maybe BrokenUpstreamPipe) of
    Nothing -> throw e
    Just _  -> return r
  (_, Throw e) -> throw e
  (_, Pure r) -> return r

infixr 9 <+<
(<+<) :: Monad m => Pipe b c m r -> Pipe a b m r -> Pipe a c m r
p2 <+< p1 = p1 >+> p2

runPipe :: MonadBaseControl IO m => Pipeline m r -> m r
runPipe p = E.mask $ \restore -> run restore p
  where
    run restore = go
      where
        go (Pure r) = return r
        go (Free c h) = stepPipe try c >>= \x -> case x of
          Left e   -> go $ h e
          Right p' -> go p'
        go (Throw e) = E.throwIO e

        try m s = E.try $ case s of
          Unmasked -> restore m
          _ -> m

runPurePipe :: Monad m => Pipeline m r -> m (Either SomeException r)
runPurePipe (Pure r) = return $ Right r
runPurePipe (Throw e) = return $ Left e
runPurePipe (Free c h) = stepPipe try c >>= runPurePipe . either h id
  where try m _ = liftM Right m

runPurePipe_ :: Monad m => Pipeline m r -> m r
runPurePipe_ = runPurePipe >=> either E.throw return

stepPipe :: Monad m
         => (m r -> MaskState -> m (Either SomeException r))
         -> PipeF () Void m r
         -> m (Either SomeException r)
stepPipe _ (Await k) = return . Right $ k ()
stepPipe _ (Yield x _) = absurd x
stepPipe try (M m s) = try m s
