{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}
module Control.Pipe.Common (
  -- ** Types
  Pipe(..),
  Producer,
  Consumer,
  Pipeline,
  Void,

  -- ** Primitives
  --
  -- | 'await' and 'yield' are the two basic primitives you need to create
  -- 'Pipe's. Because 'Pipe' is a monad, you can assemble them using ordinary
  -- @do@ notation. Since 'Pipe' is also a monad trnasformer, you can use
  -- 'lift' to invoke the base monad. For example:
  --
  -- > check :: Pipe a a IO r
  -- > check = forever $ do
  -- >   x <- await
  -- >   lift $ putStrLn $ "Can " ++ show x ++ " pass?"
  -- >   ok <- lift $ read <$> getLine
  -- >   when ok $ yield x
  await,
  yield,
  masked,
  ensure,

  -- ** Basic combinators
  pipe,
  idP,
  discard,
  (>+>),
  (<+<),

  -- ** Running pipes
  runPipe,
  runPurePipe,
  runPurePipe_,

  -- ** Low level types
  BrokenDownstreamPipe,
  BrokenUpstreamPipe,
  PipeF(..),
  MaskState(..),

  -- ** Low level primitives
  --
  -- | These functions can be used to implement exception-handling combinators.
  -- For normal use, prefer the functions defined in 'Control.Pipe.Exception'.
  throwP,
  catchP,
  liftP,
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

-- | The 'BrokenDownstreamPipe' exception is used to signal termination of the
-- downstream portion of a 'Pipeline' after the current pipe.
--
-- There is usually no need to catch this exception explicitly, a pipe will
-- terminate automatically when the downstream pipe terminates.
data BrokenDownstreamPipe = BrokenDownstreamPipe
  deriving (Show, Typeable)

instance Exception BrokenDownstreamPipe

-- | The 'BrokenUpstreamPipe' exception is used to signal termination of the
-- upstream portion of a 'Pipeline' before the current pipe
--
-- A 'BrokenUpstreamPipe' exception can be caught to perform cleanup actions
-- immediately before termination, like returning a result or yielding
-- additional values.
data BrokenUpstreamPipe = BrokenUpstreamPipe
  deriving (Show, Typeable)

instance Exception BrokenUpstreamPipe

-- | Type of action in the base monad.
data MaskState
  = Masked    -- ^ Action to be run with asynchronous exceptions masked.
  | Unmasked  -- ^ Action to be run with asynchronous exceptions unmasked.
  | Ensure    -- ^ Action to be run with priority with respect to downstream
              --   actions, and asynchronous exceptions masked.

data PipeF a b m x
  = M (m x) MaskState
  | Await (a -> x)
  | Yield b x

instance Monad m => Functor (PipeF a b m) where
  fmap f (M m s) = M (liftM f m) s
  fmap f (Await k) = Await (f . k)
  fmap f (Yield b c) = Yield b (f c)

-- | The base type for pipes.
--
--  [@a@] The type of input received fom upstream pipes.
--
--  [@b@] The type of output delivered to downstream pipes.
--
--  [@c@] The base monad.
--
--  [@d@] The type of the monad's final result.
data Pipe a b m r
  -- Pipe is a free monad over the functor
  --
  -- data PipeF' a b m r
  --   = Catch (PipeF a b m r) (SomeException -> r)
  --   | Throw e
  -- 
  -- but is implemented inline because it makes the code simpler.
  = Pure r
  | Free (PipeF a b m (Pipe a b m r))
         (SomeException -> Pipe a b m r)
  | Throw SomeException

-- | A pipe that can only produce values.
type Producer b m = Pipe () b m

-- | A pipe that can only consume values.
type Consumer a m = Pipe a Void m

-- | A self-contained pipeline that is ready to be run.
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
liftF c = Free (fmap return c) throwP

-- | Throw an exception within the 'Pipe' monad.
throwP :: Monad m => SomeException -> Pipe a b m r
throwP = Throw

-- | Catch an exception within the pipe monad.
catchP :: Monad m
       => Pipe a b m r
       -> (SomeException -> Pipe a b m r)
       -> Pipe a b m r
catchP (Pure r) _ = return r
catchP (Free c h1) h2 = Free
  (fmap (`catchP` h2) c)
  (\e -> catchP (h1 e) h2)
catchP (Throw e) h = h e

-- | Wait for input from upstream within the 'Pipe' monad.
--
-- 'await' blocks until input is ready.
await :: Monad m => Pipe a b m a
await = liftF $ Await id

-- | Pass output downstream within the 'Pipe' monad.
--
-- 'yield' blocks until the downstream pipe calls 'await' again.
yield :: Monad m => b -> Pipe a b m ()
yield x = liftF $ Yield x ()

-- | Execute an action in the base monad with the given 'MaskState'.
liftP :: Monad m => MaskState -> m r -> Pipe a b m r
liftP s m = liftF (M m s)

instance MonadTrans (Pipe a b) where
  lift = liftP Unmasked

-- | Execute an action in the base monad with asynchronous exceptions masked.
--
-- This function is effective only if the 'Pipeline' is run with 'runPipe',
-- otherwise it is identical to 'lift'
masked :: Monad m => m r -> Pipe a b m r
masked = liftP Masked

-- | Execute an action in the base monad with upstream priority.
--
-- Normally, downstream effects are given priority in pipe composition. Using
-- 'ensure' allows to change this behavior, and create an action which is
-- executed /before/ downstream actions and despite downstream termination.
--
-- Actions executed using 'ensure' are implicitly masked when the pipeline is
-- run with 'runPipe'.
ensure :: Monad m => m r -> Pipe a b m r
ensure = liftP Ensure

-- | Convert a pure function into a pipe.
--
-- > pipe = forever $ do
-- >   x <- await
-- >   yield (f x)
pipe :: Monad m => (a -> b) -> Pipe a b m r
pipe f = forever $ await >>= yield . f

-- | The identity pipe.
idP :: Monad m => Pipe a a m r
idP = pipe id

-- | The 'discard' pipe silently discards all input fed to it.
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
compose (M m Ensure) _ = AdvanceFirst (liftP Ensure m)
compose _ (Yield c y) = AdvanceSecond (yield c >> return y)
compose _ (M m s) = AdvanceSecond (liftP s m)
compose (M m s) _ = AdvanceFirst (liftP s m)
compose (Await k) _ = AdvanceFirst (liftM k await)

finalize2 :: Monad m
          => PipeF b c m r
          -> Maybe (Pipe a c m r)
finalize2 (Await _) = Nothing
finalize2 (M m s) = Just $ liftP s m
finalize2 (Yield c r) = Just $ yield c >> return r

finalize1 :: Monad m
          => PipeF a b m r
          -> Maybe (Pipe a c m r)
finalize1 (M m Ensure) = Just $ ensure m
finalize1 _ = Nothing

infixl 9 >+>
-- | Left to right pipe composition.
(>+>) :: Monad m => Pipe a b m r -> Pipe b c m r -> Pipe a c m r
p1 >+> p2 = case (p1, p2) of
  (Free c1 h1, Free c2 h2) -> case compose c1 c2 of
    AdvanceFirst comp -> catchP comp (return . h1) >>= \p1' -> p1' >+> p2
    AdvanceSecond comp -> catchP comp (return . h2) >>= \p2' -> p1 >+> p2'
    AdvanceBoth p1' p2' -> p1' >+> p2'
  (Throw e, Free c h) -> case finalize2 c of
    Nothing   -> p1 >+> h e
    Just comp -> catchP comp (return . h) >>= \p2' -> p1 >+> p2'
  (Pure r, Free c h) -> case finalize2 c of
    Nothing   -> p1 >+> h (E.toException BrokenUpstreamPipe)
    Just comp -> catchP comp (return . h) >>= \p2' -> p1 >+> p2'
  (Free c h, Throw e) -> case finalize1 c of
    Nothing   -> h e >+> p2
    Just comp -> catchP comp (return . h) >>= \p1' -> p1' >+> p2
  (Free c h, Pure r) -> case finalize1 c of
    Nothing   -> h (E.toException BrokenDownstreamPipe) >+> p2
    Just comp -> catchP comp (return . h) >>= \p1' -> p1' >+> p2
  (Pure r, Throw e) -> case (E.fromException e :: Maybe BrokenUpstreamPipe) of
    Nothing -> throwP e
    Just _  -> return r
  (_, Throw e) -> throwP e
  (_, Pure r) -> return r

infixr 9 <+<
-- | Right to left pipe composition.
(<+<) :: Monad m => Pipe b c m r -> Pipe a b m r -> Pipe a c m r
p2 <+< p1 = p1 >+> p2

-- | Run a self-contained 'Pipeline', converting it to an action in the base
-- monad.
--
-- This function is exception-safe. Any exception thrown in the base monad
-- during execution of the pipeline will be captured by
-- 'Control.Pipe.Exception.catch' statements in the 'Pipe' monad.
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

-- | Run a self-contained pipeline over an arbitrary monad, with fewer
-- exception-safety guarantees than 'runPipe'.
--
-- Only pipe termination exceptions and exceptions thrown using
-- 'Control.Pipe.Exception.throw' will be catchable within the 'Pipe' monad.
-- Any other exception will terminate execution immediately and finalizers will
-- not be called.
--
-- Any captured exception will be returned in the left component of the result.
runPurePipe :: Monad m => Pipeline m r -> m (Either SomeException r)
runPurePipe (Pure r) = return $ Right r
runPurePipe (Throw e) = return $ Left e
runPurePipe (Free c h) = stepPipe try c >>= runPurePipe . either h id
  where try m _ = liftM Right m

-- | A version of 'runPurePipe' which rethrows any captured exception instead
-- of returning it.
runPurePipe_ :: Monad m => Pipeline m r -> m r
runPurePipe_ = runPurePipe >=> either E.throw return

stepPipe :: Monad m
         => (m r -> MaskState -> m (Either SomeException r))
         -> PipeF () Void m r
         -> m (Either SomeException r)
stepPipe _ (Await k) = return . Right $ k ()
stepPipe _ (Yield x _) = absurd x
stepPipe try (M m s) = try m s
