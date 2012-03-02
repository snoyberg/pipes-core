module Control.Pipe.Monoidal (
  -- | The combinators in this module allow you to create and manipulate
  -- multi-channel pipes. Multiple input or output channels are represented with
  -- 'Either' types.
  --
  -- Most of the combinators are generalizations of the corresponding functions
  -- in 'Control.Arrow', and obey appropriately generalized laws.
  firstP,
  secondP,
  (***),
  associateP,
  disassociateP,
  discardL,
  discardR,
  swapP,
  joinP,
  splitP,
  loopP,
  ) where

import Control.Category.Associative
import Control.Category.Braided
import Control.Category.Monoidal
import Control.Monad
import qualified Control.Monad.Trans as T
import Control.Monad.State
import Control.Pipe.Common

-- | Create a 'Pipe' that behaves like the given 'Pipe' of the left component
-- of the input, and lets values in the right component pass through.
firstP :: Monad m
       => Pipe a b m r
       -> Pipe (Either a c) (Either b c) m r
firstP (Pure r) = return r
firstP (Throw e) = Throw e
firstP (Free c h) = catchP (step c) (return . h) >>= firstP
  where
    step (M m s) = liftP s m
    step (Yield b x) = yield (Left b) >> return x
    step (Await k) = go
      where
        go = await >>= either (return . k)
                              (yield . Right >=> const go)

-- | This function is the equivalent of 'firstP' for the right component.
secondP :: Monad m
        => Pipe a b m r
        -> Pipe (Either c a) (Either c b) m r
secondP (Pure r) = return r
secondP (Throw e) = Throw e
secondP (Free c h) = catchP (step c) (return . h) >>= secondP
  where
    step (M m s) = liftP s m
    step (Yield b x) = yield (Right b) >> return x
    step (Await k) = go
      where
        go = await >>= either (yield . Left >=> const go)
                              (return . k)

-- | Combine two pipes into a single pipe that behaves like the first on the
-- left component, and the second on the right component.
(***) :: Monad m
      => Pipe a b m r
      -> Pipe a' b' m r
      -> Pipe (Either a a') (Either b b') m r
p1 *** p2 = firstP p1 >+> secondP p2

-- | Convert between the two possible associations of a triple sum.
associateP :: Monad m
           => Pipe (Either (Either a b) c) (Either a (Either b c)) m r
associateP = pipe associate

-- | Inverse of 'associateP'.
disassociateP :: Monad m
              => Pipe (Either a (Either b c)) (Either (Either a b) c) m r
disassociateP = pipe disassociate

-- | Discard all values on the left component.
discardL :: Monad m => Pipe (Either x a) a m r
discardL = firstP discard >+> pipe idl

-- | Discard all values on the right component.
discardR :: Monad m => Pipe (Either a x) a m r
discardR = secondP discard >+> pipe idr

-- | Swap the left and right components.
swapP :: Monad m => Pipe (Either a b) (Either b a) m r
swapP = pipe swap

-- | Yield all input values into both the left and right components of the
-- output.
splitP :: Monad m => Pipe a (Either a a) m r
splitP = forever $ await >>= yield2
  where
    yield2 x = yield (Left x) >> yield (Right x)

-- | Yield both components of input values into the output.
joinP :: Monad m => Pipe (Either a a) a m r
joinP = pipe $ either id id

data Queue a = Queue ![a] ![a]

emptyQueue :: Queue a
emptyQueue = Queue [] []

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue xs ys) = Queue (x : xs) ys

dequeue :: Queue a -> (Queue a, Maybe a)
dequeue (Queue (x : xs) ys) = (Queue xs ys, Just x)
dequeue q@(Queue [] []) = (q, Nothing)
dequeue (Queue [] ys) = dequeue (Queue (reverse ys) [])

-- The 'loopP' combinator allows to create 'Pipe's whose output value is fed
-- back to the pipe as input.
loopP :: Monad m => Pipe (Either a c) (Either b c) m r -> Pipe a b m r
loopP = go emptyQueue
  where
    go _ (Pure r) = return r
    go _ (Throw e) = throwP e
    go q (Free c h) = case step q c of
      (q', p') -> catchP p' (return . h) >>= go q'

    step q (Await k) = case dequeue q of
      (q', x) -> (q', maybe (liftM (k . Left) await) (return . k . Right) x)
    step q (Yield (Right x) c) = (enqueue x q, return c)
    step q (Yield (Left x) c) = (q, yield x >> return c)
    step q (M m s) = (q, liftP s m)
