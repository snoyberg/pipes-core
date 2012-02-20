module Control.Pipe.Monoidal (
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
import Control.Monad.Free
import Control.Pipe.Common

firstP :: Monad m
       => Pipe a b m r
       -> Pipe (Either a c) (Either b c) m r
firstP (Pure r) = return r
firstP (Free c) = step c >>= firstP
  where
    step (M m s) = lift_ s m
    step (Yield b x) = yield (Left b) >> return x
    step (Throw e) = throw e
    step (Catch s h) = catchP (step s) (liftM return . h)
    step (Await k) = go
      where
        go = await >>= either (return . k)
                              (yield . Right >=> const go)

secondP :: Monad m
        => Pipe a b m r
        -> Pipe (Either c a) (Either c b) m r
secondP (Pure r) = return r
secondP (Free c) = step c >>= secondP
  where
    step (M m s) = lift_ s m
    step (Yield b x) = yield (Right b) >> return x
    step (Throw e) = throw e
    step (Catch s h) = catchP (step s) (liftM return . h)
    step (Await k) = go
      where
        go = await >>= either (yield . Left >=> const go)
                              (return . k)

(***) :: Monad m
      => Pipe a b m r
      -> Pipe a' b' m r
      -> Pipe (Either a a') (Either b b') m r
p1 *** p2 = firstP p1 >+> secondP p2

associateP :: Monad m
           => Pipe (Either (Either a b) c) (Either a (Either b c)) m r
associateP = pipe associate

disassociateP :: Monad m
              => Pipe (Either a (Either b c)) (Either (Either a b) c) m r
disassociateP = pipe disassociate

discardL :: Monad m => Pipe (Either x a) a m r
discardL = firstP discard >+> pipe idl

discardR :: Monad m => Pipe (Either a x) a m r
discardR = secondP discard >+> pipe idr

swapP :: Monad m => Pipe (Either a b) (Either b a) m r
swapP = pipe swap

splitP :: Monad m => Pipe a (Either a a) m r
splitP = forever $ await >>= yield2
  where
    yield2 x = yield (Left x) >> yield (Right x)

joinP :: Monad m => Pipe (Either a a) a m r
joinP = pipe $ either id id

loopP :: Monad m => Pipe (Either a c) (Either b c) m r -> Pipe a b m r
loopP (Pure r) = return r
loopP (Free c) = case step c of
  (z, m) -> m >>= \p -> loopP (feeder z >+> p)
  where
    feeder z = maybe (return ()) (yield . Right) z >> idP

    step (M m s) = (Nothing, lift_ s m)
    step (Await k) = (Nothing, liftM (k . Left) await)
    step (Yield (Left b) x) = (Nothing, yield b >> return x)
    step (Yield (Right z) x) = (Just z, return x)
    step (Catch s h) = (z, catchP s' (liftM return . h))
      where (z, s') = step s
    step (Throw e) = (Nothing, throw e)
