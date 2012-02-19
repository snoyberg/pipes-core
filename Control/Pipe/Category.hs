{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

module Control.Pipe.Category (
  PipeC(..),
  IFunctor(..),
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

import Control.Categorical.Bifunctor
import Control.Category
import Control.Category.Associative
import Control.Category.Braided
import Control.Category.Monoidal
import Control.Category.Multiplicative
import Control.Monad
import Control.Monad.Free
import Control.Pipe.Common
import Data.Void
import Prelude hiding ((.), id)

-- category instance
newtype PipeC m r a b = PipeC { unPipeC :: Pipe a b m r }

instance Monad m => Category (PipeC m r) where
  id = PipeC idP
  PipeC p2 . PipeC p1 = PipeC (p2 <+< p1)

-- | Identity-on-objects functor.
--
-- This is part of the interface of Arrow.
class Category k => IFunctor k where
  arr :: (a -> b) -> k a b

instance Monad m => IFunctor (PipeC m r) where
  arr = PipeC . pipe

instance Monad m => PFunctor Either (PipeC m r) (PipeC m r) where
  first = PipeC . firstP . unPipeC where

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

instance Monad m => QFunctor Either (PipeC m r) (PipeC m r) where
  second = PipeC . secondP . unPipeC where

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

instance Monad m => Bifunctor Either (PipeC m r) (PipeC m r) (PipeC m r) where
  bimap f g = first f >>> second g

(***) :: Monad m
      => Pipe a b m r
      -> Pipe a' b' m r
      -> Pipe (Either a a') (Either b b') m r
p1 *** p2 = unPipeC $ bimap (PipeC p1) (PipeC p2)

instance Monad m => Associative (PipeC m r) Either where
  associate = arr associate

associateP :: Monad m
           => Pipe (Either (Either a b) c) (Either a (Either b c)) m r
associateP = unPipeC associate

instance Monad m => Disassociative (PipeC m r) Either where
  disassociate = arr disassociate

disassociateP :: Monad m
              => Pipe (Either a (Either b c)) (Either (Either a b) c) m r
disassociateP = unPipeC disassociate

type instance Id (PipeC m r) Either = Void

instance Monad m => Monoidal (PipeC m r) Either where
  idl = arr idl
  idr = arr idr

instance Monad m => Comonoidal (PipeC m r) Either where
  coidl = arr coidl
  coidr = arr coidr

discardL :: Monad m => Pipe (Either x a) a m r
discardL = firstP discard >+> unPipeC idl

discardR :: Monad m => Pipe (Either a x) a m r
discardR = secondP discard >+> unPipeC idr

instance Monad m => Braided (PipeC m r) Either where
  braid = arr braid

instance Monad m => Symmetric (PipeC m r) Either where

swapP :: Monad m => Pipe (Either a b) (Either b a) m r
swapP = unPipeC swap

instance Monad m => Comultiplicative (PipeC m r) Either where
  counit = PipeC discard
  comult = PipeC . forever $ await >>= yield2
    where
      yield2 x = yield (Left x) >> yield (Right x)

splitP :: Monad m => Pipe a (Either a a) m r
splitP = unPipeC comult

instance Monad m => Multiplicative (PipeC m r) Either where
  unit = arr absurd
  mult = arr $ either id id

joinP :: Monad m => Pipe (Either a a) a m r
joinP = unPipeC mult

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
