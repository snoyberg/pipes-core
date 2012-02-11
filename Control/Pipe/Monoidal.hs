{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

module Control.Pipe.Monoidal (
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
import Control.Categorical.Object
import Control.Category
import Control.Category.Associative
import Control.Category.Braided
import Control.Category.Monoidal
import Control.Category.Multiplicative
import Control.Monad
import Control.Monad.Trans
import Control.Pipe.Common
import Data.Void
import Prelude hiding ((.), id)

-- | Identity-on-objects functor.
--
-- This is part of the interface of Arrow.
class Category k => IFunctor k where
  arr :: (a -> b) -> k a b

instance Monad m => IFunctor (PipeC m r) where
  arr = PipeC . pipe

instance Monad m => PFunctor Either (PipeC m r) (PipeC m r) where
  first = PipeC . firstP . unPipeC where

firstP (Pure r) = return r
firstP (M m) = lift m >>= firstP
firstP (Await k) = go
  where
    go = tryAwait >>= maybe
                        (firstP $ k Nothing)
                        (either (firstP . k . Just)
                                (yield . Right >=> const go))
firstP (Yield x c) = yield (Left x) >> firstP c

instance Monad m => QFunctor Either (PipeC m r) (PipeC m r) where
  second = PipeC . secondP . unPipeC where

secondP (Pure r) = return r
secondP (M m) = lift m >>= secondP
secondP (Await k) = go
  where
    go = tryAwait >>= maybe
                        (secondP $ k Nothing)
                        (either (yield . Left >=> const go)
                                (secondP . k . Just))
secondP (Yield x c) = yield (Right x) >> secondP c

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

joinP :: Monad m => Pipe a (Either a a) m r
joinP = unPipeC comult

instance Monad m => Multiplicative (PipeC m r) Either where
  unit = arr absurd
  mult = arr $ either id id

splitP :: Monad m => Pipe (Either a a) a m r
splitP = unPipeC mult

loopP :: Monad m => Pipe (Either a c) (Either b c) m r -> Pipe a b m r
loopP (Pure r) = return r
loopP (M m) = lift m >>= loopP
loopP (Await k) = tryAwait >>= \x -> loopP (k $ liftM Left x)
loopP (Yield x c) = case x of
  Left x -> yield x >> loopP c
  Right z -> loopP (feed (Right z) c)
  where
    feed x p = (yield x >> idP) >+> p
