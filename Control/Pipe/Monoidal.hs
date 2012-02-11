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

instance Monad m => IFunctor (Lazy m r) where
  arr = Lazy . pipe

instance Monad m => PFunctor Either (Lazy m r) (Lazy m r) where
  first = Lazy . firstP . unLazy where

firstP (Pure r) = return r
firstP (M m) = lift m >>= firstP
firstP (Await k) = go
  where
    go = tryAwait >>= maybe
                        (firstP $ k Nothing)
                        (either (firstP . k . Just)
                                (yield . Right >=> const go))
firstP (Yield x c) = yield (Left x) >> firstP c

instance Monad m => QFunctor Either (Lazy m r) (Lazy m r) where
  second = Lazy . secondP . unLazy where

secondP (Pure r) = return r
secondP (M m) = lift m >>= secondP
secondP (Await k) = go
  where
    go = tryAwait >>= maybe
                        (secondP $ k Nothing)
                        (either (yield . Left >=> const go)
                                (secondP . k . Just))
secondP (Yield x c) = yield (Right x) >> secondP c

instance Monad m => Bifunctor Either (Lazy m r) (Lazy m r) (Lazy m r) where
  bimap f g = first f >>> second g

(***) :: Monad m
      => Pipe a b m r
      -> Pipe a' b' m r
      -> Pipe (Either a a') (Either b b') m r
p1 *** p2 = unLazy $ bimap (Lazy p1) (Lazy p2)

instance Monad m => Associative (Lazy m r) Either where
  associate = arr associate

associateP :: Monad m
           => Pipe (Either (Either a b) c) (Either a (Either b c)) m r
associateP = unLazy associate

instance Monad m => Disassociative (Lazy m r) Either where
  disassociate = arr disassociate

disassociateP :: Monad m
              => Pipe (Either a (Either b c)) (Either (Either a b) c) m r
disassociateP = unLazy disassociate

type instance Id (Lazy m r) Either = Void

instance Monad m => Monoidal (Lazy m r) Either where
  idl = arr idl
  idr = arr idr

instance Monad m => Comonoidal (Lazy m r) Either where
  coidl = arr coidl
  coidr = arr coidr

discardL :: Monad m => Pipe (Either x a) a m r
discardL = firstP discard >+> unLazy idl

discardR :: Monad m => Pipe (Either a x) a m r
discardR = secondP discard >+> unLazy idr

instance Monad m => Braided (Lazy m r) Either where
  braid = arr braid

instance Monad m => Symmetric (Lazy m r) Either where

swapP :: Monad m => Pipe (Either a b) (Either b a) m r
swapP = unLazy swap

instance Monad m => Comultiplicative (Lazy m r) Either where
  counit = Lazy discard
  comult = Lazy . forever $ await >>= yield2
    where
      yield2 x = yield (Left x) >> yield (Right x)

joinP :: Monad m => Pipe a (Either a a) m r
joinP = unLazy comult

instance Monad m => Multiplicative (Lazy m r) Either where
  unit = arr absurd
  mult = arr $ either id id

splitP :: Monad m => Pipe (Either a a) a m r
splitP = unLazy mult

loopP :: Monad m => Pipe (Either a c) (Either b c) m r -> Pipe a b m r
loopP (Pure r) = return r
loopP (M m) = lift m >>= loopP
loopP (Await k) = tryAwait >>= \x -> loopP (k $ liftM Left x)
loopP (Yield x c) = case x of
  Left x -> yield x >> loopP c
  Right z -> loopP (feed (Right z) c)
  where
    feed x p = (yield x >> idP) >+> p
