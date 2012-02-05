{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

module Control.Pipe.Monoidal (
  IFunctor(..)
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
  first = Lazy . f . unLazy where
    f (Pure r) = return r
    f (M m) = lift m >>= f
    f (Await k) = go where
      go = tryAwait >>= maybe (f $ k Nothing)
                          (either (f . k . Just)
                                  (yield . Right >=> const go))
    f (Yield x c) = yield (Left x) >> f c

instance Monad m => QFunctor Either (Lazy m r) (Lazy m r) where
  second = Lazy . f . unLazy where
    f (Pure r) = return r
    f (M m) = lift m >>= f
    f (Await k) = go where
      go = tryAwait >>= maybe (f $ k Nothing)
                          (either (yield . Left >=> const go)
                                  (f . k . Just))
    f (Yield x c) = yield (Right x) >> f c

instance Monad m => Bifunctor Either (Lazy m r) (Lazy m r) (Lazy m r) where
  bimap f g = first f >>> second g

instance Monad m => Associative (Lazy m r) Either where
  associate = arr associate

instance Monad m => Disassociative (Lazy m r) Either where
  disassociate = arr disassociate

type instance Id (Lazy m r) Either = Void

instance Monad m => Monoidal (Lazy m r) Either where
  idl = arr idl
  idr = arr idr

instance Monad m => Comonoidal (Lazy m r) Either where
  coidl = arr coidl
  coidr = arr coidr

instance Monad m => Braided (Lazy m r) Either where
  braid = arr braid

instance Monad m => Symmetric (Lazy m r) Either where

instance Monad m => Comultiplicative (Lazy m r) Either where
  counit = Lazy discard
  comult = Lazy . forever $ await >>= yield2
    where
      yield2 x = yield (Left x) >> yield (Right x)

instance Monad m => Multiplicative (Lazy m r) Either where
  unit = arr absurd
  mult = arr $ either id id
