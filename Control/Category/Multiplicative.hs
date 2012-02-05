{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Category.Multiplicative where

import Control.Category.Monoidal

-- | Monoidal category with a multiplication natural transformation.
--
-- A multiplicative structure on @k@ is the same thing as a monoid object
-- structure on the identity functor, when End(k) is given the pointwise
-- monoidal structure.
--
-- Laws:
--
-- > first unit . mult = idl
-- > second unit . mult = idr
-- > mult . first mult = mult . second mult . associate
--
class Monoidal k p => Multiplicative k p where
  unit :: k (Id k p) a
  mult :: k (p a a) a

-- | Comonoidal category with a comultiplication natural transformation.
--
-- A comultiplicative structure on @k@ is the same thing as a coalgebra object
-- structure on the identity functor, when End(k) is given the pointwise
-- comonoidal structure.
--
-- Laws:
--
-- > first counit . comult = coidl
-- > second counit . comult = coidr
-- > first diag . diag = disassociate . second diag . diag
--
class Monoidal k p => Comultiplicative k p where
  counit :: k a (Id k p)
  comult :: k a (p a a)
