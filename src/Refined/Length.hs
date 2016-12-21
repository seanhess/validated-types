{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Refined.Length where

import Refined (Predicate(..))
import Refined (EqualTo)
import Refined.LessThanEq (LessThanEq)
import Data.CharLength (CharLength(..))

import GHC.TypeLits (Nat)


-- | The length of an input
data Length p

instance (CharLength x, Predicate p Int) => Predicate (Length p) x where
  validate p x =
    validate p (charLength x)


type LengthEq (n :: Nat) =
    Length (EqualTo n)

type LengthMax (n :: Nat) =
    Length (LessThanEq n)


