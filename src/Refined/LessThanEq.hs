{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Refined.LessThanEq where

import Refined (Predicate(..))
import Data.Monoid ((<>))

import GHC.TypeLits (Nat, KnownNat, natVal)

data LessThanEq (n :: Nat)

instance (Ord x, Num x, KnownNat n) => Predicate (LessThanEq n) x where
  validate p x =
    if x <= fromIntegral x'
      then Nothing
      else Just ("Value is not less than or eq " <> show x')
    where
      x' = natVal p

