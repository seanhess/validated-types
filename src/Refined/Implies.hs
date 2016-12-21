{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Refined.Implies where

import Refined (Refined, Predicate(..))
import Refined (EqualTo, LessThan)
import Refined.LessThanEq (LessThanEq)
import Refined.Length (Length)
import Data.Monoid ((<>))
import Data.Coerce (coerce)
import Data.CharLength (CharLength(..))

import GHC.TypeLits

-- | Certain refinedments imply others. See https://github.com/nikita-volkov/refined/pull/6

class Implies from to where
    relax :: Refined from x -> Refined to x
    relax = coerce


instance (n <= m) => Implies (LessThan n) (LessThan m)
instance (n + 1 <= m) => Implies (EqualTo n) (LessThan m)
instance (n <= m) => Implies (EqualTo n) (LessThanEq m)

instance (Implies m n) => Implies (Length m) (Length n)


