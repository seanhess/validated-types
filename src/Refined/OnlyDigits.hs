{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Refined.OnlyDigits where

import Refined (Predicate(..))
import Data.Char (isDigit)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text

data OnlyDigits



instance Predicate OnlyDigits Text where
  validate _ x =
    if Text.all isDigit x
      then Nothing
      else Just ("Value contains non-digits " <> show x)


instance Predicate OnlyDigits String where
  validate _ x =
    if all isDigit x
      then Nothing
      else Just ("Value contains non-digits " <> show x)
