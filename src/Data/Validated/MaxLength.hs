{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Validated.MaxLength
  ( MaxLength
  , create
  , createRight
  , fromEq
  , value
  ) where


import Data.Proxy (Proxy(..))
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Validated.CharLength (CharLength(..))
import Data.Validated.EqLength (EqLength(..))
import qualified Data.Text as Text
import GHC.TypeLits (Nat, KnownNat, natVal)




-- | Maximum length fields

data MaxLength (n :: Nat) a = MaxLength a
  deriving (Show, Eq)

instance (KnownNat n) => IsString (MaxLength n Text) where
  fromString =
    createRight . Text.pack

value :: MaxLength n a -> a
value (MaxLength a) = a


-- | Create a MaxLength from EqLength
fromEq :: EqLength n a -> MaxLength n a
fromEq (EqLength a) = MaxLength a


-- | Create a MaxLength if the value fits within the size
create :: forall n a. (KnownNat n, CharLength a) => a -> Maybe (MaxLength n a)
create a
  | charLength a <= size = Just (MaxLength a)
  | otherwise = Nothing
  where
    size = fromIntegral $ natVal (Proxy :: Proxy n)


-- | Create a MaxLength by dropping extra characters from the right
createRight :: forall n a. (KnownNat n, CharLength a) => a -> MaxLength n a
createRight a =
  MaxLength . charIn . Text.take size . charOut $ a
  where
    size = fromIntegral $ natVal (Proxy :: Proxy n)
