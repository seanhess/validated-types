{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Validated.MaxLength
  ( MaxLength
  , maxLength
  , maxLengthDrop
  , maxValue
  , CharLength(..)
  ) where


import Data.Proxy (Proxy(..))
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.TypeLits (Nat, KnownNat, natVal)

-- | Maximum length fields

data MaxLength (n :: Nat) a = MaxLength a
  deriving (Show, Eq)

instance (KnownNat n) => IsString (MaxLength n Text) where
  fromString =
    maxLengthDrop . Text.pack

maxValue :: MaxLength n a -> a
maxValue (MaxLength a) = a


-- | tools to detect the length of a type

class CharLength a where
  charLength :: a -> Int
  charLength a = Text.length $ charOut a

  charOut :: a -> Text
  charIn :: Text -> a

instance CharLength Text where
  charOut = id
  charIn = id

instance CharLength Int where
  charOut = Text.pack . show
  charIn = read . Text.unpack


-- | Create a MaxLength if the value fits within the size
maxLength :: forall n a. (KnownNat n, CharLength a) => a -> Maybe (MaxLength n a)
maxLength a
  | charLength a <= size = Just (MaxLength a)
  | otherwise = Nothing
  where
    size = fromIntegral $ natVal (Proxy :: Proxy n)


-- | Create a MaxLength by dropping extra characters from the right
maxLengthDrop :: forall n a. (KnownNat n, CharLength a) => a -> MaxLength n a
maxLengthDrop a =
  MaxLength . charIn . Text.take size . charOut $ a
  where
    size = fromIntegral $ natVal (Proxy :: Proxy n)
