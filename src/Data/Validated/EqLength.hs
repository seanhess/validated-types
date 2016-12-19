{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Validated.EqLength
  ( EqLength(..)
  , createRight
  , create
  , value
  ) where


import Data.Proxy (Proxy(..))
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Validated.CharLength (CharLength(..))
import qualified Data.Text as Text
import GHC.TypeLits (Nat, KnownNat, natVal)




-- | exact length fields

data EqLength (n :: Nat) a = EqLength a
  deriving (Show, Eq)

instance (KnownNat n) => IsString (EqLength n Text) where
  fromString =
    createRight . Text.pack

value :: EqLength n a -> a
value (EqLength a) = a



-- | Create a EqLength if the value fits within the size
create :: forall n a. (KnownNat n, CharLength a) => a -> Maybe (EqLength n a)
create a
  | charLength a == size = Just (EqLength a)
  | otherwise = Nothing
  where
    size = fromIntegral $ natVal (Proxy :: Proxy n)


-- | Create a EqLength by dropping extra characters or padding on the right
createRight :: forall n a. (KnownNat n, CharLength a) => a -> EqLength n a
createRight a =
  EqLength . charIn . pad . Text.take size . charOut $ a
  where
    size = fromIntegral $ natVal (Proxy :: Proxy n)
    pad = Text.justifyRight size ' '
