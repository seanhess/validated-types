{-# LANGUAGE OverloadedStrings #-}
module Data.Validated.CharLength
  ( CharLength(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

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
