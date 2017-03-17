-- String.hs Copyright 2014 John F. Miller

-- | String data and functions

module String
  ( SapString
  , mkStringLiteral
  , string
  , stringLength
  ) where

import qualified Data.Text.Lazy as T
import Data.Monoid
import Data.String

data SapString =
   SapString
      { escapes :: [String]
      , text :: T.Text
      }

instance Show SapString where
  show  s = '"':(T.unpack $ text s) ++ ['"']

instance Eq SapString where
  a == b = text a == text b

stringLength s = fromIntegral $ T.length $ text s

instance IsString SapString where
  fromString str = mkStringLiteral str

mkStringLiteral ::  String -> SapString
mkStringLiteral s = SapString { escapes = [], text = T.pack s }

string :: SapString -> String
string = T.unpack . text

instance Monoid SapString where
  mempty = SapString{escapes = [], text = T.empty}

  mappend SapString{escapes=e1, text=x} SapString{escapes=e2, text=y}
    | (e1 == e2) = SapString{escapes = e1, text=mappend x y}
    | otherwise  = error "Concatenating Strings with different escapes!"  -- TODO fixme
