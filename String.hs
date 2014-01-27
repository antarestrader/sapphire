-- String.hs Copyright 2014 John F. Miller

-- | String data and functions

module String 
  ( SapString
  , mkStringLiteral
  , string
  ) where

import qualified Data.Text.Lazy as T

data SapString = 
   SapString 
      { escapes :: [String]
      , text :: T.Text 
      }

instance Show SapString where
  show  s = '"':(T.unpack $ text s) ++ ['"']

instance Eq SapString where
  a == b = text a == text b

mkStringLiteral ::  String -> SapString
mkStringLiteral s = SapString { escapes = [], text = T.pack s }

string :: SapString -> String
string = T.unpack . text
