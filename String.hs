-- String.hs Copyright 2014-17 John F. Miller

-- | String data and functions
{-# LANGUAGE OverloadedStrings #-}
module String
  ( module Data.String
  , SapString
  , mkStringLiteral
  , string
  , stringLength
  , sconcat
  ) where

import qualified Data.Text.Lazy as T
import Data.Monoid
import Data.String
import Control.Monad.Except

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

sconcat :: (IsString e, MonadError e m) =>
    SapString -> SapString -> m SapString
sconcat SapString{escapes=e1, text=x} SapString{escapes=e2, text=y}
    | (e1 == e2) = return SapString{escapes = e1, text=mappend x y}
    | otherwise  = throwError $  fromString
          "String Error:Concatenating Strings with different escapes!"

instance Monoid SapString where
  mempty = SapString{escapes = [], text = T.empty}

  mappend SapString{escapes=e1, text=x} SapString{escapes=e2, text=y}
    | (e1 == e2) = SapString{escapes = e1, text=mappend x y}
    | otherwise  = error "Concatenating Strings with different escapes!"  -- TODO fixme
