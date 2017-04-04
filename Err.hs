-- Err.hs Copyright 2015-17 by John F. Miller

-- | The error data type

{-# LANGUAGE FlexibleInstances #-}
module Err (
    Err(..)
  , Error(..)
) where

import Data.String

import Name

class Error e where
  strMsg :: String -> e
  noMsg :: e
  noMsg = strMsg ""

instance Error String where
  strMsg = id

data Err a = Err String String [a]
           | ErrPos String String Position [a]
           | ErrObj a
             deriving Eq

instance Error (Err a) where
  strMsg s = Err "StandardError" s []

instance (Show a) => Show (Err a) where
  show (Err typ msg []) = typ ++": "++msg
  show (Err typ msg set) = typ ++": "++msg ++ "\n  " ++ show set
  show (ErrObj obj) = show obj

instance IsString (Err a) where
  fromString str = strMsg str

