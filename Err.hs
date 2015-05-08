-- Err.hs Copyright 2015 by John F. Miller

-- | The error data type
--
--   This replaces String as a more detailed error to be thrown within the 
--   EvalM monad.
{-# LANGUAGE FlexibleInstances #-}
module Err (
    Err(..)
  , Error(..)
) where

class Error e where
  strMsg :: String -> e
  noMsg :: e
  noMsg = strMsg ""

instance Error String where
  strMsg = id

data Err a = Err String String [a]
           | ErrObj a

instance Error (Err a) where
  strMsg s = Err "StandardError" s []

instance (Show a) => Show (Err a) where
  show (Err typ msg []) = typ ++": "++msg
  show (Err typ msg set) = typ ++": "++msg ++ "\n  " ++ show set
  show (ErrObj obj) = show obj

