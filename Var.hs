module Var where

data Var = Var {name :: String, scope :: Scope} | Self deriving Show 
  -- TODO: Better Show for Var

type Op = String

type Scope = [String]
