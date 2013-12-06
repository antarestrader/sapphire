module Var where

data Var = Var {name :: String, scope :: Scope} | Self

instance Show Var where
  show Self = "self"
  show Var {name = n, scope = []} = n
  show Var {name = n, scope = xs} = scopeToString "" xs ++ n
    where 
      scopeToString str [] = str
      scopeToString str (x:xs) = scopeToString (str++x++"::") xs

type Op = String

type Scope = [String]
