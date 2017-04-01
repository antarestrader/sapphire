module Var 
  ( Var(..)
  , simple
  , top
  , bottom
  )
where

import Name

data Var = Var {name :: Name, varscope :: [Name]} | Self

instance Show Var where
  show Self = "self"
  show Var {name = n, varscope = []} = '"':n++'"':[]
  show Var {name = n, varscope = xs} = scopeToString "" xs ++ n
    where
      scopeToString str [] = str
      scopeToString str (x:xs) = scopeToString (str++x++"::") xs

simple :: String -> Var
simple n = Var {name=n,varscope=[]}

top :: Var -> Name
top Var {name = n, varscope = []} = n
top Var {varscope = (x:xs)} = x
top Self = "self"

bottom :: Var -> Maybe Var
bottom v@(Var{varscope=(x:xs)}) = Just v{varscope = xs}
bottom _ = Nothing


