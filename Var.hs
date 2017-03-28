module Var 
  ( Var(..)
  , simple
  , top
  , bottom
  )
where

import Name

type Scope = [Name]

data Var = Var {name :: Name, scope :: Scope} | Self

instance Show Var where
  show Self = "self"
  show Var {name = n, scope = []} = '"':n++'"':[]
  show Var {name = n, scope = xs} = scopeToString "" xs ++ n
    where
      scopeToString str [] = str
      scopeToString str (x:xs) = scopeToString (str++x++"::") xs

simple :: String -> Var
simple n = Var {name=n,scope=[]}

top :: Var -> Name
top Var {name = n, scope = []} = n
top Var {scope = (x:xs)} = x
top Self = "self"

bottom :: Var -> Maybe Var
bottom v@(Var{scope=(x:xs)}) = Just v{scope = xs}
bottom _ = Nothing


