module Name (module Name, Map) where

import Data.Map.Strict (Map)

type Name = String
type Op = String
type PrecedenceTable = Map Op Precedence
type Arity = (Int,Maybe Int)
type Namespace a = Map Name a
type Precedence = (Int, AssocLR, AssocLR)

data AssocLR = L | R | N deriving (Show,Eq,Ord)
