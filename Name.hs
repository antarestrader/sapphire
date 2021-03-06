module Name (module Name, Map) where

import Data.Map.Strict (Map)
import Data.Word

type Name = String
type Op = String
type UID = Word32
type PrecedenceTable = Map Op Precedence
type Arity = (Int,Maybe Int)
type Namespace a = Map Name a
type Precedence = (Int, AssocLR, AssocLR)

data Position = Position 
  { filename :: FilePath
  , lineNo :: Integer
  , offset :: Integer
  } deriving (Show,Eq)

data AssocLR = L | R | N deriving (Show,Eq,Ord)
data Visibility = Public | Private | Protected deriving (Show, Eq)
data Order = Append | Prepend | Overwrite deriving (Show,Ord,Eq)

