module Object where

data Message
data Object
data AssocLR
type Precedence = (Int, AssocLR, AssocLR)
data SapString
type Arity = (Int,Maybe Int)
data Value
instance Eq Value
instance Show Value
