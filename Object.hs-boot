module Object where

data Message
data Object
data AssocLR
type Precedence = (Int, AssocLR, AssocLR)
type Arity = (Int,Maybe Int)
data Value
vnil :: Value
verror :: String -> Value
instance Eq Value
instance Show Value
