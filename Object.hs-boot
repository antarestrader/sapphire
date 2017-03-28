module Object where

import Data.String
import qualified Runtime as R
import Name

--type Precedence = (Int, AssocLR, AssocLR)
type Runtime = R.Runtime State Object
type PID = R.PID Object
type Fn = [Object] -> Runtime Object

data Object
data State
data Primitive
--data AssocLR

vnil :: Object
instance Show Object
instance IsString Object
