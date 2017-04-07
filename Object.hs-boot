module Object where

import Data.String
import qualified Runtime as R
import Name

type Runtime = R.Runtime SystemState Object
type PID = R.PID Object
data Fn

data Object
data Primitive
data State
data SystemState


vnil :: Object
instance Show Object
instance Eq Object
instance IsString Object
