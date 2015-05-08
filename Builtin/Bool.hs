module Builtin.Bool where

import Builtin.Utils
import Context
import Object
import String
import Control.Monad.Except
import qualified Data.Map as M

boolClasses = do
  buildClass "TrueClass"  trueBoot
  buildClass "FalseClass" falseBoot

trueBoot = M.fromList [
    ("&&", VFunction tand (1,Just 2))
  , ("||", VFunction tor (1,Just 2))
  , ("not", VFunction tnot (0,Just 0))
  , ("bool?", VFunction is_bool (0, Just 0))
  ]

is_bool [] = replyM_ VTrue

tnot [] = replyM_ VFalse

tand (v:_) | (v==VFalse || v==VNil) = replyM_ VFalse
tand [v] = replyM_ v
tand (v:_) = replyM_ VTrue

tor (v:_) | (v==VFalse || v==VNil) = replyM_ VTrue
tor [v] = replyM_ VTrue
tor (v:_) = replyM_ v

-- -- -- -- --

falseBoot = M.fromList [
    ("&&", VFunction fand (1,Just 2))
  , ("||", VFunction f_or (1,Just 2))
  , ("not", VFunction fnot (0,Just 0))
  , ("bool?", VFunction is_bool (0, Just 0))
  ]

fnot [] = replyM_ VTrue

fand _ = replyM_ VFalse

f_or (v:_) = replyM_ v

