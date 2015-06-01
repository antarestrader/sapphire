module Builtin.Error where

import Builtin.Utils
import Eval
import AST
import Object
import qualified Data.Map as M

errorClass = buildClass "Error" bootError

bootError = M.fromList [
    ("initialize", VFunction initFn (2,Nothing))
  ]

initFn :: [Value] -> EvalM()
initFn (name:msg:objs) = evalT $  flip Block "Builtin/Error.hs" [
    Assign (LIVar "error")   $ EValue name
  , Assign (LIVar "message") $ EValue msg 
  , Assign (LIVar "vars") $ EArray $ map EValue objs
  ]

