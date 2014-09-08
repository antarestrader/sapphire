module Builtin.Bindings where

import {-# SOURCE #-} Eval
import Object

initialize :: [Value] -> EvalM ()

bindPrimitiveObject ::  Value -> EvalM Object
