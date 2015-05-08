module Eval where

import {-# SOURCE #-} Context
import {-# SOURCE #-} AST
import {-# SOURCE #-} Object
import Control.Monad.Except
import Control.Monad.State
import Err

type EvalM a= StateT Context (ExceptT (Err Value) IO) a

runEvalM :: (EvalM a) -> Context -> IO (Either (Err Value) (a, Context))

eval  ::  Exp -> EvalM Value
evalT ::  Exp -> EvalM ()
