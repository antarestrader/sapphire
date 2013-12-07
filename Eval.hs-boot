module Eval where

import {-# SOURCE #-} Context
import {-# SOURCE #-} AST
import {-# SOURCE #-} Object
import Control.Monad.Error
import Control.Monad.State

type Err = String

type EvalM a= StateT Context (ErrorT Err IO) a

runEvalM :: (EvalM a) -> Context -> IO (Either Err (a, Context))

eval :: Exp -> EvalM Value
