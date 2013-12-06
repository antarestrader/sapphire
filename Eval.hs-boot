module Eval where

import {-# SOURCE #-} Context
import Control.Monad.Error
import Control.Monad.State

type EvalM a= StateT Context (ErrorT String IO) a
