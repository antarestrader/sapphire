-- Array/BuiltinFunctions.hs Copyright 2014 John F. Miller

-- | This module exports the functions needed to bootstrap the Array Class.

module Array.BuiltinFunctions where

import Prelude hiding (length)
import qualified Array as A
import qualified Data.Map as M
import Object
import Object.Graph (lookupIVarsM)
import Object.Spawn (spawn)
import Eval
import AST
import Var (simple)
import Context (self, replyM_)
import Control.Monad.State (gets)
import Control.Monad.IO.Class
import Data.Foldable

innerValue :: EvalM Value
innerValue = do
  slf <- gets self
  val <- lookupIVarsM "__value" slf
  case val of
    Just v -> return v
    Nothing -> return $ VError "Internal Value at @__value was not found in self"

bootstrap = M.fromList [
         ("length",  VFunction length (0,Just 0))
       , ("__index", VFunction index (1,Just 1))
       , ("inject",  VFunction inject (2,Just 2))
       ]

-- | build the Array class with internal functions installed
arrayClass :: EvalM Object
arrayClass = do
  VObject superClass <- eval ( EVar $ simple "Object")
  VObject clsClass   <- eval ( EVar $ simple "Class")
  let cls = 
        VObject Class
          { ivars = M.empty
          , klass = clsClass
          , modules = []
          , process = Nothing
          , super = superClass
          , cvars = bootstrap
          , properName = "Array"
          }
  Pid pid <- liftIO $ spawn cls
  -- sendM pid $ Eval <<initialization>>  -- no initialization needed at this time
  eval $ Call (EVar $ simple "Object") "setCVar" [EAtom "Array", EValue $ VObject $ Pid pid]
  return $ Pid pid

length :: [Value] -> EvalM ()
length [] = do
  (VArray arr) <- innerValue -- TODO: deal with match failure
  replyM_ $ VInt $ fromIntegral $ A.length arr

index [VInt i] | i>=0 = do
    (VArray arr) <- innerValue
    if i' >= A.length arr 
      then replyM_ VNil -- out of bounds
      else replyM_ $ arr `A.index` i'
  where i' = fromInteger i
index _ = error "Index must take a non-negative integer, Call '[]' instead."

inject [init, fn] = do
  (VArray arr) <- innerValue
  replyM_ =<< foldlM fn' init arr
    where
      fn' accum val = eval (ApplyFn (EValue fn) [(EValue accum),(EValue val)])


