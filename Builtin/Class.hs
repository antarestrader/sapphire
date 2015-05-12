module Builtin.Class where

import Builtin.Utils
import Object
import Context
import Object.Graph
import Object.Spawn
import Context
import Err
import Eval
import AST
import Var
import String
import Array
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.Except

classClass object = spawn $ Class{
        ivars = M.fromList [("setClass",VFunction setClass (1,Just 1))],
	klass = ROOT,
	modules = [],
	process = Nothing,
	super = object,
	cvars = bootstrap,
        cmodules = [],
	properName = "Class"}

bootstrap = M.fromList [
         ("new"     , VFunction new   (0,Nothing))
       , ("setCVar" , VFunction setCVar (2,Just 2))
       , ("spawn"   , VFunction spawnFn (0,Nothing))
       , ("to_s"    , VFunction to_s (0, Just 0 ))
       , ("include" , VFunction includeFn (1, Nothing))
       , ("cmodules", VFunction cmodulesFn (0, Just 0 ))
       ]

setCVar [VAtom n,val] = do
  modifySelf $ insertCVars n val
  replyM_ val

new [] = do
  slf <- gets self
  let obj = VObject $ Object {
      ivars = M.empty
    , klass = slf
    , modules = []
    , process = Nothing
    }
  -- initialize here
  replyM_ obj

spawnFn xs = do
  (Response r) <- extract $ new xs 
  r' <- valToObj r
  obj <- liftIO $ spawn r' 
  replyM_ $ VObject obj

to_s [] = do
  (VObject Class{properName = s}) <- eval (EVar Self)
  replyM_ $ VString $ mkStringLiteral $ s

setClass [VObject cls] = do
  slf <- gets self
  modify (\c -> c{self=slf{klass=cls}})
  replyM_ VNil

includeFn:: [Value] -> EvalM()
includeFn mdls = do
    val <- eval (EVar Self)
    case val of
      VObject obj@Class{} -> do
        obj' <- loop obj mdls
        modifySelf $ const obj'
        replyM_ VNil
      _ -> throwError $ Err  "SystemError" "Tried to include a module in on Objcet that was not a local class" [val]
  where
    loop obj [] = return obj
    loop obj (x:xs) = do
      o <- valToObj x
      loop (obj{cmodules = (o:(cmodules obj))}) xs

cmodulesFn :: [Value] -> EvalM()
cmodulesFn _ = do
  slf <- gets self
  replyM_ $ VArray $ fromList $ map VObject $ cmodules slf 
  

