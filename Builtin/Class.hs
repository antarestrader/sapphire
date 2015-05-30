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
import Control.Concurrent.STM
import System.IO.Unsafe

classClass :: Object -> IO Object
classClass object = do
  ctx <- newContextIO object undefined
  Right (cls, _) <- flip runEvalM ctx $ do -- none of these operations can fail
    setScope  object
    setGlobal object
    spawn $ Class{
        ivars = M.fromList [("setClass",VFunction setClass (1,Just 1))],
	klass = ROOT,
	modules = [],
	process = unsafePerformIO newEmptyTMVarIO,
	super = object,
	cvars = bootstrap,
        cmodules = [],
	properName = "Class"}
  return cls

bootstrap = M.fromList [
         ("new"     , VFunction new   (0,Nothing))
       , ("setCVar" , VFunction setCVar (2,Just 2))
       , ("setIVar" , VFunction setIVar (2,Just 2))
       , ("spawn"   , VFunction spawnFn (0,Nothing))
       , ("to_s"    , VFunction to_s (0, Just 0 ))
       , ("include" , VFunction includeFn (1, Nothing))
       , ("cmodules", VFunction cmodulesFn (0, Just 0 ))
       , ("instance_methods", VFunction instanceMethodsFn (0, Just 1 ))
       ]

setCVar [VAtom n,val] = do
  modifySelf $ insertCVars n val
  replyM_ val

setIVar [VAtom n,val] = do
  modifySelf $ insertIVars n val
  replyM_ val

new :: [Value] -> EvalM ()
new [] = do
  slf <- gets self
  tmvar <- liftIO $ newEmptyTMVarIO
  slf' <- case slf of
      pid@(Pid _) -> return pid
      cls@Class{} -> do
            pid <- liftIO $ atomically $ tryReadTMVar $ process cls
            return $ maybe slf Pid pid
      _ -> error "trying to make instantiate a regular object.  What would that even mean?"
  let obj = VObject $ Object {
      ivars = M.empty
    , klass = slf'
    , modules = []
    , process =  tmvar
    }
  -- initialize here
  replyM_ obj

spawnFn xs = do
  (Response r) <- extract $ new xs
  r' <- valToObj r
  obj <- spawn r'
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

instanceMethodsFn  :: [Value] -> EvalM()
instanceMethodsFn _ = do -- TODO: for true values move through inheritance chain
   slf <- gets self
   replyM_ $ VArray $ fromList $ map VAtom $ M.keys $ cvars slf


