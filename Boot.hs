-- Boot.hs Copyright 2013, 2014 John F. Miller

-- | This module which exports a single function, @boot@, is responsible for
--   setting up the runtime system.  Note: if you are looking for the place
--   where all the built-in classes get loaded, it is in Builtin/Bindings.hs

module Boot (boot) where

import Var
import AST
import Builtin.Object
import Builtin.Class
import Context
import Eval
import Object
import Object.Spawn
import String
import Continuation (send, newContIO)
import qualified Data.Map as M
import System.Environment.Executable
import Control.Concurrent.STM.TMVar


baseLibrary = "lib/base.sap"

-- | This function builds up the initial runtime. The run time includes
--   Object and Class classes with the internal functions installed. The
--   object returned is an instance of Object sutable to running code.
boot :: IO Context
boot = do
  object <- objectClass
  let (Pid obj_pid) = object
  cls@(Pid cls_pid) <- classClass object
  cont <- newContIO
  send cont obj_pid (Eval $ Apply (simple "setClass") [EValue $ VObject cls] Private)
  send cont cls_pid (Eval $ Apply (simple "setClass") [EValue $ VObject cls] Private)
  send cont obj_pid (Eval $ Apply (simple "setIVar")  [EValue $ VAtom "Object", EValue $ VObject object] Private)
  send cont obj_pid (Eval $ Apply (simple "setIVar")  [EValue $ VAtom "Class", EValue $ VObject cls] Private)

  tmvar <- newEmptyTMVarIO
  let self = Object
             {  ivars   = M.fromList [("test",VInt 5)]
             ,  modules = []
             ,  klass   = object
             ,  process = tmvar
             }
  context <- newContextIO self responderObject
  file <- getExecutablePath
  -- let context' = insertLocals "__FILE__"  (VString $ mkStringLiteral file) context
  let startupSequence = do
        setScope  object
        setGlobal object
        eval $ Assign (LVar $ simple "__FILE__") (EString file)
        load baseLibrary
  r <- runEvalM startupSequence context       
  case r of
    Left err -> putStrLn (show err) >> return context
    Right (res, c) -> return c
