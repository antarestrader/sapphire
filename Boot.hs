-- Boot.hs Copyright 2013, 2014 John F. Miller

-- | This module which exports a single function, @boot@, is responsible for
--   setting up the runtime system.

module Boot (boot) where

import Var
import Builtin.Object
import Builtin.Class
import Object
import Object.Spawn
import Continuation (send, newContIO)
import qualified Data.Map as M

-- | This function builds up the initial runtime. The run time includes
--   Object and Class classes with the internal functions installed. The
--   object returned is an instance of Object sutable to running code.
boot :: IO Object
boot = do
  object <- objectClass
  let (Pid obj_pid) = object 
  cls <- classClass object
  cont <- newContIO
  send cont obj_pid (Execute (simple "setClass") [VObject cls])
  send cont obj_pid (Execute (simple "setCVar")  [VAtom "Object", VObject object])
  send cont obj_pid (Execute (simple "setCVar")  [VAtom "Class", VObject cls]) 

  return $ Object {ivars = M.fromList [("test",VInt 5)], modules=[], klass = object, process=Nothing}
