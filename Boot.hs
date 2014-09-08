-- Boot.hs Copyright 2013, 2014 John F. Miller

-- | This module which exports a single function, @boot@, is responsible for
--   setting up the runtime system.

module Boot (boot) where

import Var
import Object
import Object.Spawn
import Continuation (send, newContIO)
import Control.Monad.State
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TMVar
import qualified Data.Map as M
import qualified BuiltinFunctions as F

-- | This function builds up the initial runtime. The run time includes
--   Object and Class classes with the internal functions installed. The
--   object returned is an instance of Object sutable to running code.
boot :: IO Object
boot = do
  object <- spawn $ VObject Class{
        ivars = M.fromList [("setClass",VFunction F.setClass (1,Just 1))],
        klass = ROOT,
        modules = [],
	process = Nothing,
        super = ROOT,
        cvars = bootstrapset,
        properName = "Object"}
  let (Pid obj_pid) = object 
  cls <- spawn $ VObject Class{
        ivars = M.empty,
	klass = object,
	modules = [],
	process = Nothing,
	super = object,
	cvars = clsBootstrap,
	properName = "Class"}
  cont <- newContIO
  send cont obj_pid (Execute Var{name="setClass", scope=[]} [VObject cls])
  send cont obj_pid (Execute Var{name="setCVar", scope=[]}  [VAtom "Object", VObject object])
  send cont obj_pid (Execute Var{name="setCVar", scope=[]}  [VAtom "Class", VObject cls]) 

  return $ Object {ivars = M.fromList [("test",VInt 5)], modules=[], klass = object, process=Nothing}


bootstrapset = M.fromList [
         ("add" , VFunction F.add  (2,Just 2))
       , ("+"   , VFunction F.add  (2,Just 2))
       , ("-"   , VFunction F.sub  (2,Just 2))
       , ("*"   , VFunction F.mult (2,Just 2))
       , ("puts", VFunction F.puts (0,Nothing))
       , ("to_s", VFunction F.to_s (0,Just 0)) 
       , ("cls" , VFunction F.cls  (0, Just 0))
       , ("setCVar" , VFunction F.setCVar  (2, Just 2))
       , ("__initialize", VFunction F.initialize (0, Just 0))
       , ("debug", VFunction F.debug (0,Just 0))
       ]

clsBootstrap = M.fromList [
         ("new"  , VFunction F.new   (0,Just 1))
       , ("spawn", VFunction F.spawn (0,Just 1))
       , ("to_s" , VFunction F.to_s_Class (0, Just 0 ))
       ]
