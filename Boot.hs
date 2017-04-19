-- Boot.hs Copyright 2013-2017 John F. Miller

-- | This module which exports a single function, @boot@, is responsible for
--   setting up the runtime system.  Note: if you are looking for the place
--   where all the built-in classes get loaded, it is in Builtin/Bindings.hs

module Boot (boot) where

import Control.Concurrent.STM
import Control.Monad.State hiding (State, state, ap)
import System.Exit
import Data.Map.Strict (empty)

import Object
import Boot.Options
import Object.UID hiding (nextUID)
import Runtime (bootstrap, call, readResponse)
import qualified Runtime as R
import Runtime.PID hiding (PID)
import qualified Runtime.Runtime as RR
import Runtime.Hole
import Object.Runtime
import Builtin.Bindings
import Builtin.Object
import Builtin.Class
import Scope
import Name

-- | This function builds up the initial runtime. The run time includes
--   Object and Class classes with the internal functions installed. The
--   initialize function then runs the program in a specialized instance of
--   Object called main.
boot :: Options  -- ^ command line options
     -> Runtime Response -- ^ The program to run after boot
     -> IO () -- ^ The action which is running the program
boot opts prgm = do
  uids <- newUIDSource
  pid <- bootstrap (initialState opts uids) (initialProcess prgm)
  (r,e) <- atomically $ do
    res <- mkHole
    err <- mkHole
    writePID pid $ Call "initialize" [] empty res err
    return (res, err)
  final <- atomically $  (Right <$> readHole r) `orElse`  (Left <$> readHole e)
  case final of
    Right _ -> exitSuccess
    Left err -> die (show err)

-- The process which can "tie the knot" between Object and Class. This process
-- will become Object and it will send its pid to a specially made Class
-- object and then use that to configure its own state.
initialProcess :: Runtime Response
               -> Name -> [Object] -> Runtime Response
initialProcess prgm _ _ = do
    objPid <- self
    uid <- nextUID
    ss <- R.getState
    clsPid <- R.spawn
      ss{  localScope = []
         , objectState = (classObject objPid)
         }
      (evalProcess classInit classProcess)
    put $ stateForObject objPid clsPid uid
    RR.Runtime $ modify (\rts -> rts{RR.fn=classProcess})
    main <- initialize prgm
    tailCall (Just main) "run" []

initialState :: Options -> UIDSource -> SystemState
initialState opts uids=  SystemState {
    objectState = ROOT
  , uidSource = uids
  , localScope = []
  , cmdLineOptions = opts
  , reciever = Nothing
  }
