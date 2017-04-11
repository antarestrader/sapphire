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
--   object returned is an instance of Object sutable to running code.
boot :: Options  -- ^ command line options
     -> Runtime () -- ^ The program to run after boot
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


initialProcess :: Runtime ()
               -> Name -> [Object] -> Runtime Object
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
    readResponse $ Process objPid

initialState :: Options -> UIDSource -> SystemState
initialState opts uids=  SystemState {
    objectState = ROOT
  , uidSource = uids
  , localScope = []
  , cmdLineOptions = opts
  }
