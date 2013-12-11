module Object.Graph where

import Object
import Var
import qualified Data.Map as M
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar 
import Control.Concurrent.STM.TChan 

search ::  Var -> Object ->TMVar (Maybe Value) -> STM ()
search var Pid{channel = chan} cont =  writeTChan chan (Search var cont)
search _ ROOT _ = fail "Encountered the ROOT of the object graph unexpectedly"
search var obj  cont =
  case M.lookup (top var) (ivars obj) of 
    Nothing  ->search' var (klass obj) cont -- TODO look in modules
    Just val -> 
      case (bottom var) of
        Nothing -> putTMVar cont (Just val)
	Just var' -> valToObj val >>= \obj' -> retrieve var' obj' cont 

search' var Pid{channel = chan} cont = writeTChan chan (SearchClass var cont)
search' _ ROOT cont = putTMVar cont Nothing --lookup failed
search' var obj cont = 
  case M.lookup (top var) (cvars obj) of
    Just val -> putTMVar cont (Just val)
    Nothing   -> putTMVar cont Nothing --search' var (super obj) cont -- TODO look in modules

retrieve var Pid{channel = chan} cont = writeTChan chan (Retrieve var cont)
retrieve var obj cont = 
   case M.lookup (top var) (ivars obj) of 
    Nothing  -> putTMVar cont Nothing -- TODO look in modules
    Just val -> 
      case (bottom var) of
        Nothing -> putTMVar cont (Just val)
	Just var' -> valToObj val >>= \obj' -> retrieve var' obj' cont
