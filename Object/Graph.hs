module Object.Graph where

import Object
import Var
import qualified Data.Map as M
import Control.Concurrent.MVar --may want strict
import Control.Concurrent.Chan --may want strict

search ::  Var -> Object ->MVar (Maybe Value) -> IO ()
search var Pid{channel = chan} cont = writeChan chan (Search var cont)
search _ ROOT _ = fail "Encountered the ROOT of the object graph unexpectedly"
search var obj  cont =
  case M.lookup (top var) (ivars obj) of 
    Nothing  -> search' var (klass obj) cont -- TODO look in modules
    Just val -> 
      case (bottom var) of
        Nothing -> putMVar cont (Just val)
	Just var' -> valToObj val >>= \obj' -> retrieve var' obj' cont 

search' var Pid{channel = chan} cont = writeChan chan (SearchClass var cont)
search' _ ROOT cont = putMVar cont Nothing --lookup failed
search' var obj cont = 
  case M.lookup (top var) (cvars obj) of
    Just val -> putMVar cont (Just val)
    Nothing   -> search' var (super obj) cont -- TODO look in modules

retrieve var Pid{channel = chan} cont = writeChan chan (Retrieve var cont)
retrieve var obj cont = 
   case M.lookup (top var) (ivars obj) of 
    Nothing  -> putMVar cont Nothing -- TODO look in modules
    Just val -> 
      case (bottom var) of
        Nothing -> putMVar cont (Just val)
	Just var' -> valToObj val >>= \obj' -> retrieve var' obj' cont
