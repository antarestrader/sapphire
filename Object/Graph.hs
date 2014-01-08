module Object.Graph where

import qualified Continuation as C
import Continuation (send, reply, dispatch)
import Object
import Var
import qualified Data.Map as M
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar 
import Control.Concurrent.STM.TChan 

search ::  Var -> Object -> Continuation -> IO ()
search var (Pid pid) cont = C.tail cont pid (Search var)
search _ ROOT _ = fail "Encountered the ROOT of the object graph unexpectedly"
search var obj  cont =
  case M.lookup (top var) (ivars obj) of 
    Nothing  -> search' var (klass obj) cont -- TODO look in modules
    Just val -> 
      case (bottom var) of
        Nothing -> reply cont (val) >> return () 
	Just var' -> valToObj val >>= \obj' -> retrieve var' obj' cont 

search' ::  Var -> Object -> Continuation -> IO ()
search' var (Pid pid) cont = C.tail cont pid (SearchClass var)
search' var ROOT cont = do
  reply cont $ VError $ "Variable not found: " ++ show var
  return ()
search' var obj cont = 
  case M.lookup (top var) (cvars obj) of
    Just val -> reply cont val >> return ()
    Nothing   -> search' var (super obj) cont -- TODO look in modules

retrieve ::  Var -> Object -> Continuation -> IO ()
retrieve var (Pid pid) cont = C.tail cont pid (Retrieve var)
retrieve var ROOT cont = do
  reply cont $ VError $ "Not found in ROOT: " ++ show var
  return ()
retrieve var obj cont = 
   case M.lookup (top var) (ivars obj) of 
    Nothing  -> do
      reply cont $ VError $ "Variable not found in scope: " ++ show var -- TODO look in modules
      return ()
    Just val -> 
      case (bottom var) of
        Nothing -> reply cont val >> return ()
	Just var' -> valToObj val >>= \obj' -> retrieve var' obj' cont
