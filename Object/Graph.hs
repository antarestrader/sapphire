-- Object/Graph.hs  Copyright 2013,2014 John F. Miller

-- | Functions used to look throught the Object graph.
module Object.Graph where

import qualified Continuation as C
import Context
import Continuation (send, reply, dispatch)
import Object
import Var
import qualified Data.Map as M




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

lookupM :: (MonadState Context m, MonadIO m) => Var -> m (Maybe Value)
lookupM var = get >>= (liftIO . lookup var)

retrieveM ::  (MonadState Context m, MonadIO m) => Var -> m (Maybe Value)
retrieveM var = do 
  c <- get  
  liftIO $ retrieve' var (self c) c

lookup :: Var -> Context -> IO (Maybe Value) --Check Local context
lookup Self c = return $ Just $ VObject $ self c
lookup var c = 
  case  M.lookup (top var) (locals c) of
    Nothing  ->  lookupIvars var (self c) c-- find in self  
    Just val -> 
      case bottom var of 
        Nothing -> return $ Just val
	Just var' -> do  --convert var' to object and search there.   
          obj' <-  valToObj val
          retrieve' var' obj' c

lookupIvars :: Var -> Object -> Context -> IO (Maybe Value)
lookupIvars var (Pid pid) c = Just `fmap` dispatchC_ c pid (Search var)
lookupIvars _ ROOT _ = return Nothing
lookupIvars var obj c = case M.lookup (top var) (ivars obj) of
  Nothing  -> lookupCVars var (klass obj) c
  Just val ->
    case bottom var of
      Nothing -> return $ Just val
      Just var' -> do
        obj' <- valToObj val
        retrieve' var' obj' c

lookupCVars :: Var -> Object -> Context -> IO (Maybe Value)
lookupCVars var (Pid pid) c = Just `fmap` dispatchC_ c pid (SearchClass var)
lookupCVars var obj@Class {} c = case M.lookup (top var) (cvars obj) of
  Nothing  -> lookupCVars var (super obj) c
  Just val -> case (bottom var) of 
    Nothing   -> return $ Just val
    Just var' -> do
      obj' <- valToObj val
      retrieve' var' obj' c
lookupCVars _ _ _ = return Nothing


retrieve' :: Var -> Object -> Context -> IO (Maybe Value)
retrieve' var (Pid pid) c = Just `fmap` dispatchC_ c pid (Retrieve var)
retrieve' _ ROOT _ = return Nothing
retrieve' var obj c = case M.lookup (top var) (ivars obj) of
  Nothing  -> return Nothing
  Just val ->
    case bottom var of
      Nothing -> return $ Just val
      Just var' -> do
        obj' <- valToObj val
        retrieve' var' obj' c

insertIVar :: String -> Value -> Context -> Context
insertIVar s val c@Context{self=obj} = c{self=obj{ivars=(M.insert s val (ivars obj))}}

insertCVarM s val = do
  c <- get
  let obj = self c
  case obj of
    Class{cvars = cvs} -> do
      put c{self = obj{cvars = M.insert s val cvs}}
      return val
    _ -> throwError "Class opperation performed on non-class object"


precedence :: Context -> M.Map Op Precedence
precedence _ = -- TODO read from Context
  M.fromList [
      ("+",(6,L,N))
    , ("-",(6,L,N))
    , ("*",(7,L,N))
    , ("/",(7,L,N))
    , ("<",(4,N,N))
    , (">",(4,N,L))
    , ("==",(4,N,L))
  ]



