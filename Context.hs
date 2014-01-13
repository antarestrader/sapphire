{-# LANGUAGE FlexibleContexts #-}
module Context where

import Prelude hiding (lookup)
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Error
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Concurrent.STM
import Object
import Object.Graph
import {-# SOURCE #-} Object.Spawn
import Continuation (dispatch, send, tail, reply)
import Var

data Context = Context 
               { locals :: M.Map String Value
               , self :: Object
               , continuation :: Continuation
               }

dispatchC :: Context -> Process -> Message -> IO (Value, Context)
dispatchC c pid msg= do
  (val, self') <- dispatch (self c) (responderObject) (continuation c) pid msg
  return (val, c{self=self'})

dispatchC_ :: Context -> Process -> Message -> IO Value
dispatchC_ c p m = fst `fmap` dispatchC c p m

dispatchM :: (MonadState Context m, MonadIO m) => Process -> Message -> m Value
dispatchM pid msg= do
  context <- get
  (val,context') <- liftIO $ dispatchC context pid msg
  put context'
  return val

sendC :: Context->Process->Message-> IO Replier
sendC c pid msg = send (continuation c) pid msg

sendM ::  (MonadState Context m, MonadIO m) => Process -> Message -> m Replier
sendM pid msg = do
  cont <- gets continuation
  liftIO $ send cont pid msg

replyM ::  (MonadState Context m, MonadIO m) => Value -> m Bool
replyM val = do
  cont <- gets continuation
  liftIO $ reply cont val

with ::  (MonadState Context m, MonadIO m) => Object -> m a -> m(a, Object)
with obj f = do
  context <- get
  put context{self=obj}
  a <- f
  obj' <- gets self
  put context
  return (a, obj')

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

insert :: Var -> Value -> Context -> Context
insert (Var {name=s, scope=[]}) val c@Context {locals=l} =
  c{locals = M.insert s val l}

merge :: [(String,Value)] -> Context -> Context
merge params  c@Context {locals=l} = 
  c{locals = M.union (M.fromList params) l} 

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
