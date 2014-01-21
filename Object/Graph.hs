-- Object/Graph.hs  Copyright 2013,2014 John F. Miller

{-# LANGUAGE FlexibleContexts #-}

-- | Functions used to look throught the Object graph.
module Object.Graph where

import Prelude hiding(lookup)
import qualified Continuation as C
import Continuation (send, reply, dispatch)
import Context
import Object
import Var
import AST
import Control.Monad.IO.Class
import Control.Monad.Error.Class
import Control.Monad.State.Class
import qualified Data.Map as M
import Data.Maybe

lookupIVars :: String -> Object -> Context -> IO (Maybe Value)
lookupIVars _ ROOT _ = return Nothing
lookupIVars s (Pid process) c = lookupIVarsRemote s process c
lookupIVars s obj _ = return $ lookupIVarsLocal s obj

lookupIVarsM :: (MonadIO m, MonadState Context m) => String -> Object -> m (Maybe Value)
lookupIVarsM str obj = get >>= liftIO . lookupIVars str obj

lookupIVarsRemote :: String -> Process -> Context -> IO (Maybe Value)
lookupIVarsRemote s process context = do
  val <- dispatchC_ context process (SearchIVars s)
  case val of
    VError _ -> return Nothing
    val' -> return $ Just val'

lookupIVarsLocal :: String -> Object -> Maybe Value
lookupIVarsLocal _ ROOT = Nothing
lookupIVarsLocal _ (Pid _) = error "lookupIVarsLocal called with remote object"
lookupIVarsLocal s obj = M.lookup s (ivars obj)

lookupCVars :: String -> Object -> Context -> IO (Maybe Value)
lookupCVars _ ROOT _ = return Nothing
lookupCVars s (Pid process) c = lookupCVarsRemote s process c
lookupCVars s obj context = case lookupCVarsLocal s obj of
  Just val -> return $ Just val
  Nothing  -> lookupCVars s (super obj) context

lookupCVarsM str obj = get >>= liftIO . lookupCVars str obj

-- TODO adjust CVars to take advantage of tail calls
lookupCVarsRemote :: String -> Process -> Context -> IO (Maybe Value)
lookupCVarsRemote s process context = do
  val <- dispatchC_ context process (SearchCVars s)
  case val of
    VError _ -> return Nothing
    val' -> return $ Just val'

lookupCVarsLocal :: String -> Object -> Maybe Value
lookupCVarsLocal _ ROOT = Nothing
lookupCVarsLocal _ (Pid _) = error "lookupCVarsLocal called with remote object"
lookupCVarsLocal s obj = M.lookup s (cvars obj)

lookupVar ::(MonadIO m, MonadState Context m, MonadError String m) => Var -> m Value
lookupVar var = do
  context <- get
  let str = top var
  -- First try local vars
  val <- case (lookupLocals str context) of
    Just v -> return v
    Nothing -> do
      -- IVars of self
      x <-lookupIVarsM str (self context)
      case x of
        Just v -> return v
        Nothing -> do
           -- CVars of klass and beyond
           x' <- lookupCVarsM str (klass $ self context)
           case x' of
             Just v -> return v
             Nothing -> if (isJust $ bottom var) then throwError $ str ++ " not found" else return VNil
  case (bottom var) of
    Nothing -> return val
    Just var' -> do
      obj <- valToObj val
      lookupVarForObj var' obj

lookupVarForObj :: (MonadIO m, MonadState Context m, MonadError String m) => Var -> Object -> m Value
lookupVarForObj var obj = do
  let str = top var
  val <- lookupIVarsM str obj
  case (val,bottom var) of
    (Nothing,Nothing)   -> return VNil
    (Just v, Nothing)   -> return v
    (Just v, Just var') -> valToObj v >>= lookupVarForObj var'
    (Nothing, Just _)   -> throwError $ str ++" not found"


insertLHS :: (MonadState Context m, MonadIO m) => LHS -> Value -> m ()
insertLHS (LIVar str) val = do
  context <-  get
  slf'  <- insertIVar str val (self context)
  put context{self=slf'}
insertLVar (LVar (Var str [])) val = modify $ insertLocals str val

insertIVar :: (MonadState Context m, MonadIO m) => String -> Value -> Object -> m Object
insertIVar _ _ ROOT  = fail "Inserting into ROOT not allowed, (How the hell did you get your hands on ROOT?)"
insertIVar str val obj@(Pid p) = insertIVarRemote str val p >> return obj
insertIVar str val obj = return $ obj{ivars= (M.insert str val (ivars obj))}

insertIVarLocal str val obj = obj{ivars= (M.insert str val (ivars obj))}

insertIVarRemote str val p = sendM p (SetIVar str val)

insertCVar :: (MonadState Context m, MonadIO m) => String -> Value -> Object -> m Object
insertCVar _ _ ROOT = fail "Inserting into ROOT not allowed, (How the hell did you get your hands on ROOT?)"
insertCVar str val obj@(Pid p) = insertCVarRemote str val p >> return obj
insertCVar str val obj@Class{} = return $ obj{cvars= (M.insert str val (cvars obj))}
insertCVar _ _ obj  = fail "This Object is not a Class.  You cannot set a CVar in it"

insertCVarLocal str val obj@Class{} =  obj{cvars= (M.insert str val (cvars obj))}
insertCVarLocal _ _ _  = error "This Object is not a Class.  You cannot set a CVar in it"

insertCVarRemote str val p = sendM p (SetCVar str val)

valToObj :: Monad m => Value -> m Object
valToObj (VObject obj) = return obj
valToObj _ = fail "Primitive to Object maping not implimented yet"

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



