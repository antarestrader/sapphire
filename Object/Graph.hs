-- Object/Graph.hs  Copyright 2013,2014 John F. Miller

{-# LANGUAGE FlexibleContexts #-}

-- | Functions used to look throught the Object graph.
-- 
--   Most of these functions find an object within the current context.  The
--   most general case is 'lookupVar' which looks up a symbol starting with the
--   local variable map then moving on to IVars and CVars of @self@.
--
--   Monadic versions of these functions assume a state monad with the state 
--   holding the 'Context'. Some expressly require 'EvalM' because they need
--   IO. In all cases it is expected that the monad is typically 'EvalM'.
--
--   One non-lookup function is 'valToObj'.  This function turns a 'Value' into an 
--   'Object' as its name would imply. In order to turn primitive values into
--   compltet objects with a class and instance, it needs access to the current
--   Context to look up the containting class.  Going the other way (from Object
--   to Value) is simply a matter of wrapping the object with the 'VObject'
--   constructor. Once an object is created from a primitive value, it cannot
--   be retuneded to a primitive value.
--
--   There are also a set of functions used for assignment prefixed with @insert@

module Object.Graph (
  -- * Functions for Finding Special Values
    valToObj
  , precedence
  
  -- * Value Lookup Functions 
  , lookupVar
  , lookupVarContext
  , lookupIVars
  , lookupIVarsM
  , lookupCVars
  , lookupCVarsM

  -- * Assignment / Mutation Functions
  , insertLHS
  , insertIVar
  , insertCVar

  -- * Local Functions Exported for Object.Spawn
  , lookupCVarsLocal
  , lookupIVarsLocal
  , insertCVarLocal
  , insertIVarLocal
  )
where

import Prelude hiding(lookup)
import qualified Continuation as C
import Continuation (send, reply, dispatch)
import Context
import Object
import Var
import AST
import {-# SOURCE #-} Eval
import Control.Monad.IO.Class
import Control.Monad.Error.Class
import Control.Monad.State.Class
import qualified Data.Map as M
import Data.Maybe

-- |  This function turns a 'Value' into an 
--   'Object' as its name would imply. In order to turn primitive values into
--   compltet objects with a class and instance, it needs access to the current
--   Context to look up the containting class.  Going the other way (from Object
--   to Value) is simply a matter of wrapping the object with the 'VObject'
--   constructor. Once an object is created from a primitive value, it cannot
--   be retuneded to a primitive value.
valToObj :: Value -> EvalM Object
valToObj (VObject obj) = return obj
valToObj val@(VInt _) = do
  cls <- getPrimClass "Number" 
  return $ buildPrimInstance cls val
valToObj VNil = do
  cls <- getPrimClass "NilClass"
  return $ buildPrimInstance cls VNil
valToObj val@(VString _) = do
  cls <- getPrimClass "String"
  return $ buildPrimInstance cls val
valToObj val@(VArray _) = do
  cls <- getPrimClass "Array"
  return $ buildPrimInstance cls val
valToObj val@(VFunction{}) = do
  cls <- getPrimClass "Function"
  return $ buildPrimInstance cls val
valToObj val = throwError $ "No Class for this type: " ++ show val --TODO impliment classes

-- | lookup the class for primitive values in the current context.
--   (internal function)
getPrimClass :: String -> EvalM Object
getPrimClass str = do
  cls' <- (eval $ EVar $ simple str)
  case cls' of
    (VObject obj) -> return obj
    _ -> throwError $ "System Error: Primitive class not found: " ++ str

-- | Given the primitive class and a primitive value build in instance.
--   (internal function)
buildPrimInstance :: Object -- the class
                  -> Value  -- the value
                  -> Object
buildPrimInstance cls val = Object 
                          { ivars = M.singleton "__value" val
                          , klass = cls
                          , modules = []
                          , process = Nothing
                          }
-- | The most general form of lookup, this function finds the value at Var 
--   in the the current context.  If nothing is fount it returnd 'VNil'.
lookupVar :: Var -> EvalM Value
lookupVar = fmap fst . lookupVarContext

-- | Gets the Value of a Var from the current context and return it along with a
-- | function which will put a new or modified value back where it came from.
lookupVarContext :: Var -> EvalM (Value, Value -> EvalM ())
lookupVarContext Self = do 
    result <- gets  (VObject . self)
    return (result, fn)
  where
    fn val = do
      obj <- valToObj val
      modify (\ctx ->  ctx{self = obj})
lookupVarContext var = do
  context <- get
  let str = top var
  -- First try local vars
  (val,fn) <- case (lookupLocals str context) of
    Just v -> return (v, (\modifiedValue -> modify $ insertLocals str modifiedValue))
    Nothing -> do
      -- IVars of self
      x <-lookupIVarsM str (self context)
      case x of
        Just v -> return (v, (\val -> modify (\ctx-> ctx{self = insertIVarLocal str val (self ctx)})))
        Nothing -> do
           -- CVars of klass and beyond
           x' <- lookupCVarsM str (klass $ self context)
           case x' of
             Just v -> return (v, (\_ -> throwError "Mutation of a CVar doesn't seem like a good idea"))
             Nothing -> if (isJust $ bottom var) then throwError $ str ++ " not found" else return (VNil, (\_->return ()))
  case (bottom var) of
    Nothing -> return (val,fn)
    Just var' -> do
      obj <- valToObj val
      (result,internalUpdateFunction) <- lookupVarForObj var' obj
      let updateFunction modifiedValue = do
            newObject <- internalUpdateFunction modifiedValue
            fn (VObject newObject)
      return (result, updateFunction)
      
-- | Internal helper function for moving through a Var list.
lookupVarForObj :: Var -> Object -> EvalM (Value, Value-> EvalM Object)
lookupVarForObj incommingVar targetObject = do
  let ivarName = top incommingVar
  possibleValue <- lookupIVarsM ivarName targetObject
  case (possibleValue,bottom incommingVar) of
    (Nothing,Nothing)   -> return (VNil, (\_->return targetObject))
    (Just result, Nothing)   -> return (result, (\modifiedResult->insertIVar ivarName modifiedResult targetObject))
    (Just intermediateValue, Just outgoingVar) -> do
      intermediateObject <- valToObj intermediateValue
      (result,intermediateUpdateFunction) <- lookupVarForObj outgoingVar intermediateObject
      let updateFunction modifiedValue = do
            newObjectAtIvar <- intermediateUpdateFunction modifiedValue
            insertIVar ivarName (VObject newObjectAtIvar) targetObject 
      return (result, updateFunction)
    (Nothing, Just _)   -> throwError $ ivarName ++" not found"


-- | Get an instance variable in an object.  Represented in code as \@foo.  if the
--   IVar is not present returns @Nothing@.
lookupIVars :: String  -- the name to look for
            -> Object  -- the onject to look in
            -> Context -- the current context used only address remote objects
            -> IO (Maybe Value)
lookupIVars _ ROOT _ = return Nothing
lookupIVars s (Pid process) c = lookupIVarsRemote s process c
lookupIVars s obj _ = return $ lookupIVarsLocal s obj

-- | Get an instance variable in an object.  Represented in code as \@foo.  if the
--   IVar is not present returns @Nothing@. 
lookupIVarsM :: String -> Object -> EvalM (Maybe Value)
lookupIVarsM str obj = get >>= liftIO . lookupIVars str obj

-- | Internal function used to lookup IVars in PID's
lookupIVarsRemote :: String -> Process -> Context -> IO (Maybe Value)
lookupIVarsRemote s process context = do
  val <- dispatchC_ context process (SearchIVars s)
  case val of
    VError _ -> return Nothing
    val' -> return $ Just val'

-- | Internal function used to look up IVars in local objects
lookupIVarsLocal :: String -> Object -> Maybe Value
lookupIVarsLocal _ ROOT = Nothing
lookupIVarsLocal _ (Pid _) = error "lookupIVarsLocal called with remote object"
lookupIVarsLocal s obj = M.lookup s (ivars obj)

-- | Get the class value (a.k.a method) from the current object which must be a class.
--   If the name is not found in the Object then this function proceeds up the super
--   class chain until it either succeds or meets ROOT (ie mehtod missing).
lookupCVars :: String  -- name to find 
            -> Object  -- Object to look in
            -> Context -- Context to allow looking in remote objects
            -> IO (Maybe Value)
lookupCVars _ ROOT _ = return Nothing
lookupCVars s (Pid process) c = lookupCVarsRemote s process c
lookupCVars s obj@Class{} context = case lookupCVarsLocal s obj of
  Just val -> return $ Just val
  Nothing  -> lookupCVars s (super obj) context
lookupCVars _ Object{} _ = error "tried to find CVars in an object that was not a class"

-- | Get the class value (a.k.a method) from the current object which must be a class
--   If the name is not found in the Object then this function proceeds up the super
--   class chain until it either succeds or meets ROOT (ie mehtod missing).
lookupCVarsM str obj = get >>= liftIO . lookupCVars str obj

-- | Internal function for finding CVars in PIDs.
lookupCVarsRemote :: String -> Process -> Context -> IO (Maybe Value)
lookupCVarsRemote s process context = do
  val <- dispatchC_ context process (SearchCVars s) -- NOTE: Tail call done in responder
  case val of
    VError _ -> return Nothing
    val' -> return $ Just val'

-- | Internal function for finding CVars in local objects.
lookupCVarsLocal :: String -> Object -> Maybe Value
lookupCVarsLocal _ ROOT = Nothing
lookupCVarsLocal _ (Pid _) = error "lookupCVarsLocal called with remote object"
lookupCVarsLocal s obj = M.lookup s (cvars obj)

-- | Updates the value in the context pointed to by the LHS structure
--   
--   Note: Some cases not yet implimneted
insertLHS :: LHS -> Value -> EvalM ()
insertLHS (LIVar str) val = do
  context <-  get
  slf'  <- insertIVar str val (self context)
  put context{self=slf'}
insertLHS (LVar (Var str [])) val = modify $ insertLocals str val
insertLHS (LCVar str) val = do
  context <- get
  slf' <- insertCVar str val (self context)
  put context{self=slf'}
insertLHS _ _ = throwError "I dont know how to assign to that value"
-- TODO: Other LHS Cases here

-- | overwrite the IVar at the given name with the given value in the given
--   object and return the modified object 
insertIVar :: String -> Value -> Object -> EvalM Object
insertIVar _ _ ROOT  = throwError $ "Inserting into ROOT not allowed, (How the hell did you get your hands on ROOT?)"
insertIVar str val obj@(Pid p) = insertIVarRemote str val p >> return obj
insertIVar str val obj = return $ obj{ivars= (M.insert str val (ivars obj))}

insertIVarLocal str val obj = obj{ivars= (M.insert str val (ivars obj))}

insertIVarRemote str val p = sendM p (SetIVar str val)

-- | overwrite the CVar at the given name with the given value in the given
--   object and return the modified object
insertCVar :: String -> Value -> Object -> EvalM Object
insertCVar _ _ ROOT = fail "Inserting into ROOT not allowed, (How the hell did you get your hands on ROOT?)"
insertCVar str val obj@(Pid p) = insertCVarRemote str val p >> return obj
insertCVar str val obj@Class{} = return $ obj{cvars= (M.insert str val (cvars obj))}
insertCVar _ _ obj  = throwError "This Object is not a Class.  You cannot set a CVar in it"

insertCVarLocal str val obj@Class{} =  obj{cvars= (M.insert str val (cvars obj))}
insertCVarLocal _ _ _  = error "This Object is not a Class.  You cannot set a CVar in it"

insertCVarRemote str val p = sendM p (SetCVar str val)

-- | Find the current presedence table.
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



