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

     MutableObject(..)
  ,  MutableValue(..)
  -- * Functions for Finding Special Values
  ,  valToObj
  , precedence

  -- * Direct
  , directLocals
  , directIVars
  , directCVars

  -- * Remote
  , remoteIVars
  , remoteCVars
  , remoteObject
  , remoteClass

  -- * Search
  , searchLocal
  , searchObject
  , searchClass
  , searchModules
  , searchCVars
  , searchIVars

  -- * Lookup
  , lookupVar
  , lookupSelf
  , lookupIVar

  -- * Inserts
  , insertIVars
  , insertCVars
  , insertLHS
  )
where

import Context
import Object
import {-# SOURCE #-} Eval
import {-# SOURCE #-} Builtin.Bindings
import Var
import AST


import Control.Monad.Error.Class
import Control.Monad
import Control.Monad.Trans.Maybe
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
valToObj val = bindPrimitiveObject val

-- | Find the current presedence table.
precedence :: Context -> M.Map Op Precedence
precedence _ = -- TODO read from Context
  M.fromList [
      ("+",(9,L,L))
    , ("-",(9,L,L))
    , ("*",(10,L,L))
    , ("/",(10,L,L))
    , ("%",(10,L,L))
    , ("<",(7,N,L))
    , (">",(7,N,L))
    , (">=",(7,N,L))
    , ("<=",(7,N,L))
    , ("==",(6,N,L))
    , ("!=",(6,N,L))
    , ("&&",(2,L,L))
    , ("||",(1,L,L))
  ]

directLocals :: String -> Context -> Maybe Value
directLocals str = lookupLocals str

directIVars :: String -> Object -> Maybe Value
directIVars _ ROOT = Nothing
directIVars _ (Pid _) = error "Cannot call a direct function (directIVars) on a remote object"
directIVars str obj = M.lookup str $ ivars obj

directCVars :: String -> Object -> Maybe Value
directCVars str Class{cvars=cvars} = M.lookup str cvars
directCVars _ ROOT = Nothing
directCVars _ (Pid _) = error "Cannot call a direct function (directcVars) on a remote object"
directCVars _ (Object{}) = error "Cannot find CVars on a (non-class) Object"

-- Remote

remoteIVars :: String -> Process -> EvalM Response
remoteIVars str p = dispatchM p (Search IVars str)

remoteCVars :: String -> Process -> EvalM Response
remoteCVars str p = dispatchM p (Search CVars str)

remoteObject :: String -> Process -> EvalM Response
remoteObject str p = dispatchM p (Search ObjectGraph str)

remoteClass :: String -> Process -> EvalM Response
remoteClass str p = dispatchM p (Search ClassGraph str)

-- search

data MutableObject = MO ((Value -> Object -> Object) -> Value -> EvalM ()) Object

moself = MO (\f val' -> modifySelf (f val'))

data MutableValue = MV (Value -> EvalM()) Value

mvnil =  MV (\_-> return ()) VNil

mverror = MV (\_ -> throwError "Attempted to mutate a Class variable")

searchLocal :: String -> EvalM(Maybe MutableValue)
searchLocal str = runMaybeT $ msum $ [a,b]
  where a = fmap (MV $ insertLocalM str) $ MaybeT $ gets $ directLocals str
        b = MaybeT $ do
                  s <- gets self
                  searchObject str (moself s)
                  

searchObject :: String -> MutableObject -> EvalM(Maybe MutableValue)
searchObject _ (MO _ ROOT) = return Nothing
searchObject str (MO _ (Pid p)) = do
  r <- responseToMaybe $ remoteObject str p
  return $ fmap (MV undefined ) r
searchObject str (MO update obj) = runMaybeT $ msum $ map MaybeT [a,b,c]
  where a = return $ fmap (MV $ updateIvars str update) $ directIVars str obj
        b = searchModules str (modules obj) >>= return . fmap mverror
        c = searchClass str (klass obj)  >>= return . fmap mverror

searchClass :: String -> Object -> EvalM(Maybe Value)
searchClass _ ROOT = return Nothing
searchClass str (Pid p) = responseToMaybe $ remoteClass str p
searchClass str Object{} = throwError "Tried to find class methods on an object that was not a class."
searchClass str cls@Class{} = runMaybeT $ msum $ map MaybeT [a,b,c]
  where a = return $ directCVars str cls
        b = searchModules str (cmodules cls) 
        c = searchClass str (super cls)

searchModules :: String ->[Object] -> EvalM(Maybe Value)
searchModules str ms = runMaybeT $ msum $ map (MaybeT . searchCVars str) ms

searchCVars :: String -> Object -> EvalM(Maybe Value)
searchCVars str ROOT = return Nothing
searchCVars str Object{} = throwError "Tried to find class methods on an object that was not a class."
searchCVars str (Pid p) = responseToMaybe $ remoteCVars str p
searchCVar str cls@Class{} = return $ directCVars str cls

searchIVars :: String -> MutableObject -> EvalM(Maybe MutableValue)
searchIVars str (MO _ ROOT) = return Nothing
searchIVars str (MO _ (Pid p)) =do
  r <-responseToMaybe $ remoteIVars str p
  return $ fmap (MV (\val' -> sendM p (SetIVar str val') >> return ())) r
searchIVars str (MO update obj)  = return $ fmap (MV $updateIvars str update) $ directIVars str obj

updateIvars str alt = alt (\val obj'->insertIVars str val obj') -- Late at night, find bugs here

responseToMaybe :: EvalM Response -> EvalM(Maybe Value)
responseToMaybe action = do 
  r <- action
  case r of
    (Response value) -> return $ Just value
    (NothingFound)   -> return Nothing
    (Error str)      -> throwError str

-- Lookup

lookupVar :: Var -> EvalM(MutableValue)
lookupVar Self = do
  slf <- gets self
  return $ MV (\(VObject obj) -> modifySelf (const obj)) (VObject slf)
lookupVar var = do
    let str = top var
    -- TODO: handle case when var is top level (eg ::Foo::Bar)
    r <- searchLocal str
    case (r,bottom var) of
      (Just v,  Nothing ) -> return v
      (Nothing, Nothing ) -> return $ MV (insertLocalM str) VNil
      (Nothing, Just _  ) -> throwError $ str ++ " not found"
      (Just v, Just var') -> lookupVarIn var' v
  where
    lookupVarIn :: Var -> MutableValue -> EvalM(MutableValue)
    lookupVarIn var v = do
      let str = top var
      obj <-  mutableValueToMutableObject v
      r <- searchIVars str obj
      case (r,bottom var) of 
        (Just v',  Nothing ) -> return v'
        (Nothing, Nothing ) -> return mvnil
        (Nothing, Just _  ) -> throwError $ str ++ " not found"
        (Just v', Just var') -> lookupVarIn var' v'

mutableValueToMutableObject :: MutableValue -> EvalM (MutableObject)
mutableValueToMutableObject (MV _ (VObject (Pid p))) = return $ MO undefined (Pid p) -- Should be OK
mutableValueToMutableObject (MV f val) = (MO undefined) `fmap` (valToObj val) --TODO: create function

lookupSelf :: String -> EvalM(MutableValue)
lookupSelf str= do
  slf <- gets self
  fmap (maybe mvnil id)  $ searchObject str (moself slf)

lookupIVar :: String -> EvalM(MutableValue)
lookupIVar str = do 
  slf <- gets self
  case directIVars str slf of 
    Nothing -> return $ MV (\val' -> modifySelf $ insertIVars str val') VNil
    Just v -> return $ MV (\val' -> modifySelf $ insertIVars str val') v

insertIVars :: String -> Value -> Object -> Object
insertIVars str val obj = obj{ivars = M.insert str val (ivars obj)}

insertIVarSelf :: String -> Value -> EvalM()
insertIVarSelf str val = modifySelf $ insertIVars str val

insertCVars :: String -> Value -> Object -> Object
insertCVars str val obj = obj{cvars = M.insert str val (cvars obj)}

insertLHS :: LHS -> Value -> EvalM()
insertLHS (LVar var) val = do
  (MV f _) <- lookupVar var
  f val
insertLHS (LIVar str) val = insertIVarSelf str val
insertLHS (LCVar str) val = modifySelf $ insertCVars str val
insertLHS (LIndex exp args) val = evalT (Call exp "[]=" (args ++ [EValue val]))
insertLHS (LCall exp str) val = evalT (Call exp (str++"=") [EValue val])
insertLHS (LSend exp str) val = evalT (Send exp (str++"=") [EValue val])
