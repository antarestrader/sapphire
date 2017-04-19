-- Copyright John F. Miller 2013-2017
{-# LANGUAGE OverloadedStrings,  NamedFieldPuns #-}
module Builtin.Class (classObject, classInit) where

import Data.Maybe

import Object
import Object.Runtime
import Scope (Scope(..),v,o, VariableContext( Local ), Value (..))
import Name
import Err
import Var
import Data.Map.Strict (empty, fromList)
import Control.Monad.State (get, put, modify)

classObject :: PID -> State
classObject objPid =  Class {
    ivars = empty
  , instanceOfClass = undefined
  , globalNamespace = objPid
  , localNamespace = objPid
  , localCache = empty
  , superClass = objPid
  , methods = bootstrap
  , methodCache = empty
  , modules = []
  , uid = undefined
  }

classInit :: Runtime()
classInit = do
  Pointer slf <- self
  uid <- nextUID
  modify (\st->st{instanceOfClass = slf, uid=uid})


bootstrap :: Namespace Fn
bootstrap = fromList [
         ("new"     , Fn new)
       , ("spawn"   , Fn spawnFn)
       ]

new vals = do
  obj <- newObject
  setVar Local "obj" obj
  val <- readVar Local "obj"
  call val "initialize" vals
  fromJust <$> readVar Local "obj" >>= reply . o

spawnFn args = do
  obj  <- newObject
  pid <- spawn obj
  send pid "initialize" args Nothing
  reply $ Process pid

-- todo write new and spawn for Class itself
newObject :: Scope m => m Object
newObject = do
  st <- get
  Pointer slf <- self
  uid <- nextUID
  return $ Object $ Instance {
      ivars = empty
    , instanceOfClass = slf
    , globalNamespace = globalNamespace st
    , localNamespace = localNamespace st
    , localCache = empty
    , primitive = Nothing
    , uid = uid
    }


{-
to_s [] = do
  (VObject Class{properName = s}) <- eval (EVar Self)
  replyM_ $ VString $ mkStringLiteral $ s

includeFn:: [Value] -> EvalM()
includeFn mdls = do
    val <- eval (EVar Self)
    case val of
      VObject obj@Class{} -> do
        obj' <- loop obj mdls
        modifySelf $ const obj'
        replyM_ VNil
      _ -> throwError $ Err  "SystemError" "Tried to include a module in on Objcet that was not a local class" [val]
  where
    loop obj [] = return obj
    loop obj (x:xs) = do
      o <- valToObj x
      loop (obj{cmodules = (o:(cmodules obj))}) xs

cmodulesFn :: [Value] -> EvalM()
cmodulesFn _ = do
  slf <- gets self
  replyM_ $ VArray $ fromList $ map VObject $ cmodules slf

instanceMethodsFn  :: [Value] -> EvalM()
instanceMethodsFn _ = do -- TODO: for true values move through inheritance chain
   slf <- gets self
   replyM_ $ VArray $ fromList $ map VAtom $ M.keys $ cvars slf
-}

