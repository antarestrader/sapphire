
{-# LANGUAGE  GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Test.Parameters 
  ( module Eval.Parameters
  , module Test.Parameters
  , module Name
  , module Object
  )
where

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map.Strict as M

import Name
import String
import Object hiding (State)
import Eval.Parameters
import Scope

newtype S a= S{ uns :: State (Namespace Object) a} 
  deriving (Functor, Applicative, Monad, MonadState (Namespace Object))

instance MonadError Object S where
  throwError s = error $ show s
  catchError = undefined

instance Scope S where
  readVar Local n = do
    m <- gets (M.lookup n)
    return $ fmap (\m ->MV m (\_->return ())) m
  readVar _ _ = error "Not included in test setup"
  setVar Local n o = do
    modify (M.insert n o)
  setVar _ _ _ = error "Not included in test setup"
  call _ "kind_of" [Prim (VString s)] = if (string s) == "FooClass"
    then return $ RO TrueClass
    else return $ RO FalseClass
  send = undefined
  tailCall = undefined
  spawn = undefined
  presidenceTable = undefined

run :: S a -> Namespace Object
run x = execState (uns x) (M.empty) 

runMatch p xs = run $ match p xs
