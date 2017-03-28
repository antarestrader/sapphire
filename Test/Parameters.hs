--Copyright 2017 by John F. Miller
{-# LANGUAGE  GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Test.Parameters 
  ( module Eval.Parameters
  , module Test.Parameters
  , module Name
  , module Object
  , quickCheck
  )
where

import Control.Monad.State
import Control.Monad.Except
import Data.String
import qualified Data.Map.Strict as M
import Test.QuickCheck

import Name
import String
import Object hiding (State)
import Eval.Parameters
import Scope
import qualified Array as A

newtype S a= S{ uns :: StateT (Namespace Object) (Except Object) a} 
  deriving (Functor, Applicative, Monad,
            MonadState (Namespace Object), MonadError Object)

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

run :: S a -> Either Object (Namespace Object)
run x = runExcept $ execStateT (uns x) (M.empty)

runMatch p xs = run $ match p xs

instance Arbitrary Object where
  arbitrary = Prim <$> arbitrary

instance Arbitrary Primitive where
   arbitrary = oneof [
       VInt <$> arbitrary
     , VString . fromString <$> arbitrary
     , VFloat <$> arbitrary
     , VAtom <$> arbitrary
     , VArray . A.fromList <$> arbitrary
     ]

prop_Empty xs =  --Empty only matches the empty list
   if (length xs) == 0
    then
      case r of
        Right _ -> True
        Left _  -> False
    else
      case r of
        Right _ -> False
        Left  _ -> True
  where r = runMatch Empty xs

prop_Asterisk xs = -- an Astrisk should match everything
  let r = runMatch (Asterisk "foo") xs in
    case r of 
      Left _ -> False
      Right m ->  maybe False (\_ -> True) (M.lookup "foo" m)

