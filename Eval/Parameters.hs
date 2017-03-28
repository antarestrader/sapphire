{-# LANGUAGE OverloadedStrings #-}

module Eval.Parameters (
    Parameter(..)
  , match
  , arity
  , checkArity
  )
where

import  Control.Monad.Except
import Data.String
import Data.List

import Object hiding (arity)
import Scope
import Name

data Parameter = 
    P Name Parameter
  | Empty
  | Alternatives [Parameter]
  | Asterisk Name
  | Default Name Object Parameter
  | Pattern Name Name Parameter
  | Guard Fn Parameter

instance Show Parameter where
  show (Alternatives ps) = intercalate "\n" (map show ps)
  show (Guard _ p) = show p++" | <function>"
  show p = "(" ++ show' p
    where
      show' (P n Empty) = n ++ ")"
      show' (P n x) = n ++ ", " ++ show' x
      show' Empty = ")"
      show' (Asterisk n) = n++"*)"
      show' (Default n obj Empty) = n++"="++show obj++")"
      show' (Default n obj x) = n++"="++show obj++", " ++ show' x
      show' (Pattern cls n Empty) =cls++" "++n++")"
      show' (Pattern cls n x) =cls++" "++n++", " ++ show' x

match :: Scope m => Parameter -> [Object] -> m ()
match Empty [] = return ()
match Empty xs = throwError "PatternMatchError: Too many parameters"
match (P _ _) [] = throwError "PatternMatchError: Too few parameters"
match (P n next) (x:xs) = setLocal n x >> match next xs
match (Asterisk n) xs = setLocal n (varray xs) >> return ()
match (Default n _ next) (x:xs) = setLocal n x >> match next xs
match (Default n x next) [] = setLocal n x >> match next []
match (Pattern _ _ _) [] = throwError "PatternMatchError: Too few parameters"
match (Pattern cls n next) (x:xs) = do
  r <- call (Just $ RO x) "kind_of" [fromString cls]
  case (obj r) of 
     TrueClass -> setLocal n x >> match next xs
     otherwise -> throwError "PatternMatchError: Pattern failed" -- todo add information n is not a cls
match (Alternatives xs) ps = alternatives [] xs ps

arity :: Parameter -> Arity
arity ps = arity' (0,Just 0) ps
  where
    arity' x Empty = x
    arity' (a,_) (Asterisk _) = (a, Nothing)
    arity' (a, Just b) (Default _ _ x) = arity' (a,Just (b+1)) x
    arity' (a, Just b) (P _ x) = arity' (a+1, Just (b+1)) x
    arity' (a, Just b) (Pattern _ _ x) = arity' (a+1, Just (b+1)) x
    arity' x (Alternatives []) = x
    arity'  _ (Alternatives ps) = (a', b')
      where
        ps' = map (arity' (0,Just 0)) ps
        a' = minimum $ map fst ps'
        b' = foldl max' (Just 0) (map snd ps')
        max' Nothing _ = Nothing -- Nothing represents no limit (i.e. infinity) so it is the maximal value
        max' _ Nothing = Nothing
        max' (Just a) (Just b) = Just (max a b)
    arity' a (Guard _ x) = arity' a x

checkArity :: Arity -> Int -> Bool
checkArity (min, Just max) x | (min <= x) && (x <= max) = True
checkArity (min, Nothing)  x | (min <= x) = True
checkArity _ _ = False

setLocal ::Scope m => Name -> Object -> m ()
setLocal n x = setVar Local n x

alternitives :: Scope m => [Object] -> [Parameter] -> [Object] -> m ()
alternitives errs [] _ = throwError $ varray ("PatternMatchError: No matching pattern":errs)
alternatives [] [p] ps = match p ps
alternatives errs (x:xs) ps = match x ps `catchError` (\err -> alternatives (err:errs) xs ps)

