module Builtin.String where

import String as S
import AST
import Object
import Eval
import Context
import Err
import Builtin.Utils
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import Control.Monad.Except
import Data.Monoid

stringClass = buildClassEx "String" bootstrap bootClass

bootstrap = M.fromList [
      ("length" , VFunction lengthFn  (0,Just 0))
    , ("=="     , VFunction eq        (1,Just 2))
    ]

bootClass = M.fromList [
      ("concat" , VFunction concatFn (1,Nothing))
    ]

lengthFn :: [Value] -> EvalM ()
lengthFn [] = do
  (VString s) <- innerValue
  replyM_ $ VInt $ stringLength s
lengthFN _ = throwError $ Err "RunTimeError" "String#Length does not take arguments" []

eq :: [Value] -> EvalM ()
eq ((VString t):_) = do
  (VString s) <- innerValue
  if ( s ==  t)
    then replyM_ VTrue
    else replyM_ VFalse
eq [val] = do
  slf <- innerValue
  callT val "==" [slf,VTrue]
eq (val:_) = replyM_ VFalse

concatFn :: [Value] -> EvalM ()
concatFn [] = replyM_ $ VString $  mkStringLiteral ""
concatFn (s@(VString _):[]) = replyM_ s
concatFn ((VString s):(VString t):vals) = concatFn ((VString (s `mappend` t)):vals)
concatFn (s@(VString _):t:vals) = do
  t' <- toString' t
  concatFn (s:t':vals)
concatFn (s:vals) = do
  s' <- toString' s
  concatFn (s':vals)

toString' :: Value -> EvalM Value
toString' val = loop val 12
  where
    loop s@(VString str) _ = return s
    loop _ 0 = throwError $ Err "RunTimeError" "Value refuses to be converted to a String" [val]
    loop val' n = do
      val'' <- eval (Call (EValue val') "to_s" [])
      loop val'' (n-1)

