module Builtin.Integer where

import Builtin.Utils
import Err
import Context
import Object
import String
import Control.Monad.Except
import qualified Data.Map as M

integerClass = buildClass "Integer" bootstrap

bootstrap = M.fromList [
    ("+", VFunction add (1,Just 2))
  , ("-", VFunction sub (1,Just 2))
  , ("*", VFunction mult (1,Just 2))
  , ("/", VFunction sapDiv (1,Just 2))
  , ("<", VFunction lt (1,Just 2))
  , (">", VFunction gt (1,Just 2))
  , ("<=", VFunction lte (1,Just 2))
  , (">=", VFunction gte (1,Just 2))
  , ("==", VFunction eq (1,Just 2))
  ]

add ((VInt b):_) = do
  (VInt a) <- innerValue
  replyM_ $ VInt (a+b)
add ((VFloat b):_) = do
  (VInt a) <- innerValue
  replyM_ $ VFloat (fromIntegral a+b)
add [val] = do
  slf <- innerValue
  callT val "+" [slf,VTrue]
add (val:_) = do
  slf <- innerValue
  (VString ss) <- call val "to_s" []
  throwError $ Err "NumericError" ("Unable to add "++(show slf)++" and "++(string ss)) [slf,val]

sub ((VInt b):[]) = do
  (VInt a) <- innerValue
  replyM_ $ VInt (a-b)
sub ((VFloat b):[]) = do
  (VInt a) <- innerValue
  replyM_ $ VFloat (fromIntegral a-b)
sub ((VInt b):_) = do
  (VInt a) <- innerValue
  replyM_ $ VInt (b-a)
sub ((VFloat b):_) = do
  (VInt a) <- innerValue
  replyM_ $ VFloat (b-fromIntegral a)
sub [val] = do
  slf <- innerValue
  callT val "-" [slf,VTrue]
sub (val:_) = do
  slf <- innerValue
  (VString ss) <- call val "to_s" []
  throwError $ Err "NumericError" ("Unable to subtract "++(show slf)++" and "++(string ss)) [slf,val]

mult ((VInt b):_) = do
  (VInt a) <- innerValue
  replyM_ $ VInt (a*b)
mult ((VFloat b):_) = do
  (VInt a) <- innerValue
  replyM_ $ VFloat (fromIntegral a*b)
mult [val] = do
  slf <- innerValue
  callT val "*" [slf,VTrue]
mult (val:_) = do
  slf <- innerValue
  (VString ss) <- call val "to_s" []
  throwError $ Err "NumericError" ("Unable to multiply "++(show slf)++" and "++(string ss)) [slf,val]

sapDiv ((VInt b):[]) = do
  (VInt a) <- innerValue
  replyM_ $ VInt (a `div` b)
sapDiv ((VFloat b):[]) = do
  (VInt a) <- innerValue
  replyM_ $ VFloat (fromIntegral a/b)
sapDiv ((VInt b):_) = do
  (VInt a) <- innerValue
  replyM_ $ VInt (b `div` a)
sapDiv ((VFloat b):_) = do
  (VInt a) <- innerValue
  replyM_ $ VFloat (b/fromIntegral a)
sapDiv [val] = do
  slf <- innerValue
  callT val "/" [slf,VTrue]
sapDiv (val:_) = do
  slf <- innerValue
  (VString ss) <- call val "to_s" []
  throwError $ Err "NumericError" ("Unable to divide "++(show slf)++" and "++(string ss)) [slf,val]

lt ((VInt b):[]) = do
  (VInt a) <- innerValue
  replyM_ $ mkBool (a<b)
lt ((VFloat b):[]) = do
  (VInt a) <- innerValue
  replyM_ $ mkBool (fromIntegral a<b)
lt ((VInt b):_) = do
  (VInt a) <- innerValue
  replyM_ $ mkBool (b<a)
lt ((VFloat b):_) = do
  (VInt a) <- innerValue
  replyM_ $ mkBool (b<fromIntegral a)
lt [val] = do
  slf <- innerValue
  callT val "<" [slf,VTrue]
lt (val:_) = do
  slf <- innerValue
  (VString ss) <- call val "to_s" []
  throwError $ Err "NumericError" ("Unable to compare "++(show slf)++" and "++(string ss)) [slf,val]

gt ((VInt b):[]) = do
  (VInt a) <- innerValue
  replyM_ $ mkBool (a>b)
gt ((VFloat b):[]) = do
  (VInt a) <- innerValue
  replyM_ $ mkBool (fromIntegral a>b)
gt ((VInt b):_) = do
  (VInt a) <- innerValue
  replyM_ $ mkBool (b>a)
gt ((VFloat b):_) = do
  (VInt a) <- innerValue
  replyM_ $ mkBool (b>fromIntegral a)
gt [val] = do
  slf <- innerValue
  callT val ">" [slf,VTrue]
gt (val:_) = do
  slf <- innerValue
  (VString ss) <- call val "to_s" []
  throwError $ Err "NumericError" ("Unable to compare "++(show slf)++" and "++(string ss)) [slf,val]

gte ((VInt b):[]) = do
  (VInt a) <- innerValue
  replyM_ $ mkBool (a>=b)
gte ((VFloat b):[]) = do
  (VInt a) <- innerValue
  replyM_ $ mkBool (fromIntegral a>=b)
gte ((VInt b):_) = do
  (VInt a) <- innerValue
  replyM_ $ mkBool (b>=a)
gte ((VFloat b):_) = do
  (VInt a) <- innerValue
  replyM_ $ mkBool (b>=fromIntegral a)
gte [val] = do
  slf <- innerValue
  callT val ">=" [slf,VTrue]
gte (val:_) = do
  slf <- innerValue
  (VString ss) <- call val "to_s" []
  throwError $ Err "NumericError" ("Unable to compare "++(show slf)++" and "++(string ss)) [slf,val]

lte ((VInt b):[]) = do
  (VInt a) <- innerValue
  replyM_ $ mkBool (a<=b)
lte ((VFloat b):[]) = do
  (VInt a) <- innerValue
  replyM_ $ mkBool (fromIntegral a<=b)
lte ((VInt b):_) = do
  (VInt a) <- innerValue
  replyM_ $ mkBool (b<=a)
lte ((VFloat b):_) = do
  (VInt a) <- innerValue
  replyM_ $ mkBool (b<=fromIntegral a)
lte [val] = do
  slf <- innerValue
  callT val "<=" [slf,VTrue]
lte (val:_) = do
  slf <- innerValue
  (VString ss) <- call val "to_s" []
  throwError $ Err "NumericError" ("Unable to compare "++(show slf)++" and "++(string ss)) [slf,val]

eq ((VInt b):_) = do
  (VInt a) <- innerValue
  replyM_ $ mkBool (a==b)
eq ((VFloat b):_) = do
  (VInt a) <- innerValue
  replyM_ $ mkBool (fromIntegral a==b)
eq [val] = do
  slf <- innerValue
  callT val "==" [slf,VTrue]
eq (val:_) = do
  replyM_ VFalse
