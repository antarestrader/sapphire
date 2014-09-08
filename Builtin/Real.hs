module Builtin.Real where

import Builtin.Utils
import Context
import Object
import String
import Control.Monad.Error
import qualified Data.Map as M

realClass = buildClass "Real" bootstrap

bootstrap = M.fromList [
    ("+", VFunction add (1,Just 2))
  , ("-", VFunction sub (1,Just 2))
  , ("*", VFunction mult (1,Just 1))
  , ("/", VFunction sapDiv (1,Just 1))
  ]

add ((VFloat b):_) = do
  (VFloat a) <- innerValue
  replyM_ $ VFloat (a+b)
add ((VInt b):_) = do
  (VFloat a) <- innerValue
  replyM_ $ VFloat (a + fromIntegral b)
add [val] = do
  slf <- innerValue
  callT val "+" [slf,VTrue]
add (val:_) = do
  slf <- innerValue
  (VString ss) <- call val "to_s" []
  throwError $ "Unable to add "++(show slf)++" and "++(string ss)

sub ((VFloat b):[]) = do
  (VFloat a) <- innerValue
  replyM_ $ VFloat (a-b)
sub ((VInt b):[]) = do
  (VFloat a) <- innerValue
  replyM_ $ VFloat (a-fromIntegral b)
sub ((VFloat b):_) = do
  (VFloat a) <- innerValue
  replyM_ $ VFloat (b-a)
sub ((VInt b):_) = do
  (VFloat a) <- innerValue
  replyM_ $ VFloat (fromIntegral b-a)
sub [val] = do
  slf <- innerValue
  callT val "-" [slf,VTrue]
sub (val:_) = do
  slf <- innerValue
  (VString ss) <- call val "to_s" []
  throwError $ "Unable to subtract "++(show slf)++" and "++(string ss)

mult ((VFloat b):_) = do
  (VFloat a) <- innerValue
  replyM_ $ VFloat (a*b)
mult ((VInt b):_) = do
  (VFloat a) <- innerValue
  replyM_ $ VFloat (a * fromIntegral b)
mult [val] = do
  slf <- innerValue
  callT val "*" [slf,VTrue]
mult (val:_) = do
  slf <- innerValue
  (VString ss) <- call val "to_s" []
  throwError $ "Unable to multiply "++(show slf)++" and "++(string ss)

sapDiv ((VFloat b):[]) = do
  (VFloat a) <- innerValue
  replyM_ $ VFloat (a/b)
sapDiv ((VInt b):[]) = do
  (VFloat a) <- innerValue
  replyM_ $ VFloat (a/fromIntegral b)
sapDiv ((VFloat b):_) = do
  (VFloat a) <- innerValue
  replyM_ $ VFloat (b/a)
sapDiv ((VInt b):_) = do
  (VFloat a) <- innerValue
  replyM_ $ VFloat (fromIntegral b/a)
sapDiv [val] = do
  slf <- innerValue
  callT val "/" [slf,VTrue]
sapDiv (val:_) = do
  slf <- innerValue
  (VString ss) <- call val "to_s" []
  throwError $ "Unable to divide "++(show slf)++" and "++(string ss)
