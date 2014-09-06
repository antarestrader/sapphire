module Hash.BuiltinFunctions where

import Control.Monad.Error
import Data.HashMap.Strict as H
import qualified Data.Map as M
import Hash
import Object
import String
import Utils.Builtins
import Context (self, replyM_)
import {-# SOURCE #-} Eval
import AST

valueToHKey :: Value -> Maybe HKey
valueToHKey (VInt i)    = Just $ HKInt i
valueToHKey (VFloat f)  = Just $ HKFloat f
valueToHKey (VString s) = Just $ HKString (string s)
valueToHKey (VAtom s)   = Just $ HKAtom s
valueToHKey   _         = Nothing

valueToHKeyM :: Value -> EvalM HKey
valueToHKeyM VNil = throwError "Illigal value as key to a Hash."
valueToHKeyM val  = case valueToHKey val of
    Just hk -> return hk
    Nothing -> loop 12 val
  where
    loop 0 _ = throwError "Infinate loop when deriving hash key."
    loop i val = do
      v' <- eval (Call (EValue val) "to_hashable" [])
      case valueToHKey v' of
        Just hk -> return hk
        Nothing -> loop (i-1) v'

buildHashFromList :: [(Value,Value)] -> EvalM Hash
buildHashFromList kvs = do
    list <- mapM f kvs
    return $ Hash { h = H.fromList list, defaultValue = VNil }
  where
    f :: (Value,Value) -> EvalM (HKey,HValue)
    f (tk,tv) = do
      k <- valueToHKeyM tk
      let v = HValue {trueKey = tk, trueValue = tv}
      return (k,v) 

hashClass :: EvalM Object
hashClass = buildClass "Hash" bootstrap

bootstrap = M.fromList [
      ("to_sx", VFunction to_s (0,Just 0))
    ]

to_s :: [Value] -> EvalM ()
to_s _ = do
    (VHash (Hash{h=hash})) <- innerValue
    replyM_ $ VString $ mkStringLiteral $ format $ H.elems hash
  where
    format :: [HValue] -> String
    format [] = "{}"
    format hvs = "{" ++ concatMap fmtPair hvs ++ "}"
    fmtPair HValue{trueKey = tk, trueValue=tv} = show tk ++"=>"++show tv++", "
