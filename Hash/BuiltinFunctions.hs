module Hash.BuiltinFunctions where

import Control.Monad.Error
import Control.Monad.State
import qualified Data.HashMap.Strict as H
import qualified Data.Map as M
import qualified Array as A
import Hash
import Object
import Object.Graph
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
      ("keys", VFunction keys (0,Just 0))
    , ("values", VFunction values (0,Just 0))
    , ("indexed", VFunction indexed (1,Just 1))
    , ("indexAssign", VFunction indexAssign (2,Just 2))
    , ("to_list", VFunction to_list (0,Just 0))
    , ("insert" , VFunction insert (2,Just 2))
    ]

keys :: [Value] -> EvalM ()
keys _ = do
  (VHash (Hash{h=hash})) <- innerValue
  replyM_ $ VArray $ A.fromList $ map trueKey $ H.elems hash

values :: [Value] -> EvalM ()
values _ = do
  (VHash (Hash{h=hash})) <- innerValue
  replyM_ $ VArray $ A.fromList $ map trueValue $ H.elems hash

to_list  :: [Value] -> EvalM ()
to_list _ = do
    (VHash (Hash{h=hash})) <- innerValue
    replyM_ $ VArray $ A.fromList $ map format $ H.elems hash
  where
    format HValue{trueKey = tk, trueValue = tv} = VArray $ A.fromList [tk,tv]

indexed ::  [Value] -> EvalM ()
indexed [tk] = do
    (VHash (hash@Hash{h=hashmap, defaultValue=d})) <- innerValue
    k <- valueToHKeyM tk
    case (H.lookup k hashmap) of
      Just v -> replyM_ $ trueValue v
      Nothing -> case d of
        VFunction{} -> do
          tv <- eval (ApplyFn (EValue d) [EValue tk])
          replyM_ tv
          insertIVarM "__value" $ VHash $ hash{h=H.insert k (HValue{trueKey=tk, trueValue=tv}) hashmap}
        _ -> replyM_ d  

indexAssign :: [Value] -> EvalM ()
indexAssign [tk,tv] = do
  (VHash (hash@Hash{h=hashmap})) <- innerValue
  k <- valueToHKeyM tk
  insertIVarM "__value" $ VHash $ hash{h=H.insert k (HValue{trueKey=tk, trueValue=tv}) hashmap}
  replyM_ =<< fmap VObject (gets self)

-- | non-destructive assignment
insert :: [Value] -> EvalM ()
insert [tk,tv] = do
  (VHash (hash@Hash{h=hashmap})) <- innerValue
  k <- valueToHKeyM tk
  replyM_ $ VHash $ hash{h=H.insert k (HValue{trueKey=tk, trueValue=tv}) hashmap}
