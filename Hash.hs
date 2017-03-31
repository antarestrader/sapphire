{-# LANGUAGE OverloadedStrings,  NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns#-}
module Hash (
    module Object.Hash
  , vhash
  , insert
  , index
  , keys
  , values
  , toArray
  ) where

import Data.Foldable (toList)
import qualified Data.HashMap.Strict as H

import Object
import Object.Hash
import String
import Err
import qualified Array as A

hkey :: Object -> HKey
hkey (Prim (VInt i)) = HKInt i
hkey (Prim (VString ss)) = HKString $ string ss
hkey (Prim (VFloat f)) = HKFloat f
hkey (Prim (VAtom s)) = HKAtom s
hkey (Prim (VArray a)) = HKArray (map hkey (toList a))
hkey (Prim (VHash hash)) = HKHash $ hkey $ toArray hash
hkey (Process pid) = HKProcess $ show pid
hkey (TrueClass) = HKTrue
hkey (FalseClass) = HKFalse
hkey (Nil) = HKNil
hkey (VFunction{fUID}) = HKFunction fUID
hkey (Object obj) = HKObject (uid obj)
hkey (VError (ErrObj a)) = HKError (hkey a)
hkey (VError (Err a b _)) = HKErr a b
hkey o = case getUID o of 
  Just u  -> HKSpecial u
  Nothing -> HKSpecial 0

vhash :: [(Object,Object)] -> Object
vhash lst = Prim $ VHash $ Hash{defaultValue = Nil, h=hsh, hUID=0} 
  where
    hsh = H.fromList (map (\(k,v)-> mkKeyValue k v) lst)

insert :: Object -> Object -> Hash -> Hash
insert k v hsh@Hash{h} = hsh{h=h'}
  where
    h' = H.insert k' v' h
    (k',v') = mkKeyValue k v

index :: Object -> Hash -> Object
index k (Hash{defaultValue,h}) = case (H.lookup (hkey k) h) of
  Nothing -> defaultValue
  Just (HValue{trueValue}) -> trueValue

keys :: Hash -> Object
keys Hash{h} = A.varray $ map trueKey (H.elems h)

values :: Hash -> Object
values Hash{h} = A.varray $ map trueValue (H.elems h)

toArray :: Hash -> Object
toArray Hash{h} = A.varray $ map 
   (\(HValue{trueKey, trueValue}) -> A.varray [trueKey,trueValue])
   (H.elems h)

mkKeyValue :: Object -> Object -> (HKey, HValue)
mkKeyValue trueKey trueValue = (hkey trueKey, HValue{trueKey,trueValue})
