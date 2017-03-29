-- Hash.hs Copyright 2015-17 by John F. Miller
{-# LANGUAGE DeriveGeneric #-}

module Object.Hash where

import qualified Data.HashMap.Strict as H --from unordered-containers
import Data.Hashable
import GHC.Generics (Generic)
import Data.List

import {-# SOURCE #-} Object
import Name



data Hash = Hash 
  { defaultValue :: Object
  , h   :: H.HashMap HKey HValue
  , hUID :: UID
  }

instance Show Hash where
  show Hash{h=hm} = "{" ++ inner ++ "}"
    where
      inner = intercalate ", " $ map show (H.elems hm)

data HKey = HKInt Integer
          | HKFloat Double
          | HKString String
          | HKAtom String
          | HKProcess String
          | HKTrue | HKFalse | HKNil
          | HKObject UID
          | HKFunction UID
          | HKHash UID
          | HKArray [HKey]
          | HKError HKey
          | HKErr String String
          | HKSpecial UID
          deriving (Eq, Ord, Show, Generic)

data HValue = HValue{trueValue :: Object, trueKey :: Object}

instance Show HValue where
  show (HValue{trueKey = tk, trueValue=tv}) = show tk ++"=>" ++ show tv

instance Hashable HKey 

