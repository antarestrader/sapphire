{-# LANGUAGE DeriveGeneric #-}

module Hash where

import qualified Data.HashMap.Strict as H --from unordered-containers
import Data.Hashable
import GHC.Generics (Generic)

import {-# SOURCE #-} Object
import Data.List


data Hash = Hash {defaultValue :: Object, h :: H.HashMap HKey HValue}

instance Show Hash where
  show Hash{h=hm} = "{" ++ inner ++ "}"
    where
      inner = intercalate ", " $ map show (H.elems hm)

data HKey = HKInt Integer
          | HKFloat Double
          | HKString String
          | HKAtom String
          deriving (Eq, Ord, Show, Generic)

data HValue = HValue{trueValue :: Object, trueKey :: Object}

instance Show HValue where
  show (HValue{trueKey = tk, trueValue=tv}) = show tk ++"=>" ++ show tv

instance Hashable HKey 

