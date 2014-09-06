module Hash where

import qualified Data.HashMap.Strict as H --from unordered-containers
import Data.Hashable
import {-# SOURCE #-} Object
import Data.List


data Hash = Hash {defaultValue :: Value, h :: H.HashMap HKey HValue}

instance Show Hash where
  show Hash{h=hm} = "{" ++ inner ++ "}"
    where
      inner = intercalate ", " $ map show (H.elems hm)

data HKey = HKInt Integer
          | HKFloat Double
          | HKString String
          | HKAtom String
          deriving (Eq, Ord, Show)

data HValue = HValue{trueValue :: Value, trueKey :: Value}

instance Show HValue where
  show (HValue{trueKey = tk, trueValue=tv}) = show tk ++"=>" ++ show tv

instance Hashable HKey where
  hash (HKInt i) = hashWithSalt 1337 i
  hash (HKFloat f) = hash f
  hash (HKString s) = hashWithSalt 1337 s
  hash (HKAtom s) = hash s

