-- Array.hs Copyright 2014-2017 John F. Miller

module Object.Array (
  Array
) where

import Data.Sequence as S -- from the `containers` library
import {-# SOURCE #-} Object

type Array = S.Seq Object


