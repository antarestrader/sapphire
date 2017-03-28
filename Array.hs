-- Array.hs Copyright 2014-2017 John F. Miller

module Array (
  module S
, Array
) where

import Data.Sequence as S -- from the `containers` library
import {-# SOURCE #-} Object

type Array = S.Seq Object


