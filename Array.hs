module Array (
    module Object.Array
  , module S
  , varray
  , vEmptyArray
) where

import Data.Sequence as S

import Object.Array
import Object

varray :: [Object] -> Object
varray = Prim . VArray . S.fromList

vEmptyArray = Prim $ VArray S.empty 

