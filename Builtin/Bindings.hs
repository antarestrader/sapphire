module Builtin.Bindings
  (initialize)
where

import Data.Map.Strict (empty, fromList)

import Scope
import Object
import Object.Runtime

import qualified Runtime as R hiding (Runtime)

{-
import Builtin.Module
import Builtin.Atom
import Builtin.Array
import Builtin.Hash
import Builtin.Integer
import Builtin.Real
import Builtin.Bool
import Builtin.Directory
import Builtin.String
import Builtin.Error
-}

initialize :: Runtime Response -> Runtime (Value Runtime)
initialize prgm = do
  -- init modules here
  slf <- self 
  uid <- nextUID
  ss <- R.getState
  mainPid <- R.spawn ss{
      objectState =Instance {
          ivars = fromList [("test", Prim (VInt 5))]
        , instanceOfClass = slf
        , globalNamespace = slf
        , localNamespace = slf
        , localCache = empty
        , primitive = Nothing
        , uid = uid
        }
    , localScope = [empty]
    }
    (mainProcess prgm)
  return $ Pointer mainPid
  
