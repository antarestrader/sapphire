module Context where

import qualified Data.Map as M
import Control.Monad
import Control.Concurrent.MVar
import Object
import Var

data Context = Context 
               {locals :: M.Map String Value
               , self :: Either Pid Object
               , continuation :: MVar Value
               }

lookup :: Var -> Context -> Maybe Value
lookup (Var {name=s, scope=[]}) c = M.lookup s (locals c) 
lookup _ _ = Nothing

insert :: Var -> Value -> Context -> Context
insert (Var {name=s, scope=[]}) val c@Context {locals=l} =
  c{locals = M.insert s val l}

merge :: [(String,Value)] -> Context -> Context
merge params  c@Context {locals=l} = 
  c{locals = M.union (M.fromList params) l} 

precedence :: Context -> M.Map Op Precedence
precedence _ = -- TODO read from Context
  M.fromList [
      ("+",(6,L,N))
    , ("-",(6,L,N))
    , ("*",(7,L,N))
    , ("/",(7,L,N))
    , ("<",(4,N,N))
    , (">",(4,N,L))
    , ("==",(4,N,L))
  ]
