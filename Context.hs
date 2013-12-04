module Context where

import qualified Data.Map as M
import AST
import Object

emptyContext = M.empty :: Context

lookup :: Var -> Context -> Maybe Value
lookup (Var {name=s, scope=[]}) c = M.lookup s c
lookup _ _ = Nothing

insert :: Var -> Value -> Context -> Context
insert (Var {name=s, scope=[]}) val c = M.insert s val c

merge :: [(String,Value)] -> Context -> Context
merge params c = M.union (M.fromList params) c 

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
