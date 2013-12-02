module Context where

import qualified Data.Map as M
import AST

type Context = M.Map String Value

emptyContext = M.empty :: Context

lookup :: Var -> Context -> Maybe Value
lookup (Var {name=s, scope=[]}) c = M.lookup s c
lookup _ _ = Nothing

insert :: Var -> Value -> Context -> Context
insert (Var {name=s, scope=[]}) val c = M.insert s val c

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
