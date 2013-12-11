module Context where

import qualified Data.Map as M
import Control.Monad
import Control.Concurrent.STM
import Object
import Object.Graph
import Var

data Context = Context 
               {locals :: M.Map String Value
               , self :: Object
               , continuation :: TMVar Value
               }

lookup :: Var -> Context -> IO (Maybe Value) --Check Local context
lookup Self c = return $ Just $ VObject $ self c
lookup var c = 
  case  M.lookup (top var) (locals c) of
    Nothing  -> cps $ search var (self c)
    Just val -> 
      case bottom var of 
        Nothing -> return $ Just val
	Just var' -> (atomically $ valToObj val) >>= cps . search var'  

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
