module Parser where

import Tokens
import Text.Parsec hiding (token)
import qualified Text.Parsec as P
import Text.Parsec.Pos

type TParser = Parsec [Token] ()
type Op = String

emptyPosition _ =  newPos "" 1 1

token :: (Token -> Maybe a) -> TParser a
token = P.token show emptyPosition

tokenEq :: Token -> TParser ()
tokenEq t = token (\t' -> if t == t' then Just () else Nothing)

keyword :: String -> TParser String
keyword s = token testTok <?> s
  where
    testTok (TKeyword s') = if s == s' then Just s else Nothing
    testTok _ = Nothing

var :: TParser Exp
var = token testTok
  where
    testTok (TVar s) = Just $ EVar (Var s "")
    testTok _ = Nothing

int ::  TParser Exp
int  = token testTok
  where
    testTok (TInt i) = Just $ EInt i
    testTok _ = Nothing

expr :: TParser Exp
expr = var <|> int <|> paren

expr1 :: TParser Exp
expr1 = do
  e0 <- expr
  opStr e0 <|> return e0


paren :: TParser Exp
paren = between (tokenEq TOpen) (tokenEq TClose) expr1

op :: TParser (Op,Exp)
op = do
    o <-  token testTok
    e <- expr
    return(o,e)
  where
    testTok (TOperator op) = Just op
    testTok _ = Nothing

opStr :: Exp -> TParser Exp
opStr e0 = do
    ops <- many1 op
    return $ OpStr e0 ops

data Exp = 
    EVar Var 
  | EInt Integer
  | EFloat Double
  | EString String -- TODO make this smarter
  | OpStr Exp [(Op,Exp)]
  | Call Exp String [Exp]
  | Send Exp String [Exp]
  | Index Exp [Exp]
  | Assign LHS Exp
  | OpAssign LHS Op Exp
  | If {predicate :: Exp,consequent :: Exp ,alternate :: Maybe Exp}
  | While Exp Exp
  | Until Exp Exp
  | Class Var (Maybe Var) Exp
  | Module Var Exp
  | Block [Exp]  deriving Show

data Var = Var {name :: String, scope :: String} deriving Show -- TODO: make scope its own thing

data LHS = 
    LVar Var
  | LIndex Exp [Exp]  -- Translate to <Exp>.call(`[]=` <Exp assignment>, [<Exp index>]
  | LCall Exp String
  | LSend Exp String deriving Show
