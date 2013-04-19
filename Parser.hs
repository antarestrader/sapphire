module Parser where

import Tokens
import LineParser
import Control.Monad
import Data.Maybe
import Text.Parsec hiding (token)
import qualified Text.Parsec as P
import Text.Parsec.Pos

type TParser = Parsec [Token] ()
type Op = String

emptyPosition _ =  newPos "" 1 1

parseString :: String -> Either ParseError Exp
parseString s = parse expr "Input String" (alexScanTokens s)

token :: (Token -> Maybe a) -> TParser a
token = P.token show emptyPosition

tokenEq :: Token -> TParser Token
tokenEq t = token (\t' -> if t == t' then Just t else Nothing)

comma  = tokenEq TComma        <?> "',' (comma)"
open   = tokenEq TOpen         <?> "'('"
close  = tokenEq TClose        <?> "')'"
bopen  = tokenEq TBracket      <?> "'['"
bclose = tokenEq TBracketClose <?> "']'" 
nil    = keyword "nil" >> return ENil

foldP :: Stream s m t => a -> (a -> ParsecT s u m a) -> ParsecT s u m a
foldP x f = (f x >>= \x' -> foldP x' f) <|> (return x)   

andor :: Stream s m t => (e -> ParsecT s u m e) -> (e -> ParsecT s u m e) -> e -> ParsecT s u m e
andor pa pb e0 = do
  a <- optionMaybe (pa e0)
  b <- optionMaybe (pb (fromMaybe e0 a))
  maybe parserZero return (mplus b a)

prepend :: Stream s m t => ParsecT s u m a -> ParsecT s u m [a] -> ParsecT s u m [a]
prepend x xs = do
  r <- x
  rs <- option [] xs
  return (r:rs)

keyword :: String -> TParser String
keyword s = token testTok <?> s
  where
    testTok (TKeyword s') = if s == s' then Just s else Nothing
    testTok _ = Nothing

var' :: TParser String
var' =  token testTok <?> "legal identifier"
  where
    testTok (TVar s) = Just $ s
    testTok _ = Nothing

pscope :: [String] -> TParser Exp
pscope [] = pscope [""] -- global scope
pscope xs = do
  tokenEq TScope
  x <-  var'
  (pscope (x:xs)) <|> (return $ EVar $ Var x (reverse xs))

var :: TParser Exp
var = nil <|> do
  s <- var' 
  (pscope [s]) <|> (return $ EVar $ Var s []) <?> "Variable"


int ::  TParser Exp
int  = token testTok <?> "Integer"
  where
    testTok (TInt i) = Just $ EInt i
    testTok _ = Nothing

float :: TParser Exp
float = token testTok <?>  "Floating Point Number"
  where
    testTok (TFloat d) = Just $ EFloat d
    testTok _ = Nothing

expr0 :: TParser Exp
expr0 = var <|> (pscope []) <|> int <|> float <|> paren

expr1 :: TParser Exp 
expr1 = do
  e0 <- expr0
  foldP e0 (message `andor` args) 


expr1a :: TParser Exp
expr1a = do
  e0 <- expr0
  foldP e0 (message `andor` safeArgs)  

expr3 :: TParser Exp
expr3 = do
  e0 <- expr1
  opStr e0 <|> return e0

expr :: TParser Exp
expr = expr3 <?> "Sapphire Expression"

paren :: TParser Exp
paren = between open close expr

message :: Exp -> TParser Exp
message e0 = do
    t <- (tokenEq TDot <?> "'.' (call)") <|> (tokenEq TSend <?> "'->' (send)")
    s <- var'
    return $ (trans t) e0 s []
  where
    trans TDot = Call 
    trans TSend = Send


args' :: Bool -> Exp -> TParser Exp
args' safe e0 = do
      ps <- args'' safe
      return $ addArgs e0 ps 
  where
    addArgs :: Exp -> [Exp] -> Exp
    addArgs (Call e s []) ps = Call e s ps
    addArgs (Send e s []) ps = Send e s ps
    addArgs (EVar (Var s [])) ps = Call (EVar Self) s ps
    addArgs e ps = Call e "call" ps

    args'' s = (between open close (sepBy (expr <?> "argument") comma))  
       <|> if s then (sepBy1 (expr1a <?> "argument") comma) else (parserZero) 

indexes :: Exp -> TParser Exp
indexes e0 = do
    as <- (between bopen bclose (sepBy (expr <?> "index argument") comma))
    return $ addArgs e0 as
  where
    addArgs e' as = Call e' "[]" as

safeArgs :: Exp -> TParser Exp
safeArgs e = indexes e <|> args' False e

args :: Exp -> TParser Exp
args e = indexes e <|> args' True e

op :: TParser (Op,Exp)
op = do
    o <-  token testTok
    e <- expr1a
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
  | ENil
  | OpStr Exp [(Op,Exp)]
  | Call Exp String [Exp]
  | Send Exp String [Exp]
  | Assign LHS Exp
  | OpAssign LHS Op Exp
  | If {predicate :: Exp,consequent :: Exp ,alternate :: Maybe Exp}
  | While Exp Exp
  | Until Exp Exp
  | Class Var (Maybe Var) Exp
  | Module Var Exp
  | Block [Exp]  deriving Show

type Scope = [String]

data Var = Var {name :: String, scope :: Scope} | Self deriving Show -- TODO: make scope its own thing

data LHS = 
    LVar Var
  | LIndex Exp [Exp]  -- Translate to <Exp>.call(`[]=` <Exp assignment>, [<Exp index>]
  | LCall Exp String
  | LSend Exp String deriving Show
