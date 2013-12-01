module Parser where

import Tokens
import LineParser
import AST
import Control.Monad
import Data.Maybe
import Text.Parsec hiding (token, string)
import qualified Text.Parsec as P
import Text.Parsec.Pos

type TParser = Parsec [Token] ()

emptyPosition _ =  newPos "" 1 1

parseString :: String -> Either ParseError Exp
parseString s = parse expr "Input String" $ scanBlock $ parseCode "Input String" s

tokenP :: (Token -> Maybe a) -> TParser a
tokenP = P.token show emptyPosition

tokenEq :: T -> TParser Token
tokenEq t = tokenP (\t' -> if t == (token t') then Just t' else Nothing)

comma  = tokenEq TComma        <?> "',' (comma)"
open   = tokenEq TOpen         <?> "'('"
close  = tokenEq TClose        <?> "')'"
bopen  = tokenEq TBracket      <?> "'['"
bclose = tokenEq TBracketClose <?> "']'"
tdot   = tokenEq TDot          <?> "'.' (Call)"
tsend  = tokenEq TSend         <?> "'->' (Send)"
assignP= tokenEq TAssign       <?> "'=' (Assignment Operator)"
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
keyword s = tokenP testTok <?> s
  where
    testTok Token{token=(TKeyword s')} = if s == s' then Just s else Nothing
    testTok _ = Nothing

int ::  TParser Exp
int  = tokenP testTok <?> "Integer"
  where
    testTok Token{token=(TInt i)} = Just $ EInt i
    testTok _ = Nothing

float :: TParser Exp
float = tokenP testTok <?>  "Floating Point Number"
  where
    testTok Token{token=(TFloat d)} = Just $ EFloat d
    testTok _ = Nothing

string :: TParser Exp
string = tokenP tok <?> "String literal"
  where
    tok Token {token=TString s} = Just $ EString s
    tok _ = Nothing

atom :: TParser Exp
atom = tokenP tok
  where
    tok Token {token=TAtom s} = Just $ EAtom s
    tok _ = Nothing

paren :: TParser Exp
paren = between open close expr

argumentList :: TParser [Exp]
argumentList = between open close $ sepBy expr comma

identifier :: TParser String
identifier = tokenP tok
  where
    tok Token {token=TVar v} = Just v
    tok _ = Nothing

var :: TParser Exp
var = do 
    let pscope xs = do
          tokenEq TScope
          x <- identifier
          (pscope (x:xs)) <|> (return $ Var x (reverse xs))
    let local = do
          v <- identifier
          pscope [v] <|> (return $ Var v [])
    let global = pscope [""]
    var <- local <|> global
    (argumentList >>= (\args->return (Apply var args))) <|> return (EVar var)

op :: TParser (Op,Exp)
op = do
    o <-  tokenP testTok
    e <- expr
    return(o,e)
  where
    testTok Token{ token=(TOperator op)} = Just op
    testTok _ = Nothing

opStr :: Exp -> TParser Exp
opStr e0 = do
    ops <- many1 op
    return $ OpStr e0 ops

assign :: Exp -> TParser Exp
assign lhs' = do
  assignP
  lhs <- transLHS lhs'
  rhs <- expr
  return $ Assign lhs rhs

transLHS :: Exp -> TParser LHS
transLHS (EVar v) = return $LVar v --TODO add indexed, called, and sent here
transLHS _ = fail "illigal Left Hand Side of assignment expression"
  
indexed exp = do
  idx <- between bopen bclose expr
  return $ Index exp idx

called exp = do
  tdot
  s <- identifier
  args <- option [] argumentList
  return $ Call exp s args

sent exp = do
  tsend
  s <- identifier
  args <- option [] argumentList
  return $ Send exp s args

expr0 :: TParser Exp
expr0 = paren <|> nil <|> var <|> atom <|> float <|> int <|> string <?> "basic expression unit"

extension :: Exp -> TParser Exp
extension exp = opStr exp <|> assign exp <|> indexed exp <|> called exp <|> sent exp

expr :: TParser Exp
expr = do
    exp <- expr0
    extend exp
  where
    extend exp = (extension exp >>= extend) <|> return exp

