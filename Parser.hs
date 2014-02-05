module Parser where

import Tokens
import LineParser (parseCode, filename)
import AST
import Var
import Control.Monad
import Control.Monad.Error.Class
import Data.Maybe
import Text.Parsec hiding (token, string)
import qualified Text.Parsec as P
import Text.Parsec.Pos

type TParser = Parsec [Token] Bool

position t = newPos (tfile t) (fromIntegral $ tline t) (fromIntegral $ toffset t)

runParser' p s fp xs = case runParser p s fp xs of
  Left err -> Left $ show err
  Right es -> Right es

parseString :: String -> Either String [Exp]
parseString s = do
  tokens <- scanBlock $ parseCode "Input String" s
  runParser' exprs False "Input String" tokens

parseFile :: FilePath -> IO (Either String [Exp])
parseFile f = do --IO Monad
  source <- readFile f
  return $ do -- Either Monad
    tokens <- scanBlock $ parseCode f source
    runParser' exprs False f tokens

tokenP :: (Token -> Maybe a) -> TParser a
tokenP = P.token show position

tokenEq :: T -> TParser Token
tokenEq t = tokenP (\t' -> if t == (token t') then Just t' else Nothing)

comma  = tokenEq TComma        <?> "',' (comma)"
open   = tokenEq TOpen         <?> "'('"
close  = tokenEq TClose        <?> "')'"
bopen  = tokenEq TBracket      <?> "'['"
bclose = tokenEq TBracketClose <?> "']'"
copen  = tokenEq TBrace        <?> "'{'"
cclose = tokenEq TBraceClose   <?> "'}'"
tdot   = tokenEq TDot          <?> "'.' (Call)"
tsend  = tokenEq TSend         <?> "'->' (Send)"
assignP= tokenEq TAssign       <?> "'=' (Assignment Operator)"
nil    = keyword "nil"   >> return ENil
falseP = keyword "false" >> return EFalse
trueP  = keyword "true"  >> return ETrue 
selfP  = keyword "self"  >> return Self <?> "self"
tend   = tokenEq TEnd >> return () <?> "End of Line"
tsuper = tokenEq TSuper         <?> "<-"
tstartI = tokenEq StartInterp
tendI   = tokenEq EndInterp

block :: TParser Exp
block  = do
  let tok Token {token = (TBlock b)} = Just b
      tok _ = Nothing
  blk <- tokenP tok
  putState True
  let file = filename $ blk
  let result = do 
        tokens <- scanBlock blk
        runParser' exprs False file tokens 
  case result of 
    Left p -> parserFail p  -- check here for problems with large nested error messages
    Right exps ->  return $ Block exps

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

interp :: TParser Exp
interp  = between tstartI tendI expr

exString :: TParser Exp
exString = do 
    x <- string
    xs <- many (interp <|> string)
    if (null xs) then return x else return $ ExString (x:xs)

ivar :: TParser Exp
ivar = tokenP tok <?> "instance variable"
  where
    tok Token {token=TIVar s} = Just $ EIVar s
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

paramList = between open close $ sepBy identifier comma

identifier :: TParser String
identifier = tokenP tok
  where
    tok Token {token=TVar v} = Just v
    tok _ = Nothing

var :: TParser Var
var = do 
    let pscope xs = do
          tokenEq TScope
          x <- identifier
          (pscope (x:xs)) <|> (return $ Var x (reverse xs))
    let local = do
          v <- identifier
          pscope [v] <|> (return $ Var v [])
    let global = pscope [""]
    selfP <|> local <|> global <?> "variable expression"

args var =
  (argumentList >>= (\args->return (Apply var args))) <|> return (EVar var)

op :: TParser (Op,Exp)
op = do
    o <-  tokenP testTok <?> "operator symbol"
    e <- expr'
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
transLHS (EVar v) = return $ LVar v --TODO add indexed, called, and sent here
transLHS (EIVar s) = return $ LIVar s
transLHS _ = fail "illigal Left Hand Side of assignment expression"

ifParser :: TParser Exp
ifParser = do
  let local = do
        keyword "then"
        cons <- expr
        alt  <- optionMaybe (keyword "else" >> expr)
        return (cons,alt)
  keyword "if"
  pred <- expr
  (cons, alt) <- local <|> ifBlock
  return $ If pred cons alt

ifBlock = do
  let tok Token {token = (TBlock b)} = Just b
      tok _ = Nothing
  blk <- tokenP tok
  putState True
  let file = filename blk
  let result = do
        tokens <- scanBlock blk
        runParser' ifBlock' False file tokens
  case result of 
    Left p  -> parserFail p -- TODO Propagate error
    Right x -> return x

ifBlock' :: TParser (Exp, Maybe Exp)
ifBlock' = nakedThenBlock <|> thenLine <|> simpleBlock
  where
    nakedThenBlock = do
      cons <- block
      alt <- elseParse
      return (cons,alt)
    thenLine = do
      cons <- keyword "then" >>  (block <|> termExpr)
      alt  <- elseParse
      return (cons, alt)
    simpleBlock = do
      exps <- exprs
      return (Block exps, Nothing)
    elseParse = optionMaybe (keyword "else" >> (block <|> termExpr))

classParser :: TParser Exp
classParser = do
  keyword "class"
  n <- var
  s <- optionMaybe $ tsuper >> var
  exp <- block <|> expr
  return $ EClass n s exp

defParser :: TParser Exp
defParser = do
  keyword "def"
  n <- identifier
  ps <- option [] $ paramList
  exp <- block
  return $ Def n ps exp

lambda :: TParser Exp
lambda = do
  let single = do
        tsend <?> "End of params marker (->)"
        expr
  keyword "lambda"
  params <- option [] paramList
  exp <- block <|> single
  return $ Lambda params exp
  
indexed exp = do
  idx <- between bopen bclose expr <?> "index expression ([...])"
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
expr0 = paren 
     <|> nil <|> falseP <|> trueP 
     <|> (var >>= args) <|> ivar  <|> atom   <|> float 
     <|> int <|> exString <?> "basic expression unit"

extension :: Exp -> TParser Exp
extension exp = opStr exp <|> assign exp <|> indexed exp <|> called exp <|> sent exp

expr1 :: TParser Exp
expr1 = do
    exp <- expr0
    extend exp
  where
    extend exp = (extension exp >>= extend) <|> return exp


statement = lambda <|> ifParser <|> defParser <|> classParser

expr = statement <|> expr1

termExpr = (setState False) >> (statement <|> term)
  where
    term = do
      exp <- expr1
      blk <- getState
      when (not blk) tend
      return exp

tmeta = tokenP tok <?> "Comment/pragma"
  where
    tok Token{token=TMeta s} = Just s
    tok _ = Nothing

meta = do
  tmeta  
  (tend >> return ()) <|> (block >> return ())

many1Ignore :: Stream s m t => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m [a]
many1Ignore pa pb = do
  many pb
  many1 $ do
    a <- pa
    many pb
    return a

exprs = many1Ignore termExpr meta

expr' = statement <|> expr1a
  where
    expr1a = expr0 >>= extend
    extend exp = (extension' exp >>= extend) <|> return exp
    extension' exp = assign exp <|> indexed exp <|> called exp <|> sent exp
