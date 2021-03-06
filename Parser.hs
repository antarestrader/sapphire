-- Parser.hs Copyright 2013,2014 by John F. Miller

-- | Sapphire parser using the Parsec monadic parser combinator library
--
-- The goal of the parser is to turn tokens from the lexer into expressions
-- as shown in the AST. The parser is build out of a large and expanding
-- number of sub parsers.  There are four rough categories: 1) elementry
-- parsers that transalte straight from Token to Exp, 2) basic sequence
-- parsers that grab a defined list of tokens and build a Exp from them, 3)
-- extending parsers that take simple expressions and use that tokens that
-- follow them to possibally create more complex expressions, 4)
-- Unifying parsers that merge several smaller expressions into a
-- single parser that well parse any of them.

-- In addation to the parsers there are some functions that facilitate
-- parsing of lines, files and input strings

module Parser (
    -- * Invoking a Parser

    -- | These function apply the complete parser different types of input
    runParser'
  , parseString
  , parseFile
    -- * Elementry

    -- | Parsers that identify control tokens, that is tokens without
    --   content.
  , comma, open, close, bopen, bclose, copen, cclose
  , tdot, tsend, assignP, tsuper, tend, tstartI, tendI
  , tmeta -- as currently implimented

    -- * basic parsers

    -- | These parsers return an expression translated from a single token.
  , nil, falseP, trueP, selfP
  , int, float,  ivar, atom
    -- ** Strings
  , string, exString
    -- ** Scoped Variable Varsers
  , identifier, var

    -- * Grouping parsers

    -- | These parsers form tree like structures
  , paren, interp

    -- ** Parameter and Argument lists
  , paramList, argumentList

    -- * Extending parsers

    -- | These parser take a previous expression and create a more complex
    --   expression out ot the subsiquent tokens.  The root expression is passes
    --   into the parser to be used in creating the AST node.
    --
    --   All parsers in this section have the type @Exp -> TParser Exp@
  , args, indexed, called, sent
    -- ** Operator Strings
  , opStr, op, exprForOpStr
    -- ** Assignment
  , assign, transLHS
    -- ** Extended Parser Union
  , extension

    -- * Syntax Level Parsers
  , lambda, ifParser, classParser, defParser
  , meta
    -- * Unifying Parsers

    -- | These parsers use @<|>@ to bring together parsers at a related
    --   level inorder to build up the complete saphire parser
  , expr0, expr1, statement, expr, termExpr, exprs

    -- * Combinators and utilities

  , TParser, tokenP, tokenEq, keyword, position, many1Ignore, foldP, andor, prepend
)where

import Control.Monad
import Control.Monad.Error.Class
import Data.Maybe
import Text.Parsec hiding (token, string)
import qualified Text.Parsec as P
import Text.Parsec.Pos

import Tokens
import LineParser (parseCode, filename, CodeBlock)
import AST
import Parameters
import Name
import Var

-- | Type synonym for the type of our parser
--
-- all our parsers take a stream of Tokens produced by the Lexer. The Bool
-- is a user state and is used to track parsing of Blocks at the end of a
-- line.
type TParser = Parsec [Token] Bool

position t = newPos (tfile t) (fromIntegral $ tline t) (fromIntegral $ toffset t)

-- | wrapper around the Parsec runParser method that returns a string on
--   errors to be compatible with the other error producing functions in
--   Sapphire.  If someday errors become more then simple strings this is
--   where ParseErrors need to be converted into Sapphire spesific errors.
runParser' p s fp xs = case runParser p s fp xs of
  Left err -> Left $ show err
  Right es -> Right es

-- | Used by the REPL to parse its input.
parseString :: String -> Either String [Exp]
parseString s = do
  tokens <- scanBlock $ parseCode "Input String" s
  runParser' exprs False "Input String" tokens

-- | parse an entire file.
parseFile :: FilePath -> IO (Either String [Exp])
parseFile f = do --IO Monad
  source <- readFile f
  return $ do -- Either Monad
    tokens <- scanBlock $ parseCode f source
    runParser' exprs False f tokens

-- | Helper method parsing simple tokens
tokenP :: (Token -> Maybe a) -> TParser a
tokenP = P.token show position

-- | Helper method for parsing basic tokens that do not produce expression
--   on their own.
tokenEq :: T -> TParser Token
tokenEq t = tokenP (\t' -> if t == (token t') then Just t' else Nothing)

comma  = tokenEq TComma        <?> "',' (comma)"
bang   = tokenEq TBang         <?> "'!' (bang)"
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
bar    = let tok t@Token {token = (TOperator "|")} = Just t
             tok _ = Nothing
          in tokenP tok  <?> "'|' (bar)"
star   = let tok t@Token {token = (TOperator "*")} = Just t
             tok _ = Nothing
          in tokenP tok  <?> "'*' (star)"
rocket = let tok t@Token {token = (TOperator "=>")} = Just t
             tok _ = Nothing
          in tokenP tok  <?> "'=>' (Hash Rocket)"
hashAtom = let tok Token {token = (THashAtom s)} = Just $ EAtom s
               tok _ = Nothing
            in tokenP tok  <?> "hash symbol (eg foo:)"

-- | If the next token is a block, treat it as Sapphire code and parse it
--   returning a 'Block' expression.
block :: TParser Exp
block  = do
  blk <- tblock
  let file = filename $ blk
  let result = do
        tokens <- scanBlock blk
        runParser' exprs False file tokens
  case result of
    Left p -> parserFail p  -- check here for problems with large nested error messages
    Right exps ->  return $ Block exps file

-- | Extract a block from the end of the line but leave it unevaluated as it
--   may not contain sapphire code (a comment for example)
tblock :: TParser CodeBlock
tblock = do {b <- tokenP tok; putState True; return b} -- True in state inhibits searching for TEnd
  where
    tok Token {token = (TBlock b)} = Just b
    tok _ = Nothing

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

-- | Match a keyword Token.  Note the return value is almost never used.
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

-- | A piece of sapphire code embedded in an extended string.
interp :: TParser Exp
interp  = between tstartI tendI expr

-- | An extended string consisting of TStrings and interp blocks
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

-- | An expression between parentheses
paren :: TParser Exp
paren = between open close expr

-- | The list of actual arguments (expression) in a function call
argumentList :: Bool -> TParser [Exp]
argumentList False = between open close $ sepBy expr comma
argumentList True = argumentList False <|> openList
  where
    openList = sepBy1 safeExpr comma

functionBlockExtend :: Exp -> TParser Exp
functionBlockExtend (Call exp str xs) = functionBlock >>= (\x -> return $ Call exp str (xs `appendList` x))
functionBlockExtend (Send exp str xs) = functionBlock >>= (\x -> return $ Send exp str (xs `appendList` x))
functionBlockExtend (Apply var xs vis) = functionBlock >>= (\x -> return $ Apply var (xs `appendList` x) vis)
functionBlockExtend (EVar var) = functionBlock >>= (\x -> return $ Apply var [x] Private)
functionBlockExtend _ = parserFail "Could not add a block to this expression"

appendList :: [a] -> a -> [a]
appendList xs x = xs ++ [x]

functionBlock :: TParser Exp
functionBlock = do
        keyword "do"
        params <- option [] $ between bar bar $ sepBy parameter comma
        exp <- block
        return $ Lambda params exp


-- | The list of formal parameters in a method/function defination
--
--   improving this parser is the first step to Github Issue #29
paramList :: TParser [Parameter]
paramList = between open close $ sepBy parameter comma

parameter :: TParser Parameter
parameter = vararg <|> param
  where
    vararg = star >> fmap VarArg identifier
    param = fmap Param identifier >>= hasDefault
    hasDefault (Param str) = (assignP >> fmap (Default str) safeExpr) <|> (return $ Param str)

arrayLiteral :: TParser Exp
arrayLiteral = do
  xs <- between bopen bclose $ sepBy expr comma
  return $ EArray xs

hashLiteral :: TParser Exp
hashLiteral = fmap EHash $ between copen cclose $ sepBy hash comma
  where
    hash :: TParser (Exp,Exp)
    hash = withAtom <|> withRocket
    withRocket = do
      key <- safeExpr -- we may need to relax these requirments
      rocket
      value <- safeExpr
      return (key,value)
    withAtom = do
      key <- hashAtom
      value <- safeExpr
      return (key,value)

-- | An identifier without scope. It forms the bases for a number of named
--   things. (classes, defs, vars, function application, method calles, etc)
identifier :: TParser String
identifier = tokenP tok
  where
    tok Token {token=TVar v} = Just v
    tok _ = Nothing

stringT :: TParser String
stringT = tokenP tok
  where
    tok Token {token=TString s} = Just s
    tok _ = Nothing

-- | An undecorated variable (@foo@) possibally scopped (@Foo::bar@)
--
--   __Notice__: This parser does not stand on it own but relies on the special
--   extending 'args' to produce an expression.
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

-- | Tries to turn a Var into function application.  Returns the
--   expression for the Var when there is no paramenter.  Note that
--   this is a Private visibility call as there is no reciever.
--
--   __Notice__: Unlike all other extending parsers, 'args' cannot fail.  It is
--   included in the set of basic parsers.
args :: Bool-> Var -> TParser Exp
args p var =
  (argumentList p >>= (\args->return (Apply var args Private))) <|> return (EVar var)

super :: Bool -> TParser Exp
super p = do
  keyword "super"
  (argumentList p >>= (\args-> return $ ESuper $ Just args)) <|> return (ESuper Nothing)

unaryOperator :: TParser Exp
unaryOperator = do
  o <- operator
  e <- expr0
  return $ Call e ("unary"++o) []
  
bangOperator  :: TParser Exp
bangOperator = do
  bang
  e <- expr0
  return $ Call e "not" [] 
  

-- | a sequence of expressions seperated by operators
opStr :: Exp -> TParser Exp
opStr e0 = do
    ops <- many1 op
    return $ OpStr e0 ops

operator = do
    tokenP testTok <?> "operator symbol"
  where
    testTok Token{ token=(TOperator op)} = Just op
    testTok _ = Nothing

-- | An operator and the expression which follows it.
op :: TParser (Op,Exp)
op = do
    o <-  tokenP testTok <?> "operator symbol"
    e <- exprForOpStr  -- extended expression sans opStr
    return(o,e)
  where
    testTok Token{ token=(TOperator op)} = Just op
    testTok _ = Nothing

-- | An extended expression without the opStr option used within OpStr
exprForOpStr = statement <|> expr1a
  where
    expr1a = expr0 >>= extend
    extend exp = (extension' exp >>= extend) <|> return exp
    extension' exp = assign exp <|> indexed exp <|> called exp <|> sent exp

-- | An extension parser for assignment expressions
--
--   Assignment is a little tricky because only certian forms can be
--   assigned to.  There is no possible way to assign to a literal value.
--   To accomplish this after finding an assignment operator, the parser
--   uses a helper function ('transLHS') to convert the LHS expression into
--   a special data type.  If that is not possible the transLHS parser
--   fails.
assign :: Exp -> TParser Exp
assign lhs' = do
  assignP
  lhs <- transLHS lhs'
  rhs <- expr
  case lhs of -- turn call, send and index into the appropriately renamed method calls 
    (LCall exp msg args) -> return $ Call exp msg   (args ++ [rhs])
    (LSend exp msg args) -> return $ Send exp msg   (args ++ [rhs])
    (LIndex exp args)    -> return $ Call exp "[]=" (args ++ [rhs])
    lhs                  -> return $ Assign lhs rhs

transLHS :: Exp -> TParser LHS
transLHS (EVar v) = return $ LVar v
transLHS (EIVar s) = return $ LIVar s
transLHS (Call exp s args) = return $ LCall exp (s++"=") args
transLHS (Index exp args ) = return $ LIndex exp args 
transLHS exp = fail $ "illigal Left Hand Side of assignment expression: " ++ (show exp)

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
      file <- sourceName `fmap` getPosition 
      return (Block exps file, Nothing)
    elseParse = optionMaybe (keyword "else" >> (block <|> termExpr))

whileParser :: TParser Exp
whileParser = do
  keyword "while"
  cond <- expr
  loop <- (block <|> termExpr)
  return $ While cond loop


classParser :: TParser Exp
classParser = do
  keyword "class"
  n <- var
  s <- optionMaybe $ tsuper >> var
  exp <- block <|> expr
  return $ EClass n s exp

moduleParser :: TParser Exp
moduleParser = do
  keyword "module"
  n <- var
  exp <- block <|> expr
  return $ Module n exp

defParser :: TParser Exp
defParser = do
  keyword "def"
  astNode <- (keyword "self" >> tdot >> return DefSelf) <|> return Def
  n <- identifier <|> stringT
  ps <- option [] $ paramList
  exp <- block
  return $ astNode n ps exp

defineParser :: TParser Exp
defineParser = do
  keyword "define"
  n <- identifier
  ps <- option [] $ paramList
  exp <- block
  return $ Assign (LIVar n) (Lambda ps exp)

lambda :: TParser Exp
lambda = do
  let single = do
        tsend <?> "End of params marker (->)"
        expr
  keyword "lambda"
  params <- option [] paramList
  exp <- block <|> single
  return $ Lambda params exp

-- | An extension parser checking for indexing brackets @foo[1]@
indexed exp = do
  idx <- between bopen bclose (sepBy expr comma) <?> "index expression ([...])"
  return $ Index exp idx

-- | An extension paser loking for method calls of the form @foo.bar@
called exp = do
  tdot
  s <- identifier
  args <- option [] $ argumentList True
  return $ Call exp s args

-- | An extension paser loking for asyncronous method calls of the form
--   @foo->bar@
sent exp = do
  tsend
  s <- identifier
  args <- option [] $ argumentList True
  return $ Send exp s args

-- | expressions which can safely be understood as the first argument in an unenclosed argument list
safeExpr0 =  nil <|> falseP <|> trueP <|> (var >>= args False) <|> super False <|>
            ivar <|> atom   <|> float <|> int  <|>    exString <|> bangOperator

safeExpr = do
    exp <- safeExpr0
    extend exp
  where
    extend exp = loop exp <|> return exp
    safeExtension exp = indexed exp <|> called exp <|> sent exp
    loop e = do
      exp' <- safeExtension e
      extend exp'

-- | The union of all basic expressions
expr0 :: TParser Exp
expr0 = paren
     <|> nil <|> falseP      <|> trueP <|> bangOperator <|> unaryOperator 
     <|> (var >>= args True) <|> super True   <|> ivar  <|> atom         <|> float
     <|> int <|> exString    <|> arrayLiteral           <|> hashLiteral 
     <?> "basic expression unit"

-- | The union of all extending parsers.  Given a base expression this
--   parser will check the subsiquent token stream for extended forms such
--   as assignment, indexing, method calls or operator strings.
extension :: Exp -> TParser Exp
extension exp = opStr exp <|> assign exp <|> indexed exp <|> called exp <|> sent exp <|> functionBlockExtend exp

-- | Basic expressions with extensions applied
expr1 :: TParser Exp
expr1 = do
    exp <- expr0
    extend exp
  where
    extend exp = loop exp <|> return exp
    loop e = do
      exp' <- extension e
      blk  <- getState
      if (not blk) then extend exp' else return exp'

-- | forms not subject to extending parsers. These parsers include their own
--   intrensic check for line termination.
statement = lambda <|> ifParser <|> whileParser <|> defParser <|> defineParser <|> classParser <|> moduleParser

-- | All possible forms of a single expression
expr = statement <|> expr1

-- | An expression where non-statement level expressions must have a TEnd or
--   TBlock.
termExpr = (setState False) >> (statement <|> term)
  where
    term = do
      exp <- expr1
      blk <- getState
      when (not blk) tend
      return exp

-- | comments WIP
tmeta = tokenP tok <?> "Comment/pragma"
  where
    tok Token{token=TMeta s} = Just s
    tok _ = Nothing

-- | A complete line of comment, potentially including a comment block.
meta = do
  tmeta
  (tend >> return ()) <|> (tblock >> return ())

-- | Used to ignore comments.
many1Ignore :: Stream s m t => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m [a]
many1Ignore pa pb = do
  many pb
  many1 $ do
    a <- pa
    many pb
    return a

-- | A list of expressions with meta lines ignored
exprs = many1Ignore termExpr meta


