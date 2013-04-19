module LineParser where

import Prelude hiding (getLine, lines)
import Text.Printf
import qualified Prelude as P
import Control.Monad
import Control.Monad.State

type LineNo = Integer
type Offset = Integer

data Line = Line {line :: String, offset :: Offset, lineNo :: LineNo, block :: (Maybe CodeBlock)}
          | BlankLine LineNo deriving (Eq)

data CodeBlock = Block {lines :: [Line], startLine:: LineNo, indent :: Offset, filename :: FilePath} deriving (Eq)

instance Show Line where
  show (BlankLine i) = printf "%4d:\n" i
  show l = printf "%4d: %*s%s\n%s" (lineNo l) (offset l) "" (line l) (maybe "" show (block l))  

instance Show CodeBlock where
  show blk = printf "Block(start:%d, indent:%d, lines:%d, file:'%s')\n%s\n" (startLine blk) (indent blk) (length $ lines blk) (filename blk) (show $ lines blk)

data LParserState = LPS 
  { fileName        :: FilePath
  , input'          :: [String] -- lines to be parsed
  , nextLine'       :: Maybe Line -- to allow a line to be put back into the input stack
  , currentLineNum' :: LineNo 
  }

type LParser a = State LParserState a

unlineBlock :: CodeBlock -> String
unlineBlock b = concat $ map lineToString (lines b) 

lineToString :: Line -> String
lineToString (BlankLine _) = "\n"
lineToString l | (line l == "") =  (maybe "" unlineBlock (block l))
lineToString l = (replicate (fromIntegral $ offset l) ' ') ++ (line l) ++ "\n" ++ (maybe "" unlineBlock (block l))  

parseCode :: FilePath -> String -> CodeBlock
parseCode file "" = Block {lines = [], startLine = 0, indent = 0, filename = file}
parseCode file text =
  let ls = P.lines text
      s = LPS { fileName = file , input' = ls, nextLine' = Nothing, currentLineNum' = 0}
  in evalState parseLines s

parseFile :: FilePath -> IO CodeBlock
parseFile fp = (parseCode fp) `fmap` readFile fp 

getLine :: LParser (Maybe Line)
getLine = do
  s <- get
  case (nextLine' s, input' s) of
    (Nothing, (l:ls)) -> do
      let n = 1 + currentLineNum' s
      put s {currentLineNum' = n, input' = ls}
      return $ Just $ stripLine n l
    (Just l,_) -> do
      put s {nextLine' = Nothing}
      return $ Just l
    (Nothing, []) -> return Nothing

ungetLine :: Line -> LParser ()
ungetLine l = do
  s <- get
  put s {nextLine' = Just l}

compareLine :: Offset -> LParser (Maybe (Line,Ordering))
compareLine i = do
  maybe_l <- getLine
  case maybe_l of 
    Nothing -> return Nothing
    Just l@(BlankLine _) -> return $ Just (l,EQ)
    Just l -> return $ Just (l, compare (offset l) i )

parseLines ::  LParser CodeBlock
parseLines  = do
  l' <- getLine
  case l' of 
    Nothing -> error "parse lines called with nothing in buffer"
    Just l@(BlankLine _) -> parseLines >>= (return . prependLine l) -- ignore blank line
    Just l -> parseLines' (lineNo l) (offset l) [l]

parseLines' :: LineNo -> Offset -> [Line] -> LParser CodeBlock
parseLines' s i ls = do
  c <- compareLine i
  case c of
    Nothing -> mkBlock s i (reverse ls)  -- no input text left
    Just (l,EQ) -> parseLines' s i (l:ls) -- just another line in the same block
    Just (l,GT) -> do -- a further indent
      ungetLine l
      blk <- parseLines
      parseLines' s i (addBlockToHead blk ls)
    Just (l,LT) -> ungetLine l >> mkBlock s i (reverse ls)  -- end of block founds 

mkBlock :: LineNo -> Offset -> [Line]  -> LParser CodeBlock
mkBlock sl i ls = do
  fn <- gets fileName
  return Block {lines = ls, startLine = sl, indent = i, filename = fn} 

addBlockToHead :: CodeBlock -> [Line] -> [Line]
addBlockToHead blk (l:ls) = case (block l) of
  Nothing ->  let l' = l {block = Just blk} in (l':ls)
  Just existing -> 
    let l'   = Line { line = "", offset = indent blk, lineNo = startLine existing, block = Just existing}
	blk' = prependLine l' blk
	l''  = l { block = Just blk'}
    in  (l'':ls)

prependLine :: Line -> CodeBlock -> CodeBlock
prependLine l bk = bk {lines = (l:(lines bk)), startLine = lineNo' l} where
  lineNo' (BlankLine l) = l
  lineNo' l = lineNo l

stripLine :: LineNo -> [Char]  -> Line
stripLine = stripLine' 0 where
  stripLine' _ l [] = BlankLine l
  stripLine' i l (' ':cs) = stripLine' (i+1) l cs
  stripLine' i l cs = Line {line = cs, lineNo = l, offset = i, block = Nothing}
