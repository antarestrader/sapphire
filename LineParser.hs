-- LineParser.hs Copyright 2013, 2014 John F. Miller
-- This file is part of Sapphire and Licensed under the GNU General Public
-- Licnese, Version 3.  See LICENCE file for details

-- | A block is a list of lines at the same indentation level. A line is the
--   text from the first non-space character to the end of the line plus the
--   block of lines that follow immediately and are more indented then the line.
--
--   The line parser is fast, brutish and dumb. Its world consists of spaces
--   (U+0020) new lines (U+000a) and everything else. It produces blocks and
--   lines. A block is a list of lines at the same level of indentation. A line
--   is all characters between the first non-space character and a new line
--   character plus a block of all following lines whose indentation is strictly
--   greater than the leading line. Lines consisting only of spaces and a new
--   line character are ignored. All Sapphire statements must be on one line
--   (which includes the block that follows it).
--
--   The line scanner knows nothing of the grammar it scans. /Nothing/ can break
--   the rules. Comments, string literals and the like all must obey the rules.
--   The benefit of this thuggish line scanner is that we can quickly determine
--   the full extent of a class, module, or function. All fit on a single line
--   and the block which follows.
--
--   This also makes it very simple to embed independent syntactical structures
--   into Sapphire. The line scanner pulls off the demarcating spaces and
--   returns rest of the syntax intact. There is no possibility of a end tag
--   collision
module LineParser (
    LineNo
  , Offset
  , Line(..)
  , CodeBlock(..)
  , unlineBlock
  , lineToString
  , parseCode
  , parseFile
  )
where

import Prelude hiding (getLine, lines)
import Text.Printf
import qualified Prelude as P
import Control.Monad
import Control.Monad.State

type LineNo = Integer
type Offset = Integer

-- | This is a line of code as understood by the line parser (i.e. a text line
--   and all indented lines beneath as a block). A blank line has its own
--   constructor sense they do not effect block creation.
data Line
    -- | A typical line.  In addation to the text this structure contains
    --   enough information to recreate the line and calculate appropriate
    --   offsets and line numbers when reporting errors.
  = Line {   line :: String -- ^ The text of the line less leading spaces
         , offset :: Offset -- ^ The number of leading spaces stripped
         , lineNo :: LineNo -- ^ Vertical offset of this line for error reporting
         ,  block :: (Maybe CodeBlock) -- ^ The indented block below this line
         }
    -- | represents a blank line.  These are not parsed, but are needed to
    --   recreate spacing in the event that the block must be turned back into
    --   text.
  | BlankLine LineNo deriving (Eq)

-- | This is a block as understood by the line parser, a collection of lines
--   in order and at the same level of indent.  (remember lines may contain
--   blocks of indented code beneath them.)
data CodeBlock =
  Block { lines :: [Line]      -- ^ The lines in this block
        , startLine:: LineNo   -- ^ The vertical offset of the first line
        , indent :: Offset     -- ^ The number os spaces that proceed every line in the block
        , filename :: FilePath -- ^ The file that this block came from for error reporting purposes
        } deriving (Eq)

instance Show Line where
  show (BlankLine i) = printf "%4d:\n" i
  show l = printf "%4d: %*s%s\n%s" (lineNo l) (offset l) "" (line l) (maybe "" show (block l))

instance Show CodeBlock where
  show blk = printf "Block(start:%d, indent:%d, lines:%d, file:'%s')\n%s\n" (startLine blk) (indent blk) (length $ lines blk) (filename blk) (show $ lines blk)

-- | Turn a codeblock back into its original text.  Note that this is different
--   from the Show instance which will add line number to the beginging of
--   lines.
unlineBlock :: CodeBlock -> String
unlineBlock b = concat $ map lineToString (lines b)

-- | Turn a line (including an attached block)_ back into the string that
--   created it.
lineToString :: Line -> String
lineToString (BlankLine _) = "\n"
lineToString l | (line l == "") =  (maybe "" unlineBlock (block l))
lineToString l = (replicate (fromIntegral $ offset l) ' ') ++ (line l) ++ "\n" ++ (maybe "" unlineBlock (block l))

-- | parse a string into a CodeBlock
parseCode :: FilePath -> String -> CodeBlock
parseCode file "" = Block {lines = [], startLine = 0, indent = 0, filename = file}
parseCode file text =
  let ls = P.lines text
      s = LPS { fileName = file , input' = ls, nextLine' = Nothing, currentLineNum' = 0}
  in evalState parseLines s

-- | Parse a file into a CodeBlock
parseFile :: FilePath -> IO CodeBlock
parseFile fp = (parseCode fp) `fmap` readFile fp


-- Private functions and data below:

-- The state for our simple parser
data LParserState = LPS
  { fileName        :: FilePath
  , input'          :: [String] -- lines to be parsed
  , nextLine'       :: Maybe Line -- to allow a line to be put back into the input stack
  , currentLineNum' :: LineNo
  }

-- A state monad in which to run our line parsing
type LParser a = State LParserState a

-- Pull the "next" text line off of this stack, and turn it into a Line. 
-- If there is a `nextline` that has been pushed back onto the top of the
-- stack return it instead.  Also keep the currentLineNum' counter up to date.
-- This does not add a block to the line; that comes later.
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

-- This line (and its accompanying block) actually belong with the next
-- lines so push it back onto the top of the stack.
ungetLine :: Line -> LParser ()
ungetLine l = do
  s <- get
  put s {nextLine' = Just l}


-- Get a line from the stack and deside whether it is more, less or
-- the same indentation as the given Offset `i`. In the case that there
-- is nothing more on the stack return Nothing instead
compareLine :: Offset -> LParser (Maybe (Line,Ordering))
compareLine i = do
  maybe_l <- getLine
  case maybe_l of
    Nothing -> return Nothing
    Just l@(BlankLine _) -> return $ Just (l,EQ)
    Just l -> return $ Just (l, compare (offset l) i )


-- Using the next line as the starting point, grab a set of equally indented
-- lines and put them together into a block.
parseLines ::  LParser CodeBlock
parseLines  = do
  l' <- getLine
  case l' of
    Nothing -> error "parse lines called with nothing in buffer"
    Just l@(BlankLine _) -> -- For a blank line parse the rest of the block
                            -- then put a blank line back at the top. 
        parseLines >>= (return . prependLine l) 
    Just l -> parseLines' (lineNo l) (offset l) [l]

-- Helper function builds a list of lines in reverse order then turns them 
-- into a block when a less indented line (or EOF) is found.
parseLines' :: LineNo -> Offset -> [Line] -> LParser CodeBlock
parseLines' s i ls = do
  c <- compareLine i
  case c of
    Nothing -> mkBlock s i (reverse ls)  -- no input text left
    Just (l,EQ) -> parseLines' s i (l:ls) -- just another line in the same block
    Just (l,GT) -> do -- a further indent; start a new block
      ungetLine l -- this will be the first line of the new block
      blk <- parseLines -- make the new black
      -- put this block into the most recent line.
      parseLines' s i (addBlockToHead blk ls) 
    Just (l,LT) ->  -- the block ends because the next line is less indented 
                    -- than the this set reverse the list and make a new block
                    -- from it.
      ungetLine l >> mkBlock s i (reverse ls)

-- Make a Code Block from a list of lines using the filename in the parser state.
mkBlock :: LineNo -> Offset -> [Line]  -> LParser CodeBlock
mkBlock sl i ls = do
  fn <- gets fileName
  return Block {lines = ls, startLine = sl, indent = i, filename = fn}


-- Helper Function for `parseLines'`.  Puts the CodeBlock into the first line
-- of the list.  This list is the lines **IN REVERSE ORDER** as is used in
-- `parseLines'`.  There are some edge cases when then line in question
-- already has a block, or the line above is blank. Becasue of the way
-- `parseLines'` is written, this function can never be called on an empty
-- list.
addBlockToHead :: CodeBlock -> [Line] -> [Line]

-- If the most recently pushed line is blank, it does not have a block field.
-- Instead, make the blank line part of the block and add the block to the
-- line above
addBlockToHead blk (l@(BlankLine _):ls) = addBlockToHead (pushLine blk l) ls
  where
    pushLine blk@Block{lines=ls} l@(BlankLine ln) = blk{lines = l:ls, startLine = ln}

-- The standard case
addBlockToHead blk (l:ls) = case (block l) of
  Nothing -> ((l{block = Just blk}):ls) 
  Just existing -> -- There was already a block. (implies negitive 
                   -- indentation) Create a psudo Line containing no text
                   -- but only the block as the first line of the outer block.
    let l'   = Line { 
                      line = ""
                    , offset = indent blk
                    , lineNo = startLine existing
                    , block = Just existing
                    }
        blk' = prependLine l' blk
        l''  = l { block = Just blk'}
    in  (l'':ls)

-- Add a line to the front of a block. This is used to place a negitavly 
-- indented CodeBlock at the front of the existing code block, and to place
-- blank lines at the top of a code block.
prependLine :: Line -> CodeBlock -> CodeBlock
prependLine l bk = bk {lines = (l:(lines bk)), startLine = lineNo' l} where
  lineNo' (BlankLine l) = l
  lineNo' l = lineNo l

-- | Remove the leading spaces from a line
stripLine :: LineNo -> [Char]  -> Line
stripLine = stripLine' 0 where
  stripLine' _ l [] = BlankLine l
  stripLine' i l (' ':cs) = stripLine' (i+1) l cs
  stripLine' i l ('\t':cs) = error $ printf "illegal TAB charactor at line %d:%d" l i
  stripLine' i l cs = Line {line = cs, lineNo = l, offset = i, block = Nothing}
