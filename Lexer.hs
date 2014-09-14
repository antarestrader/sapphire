module Lexer where

import Control.Monad.Error
import Control.Monad.State
import qualified Data.Bits
import LineParser
import Text.Printf
import Data.Char
import Data.Word

-- | The state of the lexer to be held in the Lexer monad
data LexState = L
  { mode::[Int]  -- ^ mode for tokenizer
  , input :: AlexInput  -- ^ the buffer of text to be scanned
  , lsLine :: LineNo  -- ^ the line number being scanned
  , lsFile :: FilePath  -- ^ the file being scanned
  , _buffer :: String -> String -- ^ a buffer used to build up strings
  }

-- | the Lexer monad
type Lexer = ErrorT String (State LexState)

runLexer :: LexState -> Lexer a -> Either String a
runLexer s m = evalState (runErrorT m) s

putInput :: AlexInput -> Lexer ()
putInput t = modify (\s -> s{input = t})

pushChar :: Char -> Lexer ()
pushChar c = modify (\state -> state{input = modInput (input state)})
    where
      modInput (o,l,bs,s) = (o,l,bs,c:s)

pushMode :: Int -> Lexer ()
pushMode m = modify (\s@L{mode=ms} -> s{mode = (m:ms)})

popMode :: Lexer ()
popMode = modify pop
  where
    pop s@L{mode=([_])}  = s{mode = [0]}
    pop s@L{mode=(m:ms)} = s{mode = (ms)}



type AlexInput = ( Offset --  from front of line
                 , Char   --  previous char
                 , [Byte] --  Unicod multibyte buffer
                 , String )  --  remaining input

alexOffset :: AlexInput -> Offset
alexOffset (o,_,_,_) = o

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p,c,ps,s) = (p,c,[],s)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p,c,bs,s) = c

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (p,c,[],[]) = Nothing
alexGetByte (p,_,[],(c:s)) = 
  let p' = p + 1
      (b:bs) = utf8Encode c
  in p' `seq` Just (b, (p',c,bs,s))

skip :: Lexer [Token]
skip = return []

makeToken :: T -> Lexer [Token]
makeToken t = do
  s <- get
  return [Token{ tfile = lsFile s, tline = lsLine s, toffset = alexOffset(input s), token = t}]

buffer :: Lexer String
buffer = do
  b <- gets _buffer
  return $ b ""

clearBuffer :: Lexer ()
clearBuffer = modify (\s -> s{_buffer=id})

appendBuffer str = modify (\s@L{_buffer=b} -> s{_buffer = b . (str ++)})

-- | The core Token types.
data T =
  TKeyword String  | 
  TFloat Double    |
  TInt   Integer   | 
  TVar   String    |
  TIVar  String    |
  TOpen | TClose   |
  TBracket | TBracketClose |
  TBrace | TBraceClose |
  TDot | TSend     |
  TScope | TSuper  |
  TAssign | TComma |
  TAssignOp String |
  TAtom  String    |
  THashAtom String |
  TLabel String    |
  TOperator String |
  TString String   |
  TEnd             |
  StartInterp      |
  EndInterp        |
  TMeta String     |  -- comments, pragma and friends
  TBlock CodeBlock
    deriving (Eq, Show)

-- | A complete token with location information
data Token = Token {tfile:: FilePath, tline :: LineNo, toffset::Offset, token::T}

instance Show Token where
  show t = printf "[%s (%i,%i)]" (show $ token t) (tline t) (toffset t)

esc :: String -> String
esc "n" = "\n"
esc "t" = "\t"
esc c = c

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f = [oc]

   | oc <= 0x7ff = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

type Byte = Word8

