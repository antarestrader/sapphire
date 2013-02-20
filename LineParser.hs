module LineParser where

type LineNo = Int
type Offset = Int
type Position = (LineNo,Offset)

data Fragment= Fragment {
    position ::  Position
  , fragment :: [Char]
  } | BlankLine LineNo deriving Show

foffset = snd . position

data Line = Line [Fragment] (Maybe CodeBlock) deriving Show

type CodeBlock = [Line]

stripLine :: LineNo -> [Char]  -> Fragment
stripLine = stripLine' 0 where
  stripLine' _ l [] = BlankLine l
  stripLine' i l (' ':cs) = stripLine' (i+1) l cs
  stripLine' i l cs = Fragment {position = (l,i), fragment = cs}

makeFragments :: String -> [Fragment]
makeFragments s = reverse . snd $ foldl fn (1,[]) (lines s)
 where 
  fn (l,fs) cs = 
    let f = stripLine l cs
    in 
      case f of 
        BlankLine _ -> (l+1,fs)
	otherwise -> (l+1,f:fs)

makeLines :: [Fragment] -> CodeBlock
makeLines fs = fst $ makeLines' 0  [] fs

makeLines' ::  Offset ->[Line] -> [Fragment] -> (CodeBlock,[Fragment])
makeLines' _ rs [] = (reverse rs,[])
makeLines' o rs (f:fs) | (foffset f) < o = (reverse rs, (f:fs))
makeLines' o rs fs = 
  let (r,fs') = makeLine fs in 
  makeLines' o (r:rs) fs'

makeLine :: [Fragment] -> (Line,[Fragment])
makeLine (f:[]) = (Line [f] Nothing,[])
makeLine (f:n:fs) | (foffset f) >= (foffset n) = (Line [f] Nothing,(n:fs))
makeLine (f:fs) = let (gs,blk,fs') = makeBlock (foffset f) fs in (Line (f:gs) (Just blk), fs')

makeBlock :: Offset ->[Fragment] -> ([Fragment],CodeBlock,[Fragment])
makeBlock o fs = makeBlock' o [] fs
  where 
    makeBlock' o rs (f:fs) =
      let x = makeLines' (foffset f) [] (f:fs)
      in
        case x of
	  (ls,[]) -> (rs, ls, [])
	  (ls,(f':fs')) |  ((foffset f') <= o) ->  (rs, ls, (f':fs'))
	  (ls,fs') ->  makeBlock' o (rs++(deline ls)) fs'
    deline [] = []
    deline ((Line fs Nothing):ls) = fs ++ deline ls
    deline ((Line fs (Just rs)):ls) = fs ++ deline rs ++ deline ls

lineFile :: FilePath -> IO CodeBlock
lineFile f = do
  s <- readFile f
  return $ (makeLines . makeFragments) s
