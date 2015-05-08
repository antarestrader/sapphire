module Builtin.Object where

import Builtin.Utils hiding (call)
import Builtin.Bindings (initialize)
import Object
import AST
import Err
import Eval
import Parser (parseFile)
import Object.Spawn
import Context
import String
import Var
import Parser(parseString)

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.Except

objectClass = spawn $ Class{
        ivars = M.fromList [("setClass",VFunction setClass (1,Just 1))],
        klass = ROOT,
        modules = [],
	process = Nothing,
        super = ROOT,
        cvars = bootstrapset,
        cmodules = [],
        properName = "Object"}

bootstrapset = M.fromList [
         ("call", VFunction call (0,Nothing))
       , ("puts", VFunction puts (0,Nothing))
       , ("to_s", VFunction to_s (0,Just 0)) 
       , ("cls" , VFunction cls  (0, Just 0))
       , ("__initialize", VFunction initialize (0, Just 0))
       , ("debug", VFunction debug (0,Just 0))
       , ("eval", VFunction evalStr (1,Just 1))
       , ("load", VFunction loadFn  (1,Just 1))
       ]

debug _ = do
   val <- eval (EVar Self)
   case val of 
      VObject (Object{ivars = i}) -> do
        liftIO $ putStrLn $ show i
      VObject (Class{ivars = i, cvars = c}) -> liftIO $ do
        putStrLn "IVars:"
        putStrLn $ show i
        putStrLn "CVars:"
        putStrLn $ show c
      v -> liftIO $ putStrLn $ show v
   replyM_ VNil

to_s [] = do
  v <- eval (EIVar "__value")
  replyM_ $ VString $ mkStringLiteral $ show v

call _ = replyM_ $ VError $ Err "RunTimeError" "Function call on an object that does not act like a function" []

puts :: [Value] -> EvalM ()
puts vals = do
  vals' <- mapM (\v -> eval (Call (EValue v) "to_s" [])) vals
  let strs = map (\(VString s) -> string s) vals'   
  liftIO $ mapM_ putStrLn strs 
  replyM_ VNil

cls [] = gets (klass . self) >>= (replyM_ . VObject)

setClass [VObject cls] = do
  slf <- gets self
  modify (\c -> c{self=slf{klass=cls}})
  replyM_ VNil

evalStr :: [Value] -> EvalM()
evalStr [] = replyM_ VNil
evalStr vs = do
  let [Just str,filename,lineno,binding] = take 4 $ map Just vs ++ repeat Nothing
  str' <- toString str
  case parseString str' of -- Todo: include file name, line number and binding
    Left p   -> throwError $ Err "ParserError" p [str]
    Right es -> replyM_ =<< (fmap last $ mapM eval es)

load :: FilePath -> EvalM Value
load file = do
  ast <- liftIO $ parseFile file
  case ast of
    Left err -> throwError $ Err "ParserError" err []
    Right exprs -> eval $ Block exprs

loadFn :: [Value] -> EvalM()
loadFn [] = replyM_ VFalse
loadFn [VString x] = load (string x) >>= replyM_
loadFn _ = replyM_ VFalse
