module Builtin.Object (stateForObject) where

import Data.Map.Strict (empty, fromList)
import  Control.Monad.Except

import Object
import Name
import Err
import Scope hiding (Class)

stateForObject :: PID -> PID -> UID-> State
stateForObject objpid clspid uid = Class {
    ivars = empty -- TODO add "Class" and "Object"
  , instanceOfClass = clspid
  , globalNamespace = objpid
  , localNamespace = objpid
  , localCache = empty
  , superClass = undefined
  , methods = bootstrapset
  , methodCache = empty
  , modules = []
  , uid = uid
  }

bootstrapset = fromList [
         ("call",   Fn callFn)
       , ("initialize", Fn initFn)  
{-       , ("puts",   VFunction puts     (0,Nothing))
       , ("to_s",   VFunction to_s     (0,Just 0))
       , ("cls" ,   VFunction cls      (0, Just 0))
       , ("__initialize", VFunction initialize (0, Just 0))
       , ("debug",  VFunction debug    (0,Just 0))
       , ("eval",   VFunction evalStr  (1,Just 1))
       , ("load",   VFunction loadFn   (1,Just 1))
       , ("extend", VFunction extendFn (1, Nothing))
       , ("modules",VFunction modulesFn  (0,Just 0))
       , ("method_missing", VFunction methodMissing (1, Nothing))

       , ("==",     VFunction equality (1, Just 2))
-}
       ]

initFn _ = reply Nil --by default, do nothing

callFn :: Scope m => [Object] -> m a 
callFn _ = throwError $ VError $ Err 
   "RunTimeError" 
   "Function call on an object that does not act like a function" 
   []
{-
equality :: [Value] -> EvalM ()
equality [other] = do -- try the other side first
  slf <- gets self
  callT other "==" [(VObject slf),VTrue]
equality ((VObject other):_) = do -- compair using Object Identity as a last resort
  slf <- gets self
  replyM_ $ vbool (slf == other)
equality _ = replyM_ VFalse

methodMissing (msg:_) = do
  slf <- gets self
  throwError $ Err "MethodMissing" ("The method "++show msg++" not found.") [VObject slf]

debug _ = do
   val <- eval (EVar Self)
   case val of
      VObject (Object{ivars = i}) -> do
        liftIO $ putStrLn $ show i
      VObject (Class{ivars = i, cvars = c, cmodules = cs}) -> liftIO $ do
        putStrLn "IVars:"
        putStrLn $ show i
        putStrLn "CVars:"
        putStrLn $ show c
      v -> liftIO $ putStrLn $ show v
   replyM_ VNil

to_s [] = do
  v <- eval (EIVar "__value")
  case v of
    VNil -> replyM_ $ VString $ mkStringLiteral $ "<instance without to_s method>"
    _ -> replyM_ $ VString $ mkStringLiteral $ show v

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
    Right exprs -> eval $ Block exprs file

loadFn :: [Value] -> EvalM()
loadFn [] = replyM_ VFalse
loadFn [VString x] = load (string x) >>= replyM_
loadFn _ = replyM_ VFalse

extendFn:: [Value] -> EvalM()
extendFn mdls = do
    obj <- gets self
    obj' <- loop obj mdls
    modifySelf $ const obj'
    replyM_ VNil
  where
    loop obj [] = return obj
    loop obj (x:xs) = do
      o <- valToObj x
      loop (obj{modules = (o:(modules obj))}) xs -- assumes that self is never a PID

modulesFn :: [Value] -> EvalM()
modulesFn _ = do
  slf <- gets self
  replyM_ $ VArray $ A.fromList $ map VObject $ modules slf
-}
