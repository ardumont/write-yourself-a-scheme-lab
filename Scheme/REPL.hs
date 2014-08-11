module Scheme.REPL where

import Scheme.Type
import Scheme.Primitive
import Scheme.IOPrimitive
import Control.Monad.Error
import System.IO
import Scheme.Parser (readExpr)
import Data.IORef
import Data.Maybe (isJust)

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs = makeFunc . Just . showVal

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ v@(String _)             = return v
eval _ v@(Number _)             = return v
eval _ v@(Bool   _)             = return v
eval env (Atom var)             = getVar env var
eval _ (List [Atom "quote", v]) = return v
eval env (List [Atom "if", predicate, ifStmt, elseStmt]) =
  eval env predicate >>=
  \ result -> eval env $ case result of
                           Bool False -> elseStmt
                           _          -> ifStmt
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) = -- let
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarArgs varargs env [] body
eval env (List [Atom "load", String filename]) =
  load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do
  fn <- eval env function
  argVals <- mapM (eval env) args
  apply fn argVals
eval _ l@_ = throwError $ BadSpecialForm "Unrecognised special form" l

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc fn) args = liftThrows $ fn args
apply (Func { params = params
             , varargs = varargs
             , body = body
             , closure = closure}) args =
  if num params /= num args
  then throwError $ NumArgs (num params) args
  else liftIO (bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        remainingArgs = drop (length params) args
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
          _            -> return env

-- | Determine if a variable is bound in the environment env
isBound :: Env -> VariableName -> IO Bool
isBound envRef var = liftM (isJust . lookup var) (readIORef envRef)

-- | Primitive - Return the variable from the environment envRef
getVar :: Env -> VariableName -> IOThrowsError LispVal
getVar envRef var =
  let readRef = liftIO . readIORef in
  do env <- readRef envRef
     maybe (throwError $ UnboundVar "Unbound variable" var)
           readRef
           (lookup var env)

-- | Primitive - Set the variable in the environment
setVar :: Env -> VariableName -> LispVal -> IOThrowsError LispVal
setVar envRef var lispVal = do
  env <- (liftIO . readIORef) envRef
  maybe (throwError $ UnboundVar "Unbound variable" var)
        (liftIO . (`writeIORef` lispVal))
        (lookup var env)
  return lispVal

-- | Update a variable if already defined or create one
defineVar :: Env -> VariableName -> LispVal -> IOThrowsError LispVal
defineVar envRef var lispVal = do
  bound <- liftIO $ isBound envRef var
  if bound
  then setVar envRef var lispVal
  else liftIO $ do
         newioref <- newIORef lispVal
         env      <- readIORef envRef
         writeIORef envRef $ (var, newioref) : env
         return lispVal

-- | Bind a list of variables
bindVars :: Env -> [(VariableName, LispVal)] -> IO Env
bindVars envRef bindings =
  readIORef envRef >>= extendEnv >>= newIORef
  where extendEnv env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)

-- | Wrapper around apply
applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

-- | Load primitive functions in Env
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map (makeFun IOFunc) (("apply", applyProc) : ioPrimitives) ++ map (makeFun PrimitiveFunc) primitives)
  where makeFun constructor (vname, lispval) = (vname, constructor lispval)

-- | Init mutable environment
nullEnv :: IO Env
nullEnv = newIORef []

-- | Runs the top level IOThrowsError action and returns the IO computation
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = liftM extractValue $ runErrorT (trapError action)

-- | Display a string and force writing on stdout
flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

-- | Display a prompt and wait for user's input
readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- | Evaluate a string expression and return the result
evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ liftThrows (readExpr expr) >>= eval env

-- | Eval and print
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

-- | Repeat indefinitely
until_ :: Monad m => (t -> Bool) -> m t -> (t -> m ()) -> m ()
until_ predicate prompt action = do
  result <- prompt
  unless (predicate result) $ action result >> until_ predicate prompt action

-- | Start a repl
runRepl :: String -> String -> IO ()
runRepl quitCommand strPrompt =
  primitiveBindings >>=
  until_ (== quitCommand) (readPrompt strPrompt) . evalAndPrint

-- | Expect a filename to load as the first argument
runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    runIOThrows (liftM show $ eval env (List [Atom "load", String (head args)]))
        >>= hPutStrLn stderr
