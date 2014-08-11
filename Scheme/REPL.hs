module Scheme.REPL where

import Scheme.Type
import Scheme.Primitive
import Control.Monad.Error
import System.IO
import Scheme.Parser (readExprList
                     , readExpr )
import Data.IORef
import Data.Maybe (isJust)

-- | Lift
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right v)  = return v

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

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

-- | Wrapper around apply
applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

-- | makePort wraps the Haskell function openFile, converting it to the right type
-- and wrapping its return value in the Port constructor.
-- It's intended to be partially-applied to the IOMode, ReadMode for
-- open-input-file and WriteMode for open-output-file:
makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

-- | closePort also wraps the equivalent Haskell procedure, this time hClose:
closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftM (const $ Bool True) (liftIO $ hClose port)
closePort _           = return (Bool False)

-- | readProc wraps the Haskell hGetLine and then sends the result to parseExpr,
-- to be turned into a LispVal suitable for Scheme:
readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr

-- | writeProc converts a LispVal to a string and then writes it out on the specified port
writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftM (const $ Bool True) (liftIO $ hPrint port obj)

-- | readContents reads the whole file into a string in memory.
-- It's a thin wrapper around Haskell's readFile, again just lifting
-- the IO action into an IOThrowsError action and wrapping it in a String constructor:
readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String (liftIO $ readFile filename)

-- | The helper function load doesn't do what Scheme's load does (we handle that later).
-- Rather, it's responsible only for reading and parsing a file full of statements.
-- It's used in two places: readAll (which returns a list of values) and load (which evaluates those values as Scheme expressions).
load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

-- | readAll then just wraps that return value with the List constructor:
readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

-- | Load primitive functions in Env
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map (makeFun IOFunc) ioPrimitives ++ map (makeFun PrimitiveFunc) primitives)
  where makeFun constructor (vname, lispval) = (vname, constructor lispval)


-- *Main> :main "(equal? '(1) '(1))"
-- #t
-- *Main> :main "(equal? '(1) '(1 2))"
-- #f
-- *Main> :main "(equal? '1 \"1\")"
-- #t
-- *Main> :main "(eqv? '1 \"1\")"
-- #f
-- *Main> :main "(equal? '1)"
-- Expected 2 args; found values 1

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
