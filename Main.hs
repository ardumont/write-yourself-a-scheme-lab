{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Control.Monad.Error
import Data.IORef
import Data.Maybe (isJust)
import System.Environment
import System.IO hiding (try)
import Text.ParserCombinators.Parsec hiding (spaces)

-------------- types

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String]
                    , varargs :: Maybe String
                    , body :: [LispVal]
                    , closure :: Env}
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String s)                     = "\"" ++ s ++ "\""
showVal (Bool True)                    = "#t"
showVal (Bool False)                   = "#f"
showVal (Atom name)                    = name
showVal (Number n)                     = show n
showVal (List lispVals)                = "(" ++ unwordsList lispVals ++ ")"
showVal (DottedList headVals tailVals) = "(" ++ unwordsList headVals ++ " . " ++ showVal tailVals ++ ")"
showVal (PrimitiveFunc _)              = "<primitive>"
showVal (Func { params = args
              , varargs = vararg
              , body = _
              , closure = _})          = "(lambda (" ++ unwords (map show args) ++
                                           (case vararg of
                                             Nothing  -> ""
                                             Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _)                       = "<IO port>"
showVal (IOFunc _)                     = "<IO primitive>"

-- | The possible errors
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

-- | Use haskell's built-in error handling mechanism
instance Error LispError where
  noMsg  = Default "An error has occured"
  strMsg = Default

instance Show LispError where show = showError

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

type ThrowsError = Either LispError

trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right v) = v

-------------- functions

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- | A string begins with a "
parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

-- | An atom is a letter or a symbol followed by any number of letters, digits or symbols.
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
             "#t" -> Bool True
             "#f" -> Bool False
             _    -> Atom atom

-- | A number is any number of digits
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- | A list is a LispVal expression space separated
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- | A dotted list is a list of space separated lisp expression and
-- a . to define the tail. The tial is also a lisp expression.
parseDottedList :: Parser LispVal
parseDottedList = do
  h <- endBy parseExpr spaces
  t <- char '.' >> spaces >> parseExpr
  return $ DottedList h t

-- | Parse quote expression
parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

-- | Parse LispVal expression
parseExpr :: Parser LispVal
parseExpr =   parseAtom
          <|> parseString
          <|> parseNumber
          <|> parseQuoted
          <|> do
                char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
  Left err  -> throwError $ Parser err
  Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)

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

-- | Load primitive functions in Env
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map (makeFun IOFunc) ioPrimitives ++ map (makeFun PrimitiveFunc) primitives)
  where makeFun constructor (vname, lispval) = (vname, constructor lispval)

type PrimitiveName = String

-- | Supported primitive functions
primitives :: [(PrimitiveName, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("+", numericBinOp (+))
             , ("-", numericBinOp (-))
             , ("*", numericBinOp (*))
             , ("/", numericBinOp div)
             , ("mod", numericBinOp mod)
             , ("quotient", numericBinOp quot)
             , ("remainder", numericBinOp rem)
             , ("=", numBoolBinOp (==))
             , ("<", numBoolBinOp (<))
             , (">", numBoolBinOp (>))
             , ("/=", numBoolBinOp (/=))
             , (">=", numBoolBinOp (>=))
             , ("<=", numBoolBinOp (<=))
             , ("&&", boolBoolBinOp (&&))
             , ("||", boolBoolBinOp (||))
             , ("string=?", strBoolBinOp (==))
             , ("string?", strBoolBinOp (>))
             , ("string<=?", strBoolBinOp (<=))
             , ("string>=?", strBoolBinOp (>=))
             , ("car", car)
             , ("cdr", cdr)
             , ("cons", cons)
             , ("eq?", eqv)
             , ("eqv?", eqv)
             , ("equal?", equal)
             ]

-- | Given a binary operation on integer, reduce function from [LispVal]
-- representing numbers to LispVal
numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp binOp l@(_:_:_)    = liftM (Number . foldl1 binOp) (mapM unpackNum l)
numericBinOp _ emptyOrSingleArg = throwError $ NumArgs 2 emptyOrSingleArg

-- | Given a lisp val expression, extract a number.
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = case reads n of
                         []    -> throwError $ TypeMismatch "Number" (String n)
                         (x:_) -> return $ fst x
unpackNum (List [n]) = unpackNum n
unpackNum notANum    = throwError $ TypeMismatch "Number" notANum

-- | Given a lisp val expression, extract a string.
unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = (return . show) s
unpackStr (Bool s)   = (return . show) s
unpackStr notAString = throwError $ TypeMismatch "String" notAString

-- | Given a bool expression, extract a boolean
unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notABool = throwError $ TypeMismatch "Bool" notABool

numBoolBinOp :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinOp = boolBinop unpackNum

strBoolBinOp :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinOp = boolBinop unpackStr

boolBoolBinOp :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinOp = boolBinop unpackBool

-- | Generic binary operation on argument with explicitely 2 arguments
boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op [h, t] = do
                                 left <- unpacker h
                                 right <- unpacker t
                                 return $ Bool $ op left right
boolBinop _ _ no2Elements    = throwError $ NumArgs 2 no2Elements

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)]         = return x
car [DottedList (x : _) _] = return x
car [badArg]               = throwError $ TypeMismatch "pair" badArg
car badArgList             = throwError $ NumArgs 1 badArgList

-- *Main> :main "(car '(b . 2))"
-- b
-- *Main> :main "(car '(2))"
-- 2
-- *Main> :main "(car '())"
-- Invalid type: expected pair, found ()

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)]         = return $ List xs
cdr [DottedList [_] xs]     = return xs
cdr [DottedList (_ : xs) t] = return $ DottedList xs t
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

-- *Main> :main "(cdr '(2 1))"
-- (1)
-- *Main> :main "(cdr '(2 1 . 4))"
-- (1 . 4)
-- *Main> :main "(cdr '(2 1 . 4))"
-- (1 . 4)
-- *Main> :main "(cdr '(2))"
-- ()
-- *Main> :main "(cdr '())"
-- Invalid type: expected pair, found ()

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List (x : xs)
cons [x, DottedList xs t] = return $ DottedList (x : xs) t
cons [x0, x1] = return $ DottedList [x0] x1
cons badArgList = throwError $ NumArgs 2 badArgList

-- *Main> :main "(cons 'a '(a))"
-- (a a)
-- *Main> :main "(cons 'a '(b))"
-- (a b)
-- *Main> :main "(cons 'a '(b . 2))"
-- (a b . 2)

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool t0, Bool t1] = return $ Bool $ t0 == t1
eqv [Number n0, Number n1] = return $ Bool $ n0 == n1
eqv [String s0, String s1] = return $ Bool $ s0 == s1
eqv [Atom a0, Atom a1] = return $ Bool $ a0 == a1
eqv [DottedList x0 t0, DottedList x1 t1] = eqv [List (t0 : x0), List (t1 : x1)]
eqv [List x0, List x1] = return $ Bool $ (length x0 == length x1) && all eqvPair (zip x0 x1)
                         where eqvPair (y0, y1) = case eqv [y0, y1] of
                                 Left _         -> False
                                 Right (Bool v) -> v
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

-- *Main> :main "(eqv '(2 3) '(2 3))"
-- #t
-- *Main> :main "(eqv #t #f)"
-- #f
-- *Main> :main "(eqv #f #f)"
-- #t
-- *Main> :main "(eqv #f '(1))"
-- #f
-- *Main> :main "(eqv #t '(1))"
-- #f
-- *Main> :main "(eqv 1 '(1))"
-- #f

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

-- | Determines if 2 lispVals are equals when they are unpacked
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals l0 l1 (AnyUnpacker unpackFn) = do
  lispVal0 <- unpackFn l0
  lispVal1 <- unpackFn l1
  return $ lispVal0 == lispVal1
  `catchError`
  const (return False)

-- | Determines if 2 lispVals are loosely equals (coercion)
equal :: [LispVal] -> ThrowsError LispVal
equal l@[l0, l1] = do
  primitiveEqual <- liftM or $ mapM (unpackEquals l0 l1) [ AnyUnpacker unpackNum
                                                         , AnyUnpacker unpackStr
                                                         , AnyUnpacker unpackBool]
  (Bool eqvEqual) <- eqv l
  return $ Bool $ primitiveEqual || eqvEqual
equal badArgList = throwError $ NumArgs 2 badArgList

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

-- To permit mutable variables
type Env = IORef [(String, IORef LispVal)]

-- | Init mutable environment
nullEnv :: IO Env
nullEnv = newIORef []

-- | Error in multiple monads words
type IOThrowsError = ErrorT LispError IO

-- | Lift
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right v)  = return v

-- | Runs the top level IOThrowsError action and returns the IO computation
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = liftM extractValue $ runErrorT (trapError action)

type VariableName = String

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

main :: IO ()
main =
  getArgs >>=
  \ args -> if null args then runRepl ":q" "lisp>>> " else runOne args

-- *Main> :main "(cons 2 3)"
-- (2 . 3)
-- *Main> :main
-- lisp>>> (cons 2 3)
-- (2 . 3)
-- lisp>>> (cons 2 '())
-- (2)
-- lisp>>> (cons 2 '(3))
-- (2 3)
-- lisp>>> :q
-- *Main> :browse Data.IORef

-- *Main> :main
-- lisp>>> (+ 1 1)
-- 2
-- lisp>>> (+ 1 10)
-- 11
-- lisp>>> (define (f x y) (+ x y))
-- (lambda ("x" "y") ...)
-- lisp>>> (f 1 2)
-- 3
-- lisp>>> (define x 10)
-- 10
-- lisp>>> (f 1 2)
-- 3
-- lisp>>> (f 10 2)
-- 12
-- lisp>>> (f 10 x)
-- 20
-- lisp>>> ((lambda (x y) (* x y)) 10 10)
-- 100
-- lisp>>> ((lambda (x y) (* x y)) 10 x)ef
-- 100
-- lisp>>> ((lambda (x y) (* x y)) 10 x)
-- 100
-- lisp>>> (define x 20)
-- 20
-- lisp>>> ((lambda (x y) (* x y)) 10 x)ef
-- 200
-- lisp>>> (set! x 200)
-- 200
-- lisp>>> ((lambda (x y) (* x y)) 10 x)
-- 2000
-- lisp>>> (define (f) "hello")
-- (lambda () ...)
-- lisp>>> f
-- (lambda () ...)
-- lisp>>> (f)
-- "hello"
-- lisp>>> (define (mute!) (set! x (+ x 20)))
-- (lambda () ...)
-- lisp>>> x
-- 200
-- lisp>>> (mute!)
-- 220
-- lisp>>> x
-- 220
-- lisp>>> (mute!)
-- 240
-- lisp>>> x
-- 240
-- lisp>>> :q
