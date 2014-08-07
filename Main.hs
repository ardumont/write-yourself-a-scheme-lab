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
             deriving Eq

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String s)                     = "\"" ++ s ++ "\""
showVal (Bool True)                    = "#t"
showVal (Bool False)                   = "#f"
showVal (Atom name)                    = name
showVal (Number n)                     = show n
showVal (List lispVals)                = "(" ++ unwordsList lispVals ++ ")"
showVal (DottedList headVals tailVals) = "(" ++ unwordsList headVals ++ " . " ++ showVal tailVals ++ ")"

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

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> throwError $ Parser err
  Right val -> return val

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ v@(String _)             = return v
eval _ v@(Number _)             = return v
eval _ v@(Bool   _)             = return v
eval env (Atom var)             = getVar env var
eval _ (List [Atom "quote", v]) = return v
eval env (List [Atom "if", predicate, ifStmt, elseStmt]) =
  eval env predicate >>=
  \ result -> eval env $ if result /= Bool False then ifStmt else elseStmt
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom fn : args)) = mapM (eval env) args >>= liftThrows . apply fn
eval _ l@_                       = throwError $ BadSpecialForm "Unrecognised special form" l

type PrimitiveName = String

-- | Given a function name, and a LispVal, return a LispVal
apply :: PrimitiveName -> [LispVal] -> ThrowsError LispVal
apply fn args = maybe (throwError $ NotFunction "Unrecognised primitive function" fn)
                      ($ args)
                      (lookup fn primitives)

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
runRepl quitCommand strPrompt = nullEnv >>= until_ (== quitCommand) (readPrompt strPrompt) . evalAndPrint

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl ":q" "lisp>>> "
    1 -> runOne $ head args
    _ -> putStrLn "This programs takes 0 or 1 argument."

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
