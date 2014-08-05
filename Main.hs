module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad (liftM)
import Control.Monad.Error

-------------- types

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String s)                     = unwords ["\"", s, "\""]
showVal (Bool True)                    = "#t"
showVal (Bool False)                   = "#f"
showVal (Atom name)                    = name
showVal (Number n)                     = show n
showVal (List lispVals)                = "(" ++ unwordsList lispVals ++ ")"
showVal (DottedList headVals tailVals) = "(" ++ unwordsList headVals ++ "." ++ showVal tailVals ++ ")"

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

-- trapError :: (Show e, MonadError e m) => m String -> m String
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
  Left err      -> throwError $ Parser err
  v@(Right val) -> return val

eval :: LispVal -> ThrowsError LispVal
eval v@(String _)             = return v
eval v@(Number _)             = return v
eval v@(Bool   _)             = return v
eval (List [Atom "quote", v]) = return v
eval (List (Atom fn : args))  = mapM eval args >>= apply fn
eval l@_                      = throwError $ BadSpecialForm "Unrecognised special form" l

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
             ]

-- | Given a binary operation on integer, reduce function from [LispVal]
-- representing numbers to LispVal
numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp _ []    = throwError $ NumArgs 2 []
numericBinOp _ l@[_] = throwError $ NumArgs 2 l
numericBinOp binOp l = liftM (Number . foldl1 binOp) (mapM unpackNum l)

-- | Given a lisp val expression, extract a number.
-- If nothing matches a number, return 0
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = case reads n of
                         []    -> throwError $ TypeMismatch "Number" (String n)
                         (x:_) -> return $ fst x
unpackNum (List [n]) = unpackNum n
unpackNum _          = return 0

main :: IO ()
main = do
  args <- getArgs
  let lispVal = (readExpr . unwords) args >>= eval
  print lispVal
