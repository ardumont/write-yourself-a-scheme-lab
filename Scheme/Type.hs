module Scheme.Type where

import Control.Monad.Error
import Data.IORef
import Text.ParserCombinators.Parsec (ParseError)
import System.IO (Handle)

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

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

-- | Error in multiple monads words
type IOThrowsError = ErrorT LispError IO

-- | Environment to permit mutable variables
type Env = IORef [(String, IORef LispVal)]

-- | Variable name is a string
type VariableName = String

-- | A primitive function's name is a string
type PrimitiveName = String
