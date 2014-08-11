{-# LANGUAGE ExistentialQuantification #-}
module Scheme.Primitive where

import Scheme.Type
import Control.Monad.Error

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
