

module Scheme.Eval.Prim where

import Text.ParserCombinators.Parsec
import Control.Monad.Error 

import Scheme.Lex.LispVal
import Scheme.Eval.LispError


-- A list of default functions. These values
-- map a string name to a function mapping some
-- list of arguments to a new value.
-- The function lookup will look up some string
-- in a list of tuples like this, and return the
-- function associated with the given string. 
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("string?", is_string),
              ("number?", is_number),
              ("symbol?", is_symbol),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=))] 



-- This function takes the name of some function,
-- which we assume to be a generic haskell function
-- that operates on integers, a list of arguments,
-- and produces a curried funtion that when evaluated
-- will produce another lisp val. 
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal 
--numericBinop op params = Number $ foldl1 op $ map unpackNum params
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op 


-- Unpack a numerical argument. If the argument is
-- a string, then try to convert it to a string. 
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n
                       in if null parsed
                          then throwError $ TypeMismatch "number" $ String n
                          else return $ fst $ parsed !! 0 
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum 



-- Unpack a string
unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString



-- Unpack a Boolean
unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool 




-- Function for checking if a value is a string
is_string :: [LispVal] -> ThrowsError LispVal 
is_string (String _:[]) = return $ Bool True
is_string _ = return $ Bool False

-- Function for checking if a value is a number
is_number :: [LispVal] -> ThrowsError LispVal 
is_number (Number _:[]) = return $ Bool True
is_number _ = return $ Bool False

-- Function for checking if a value is a Symbol
is_symbol :: [LispVal] -> ThrowsError LispVal 
is_symbol (Atom _:[]) = return $ Bool True 
is_symbol _ = return $ Bool False 


-- This base function is a helper function that
-- parameterizes the operation evaluation process. 
boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args 
                             else do
                               left <- unpacker $ args !! 0
                               right <- unpacker $ args !! 1
                               return $ Bool $ left `op` right 



-- Utility functions for evaluating binary boolean operations.
-- These functions curry the above boolBinop function with
-- different unpacking functions. 
numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal 
numBoolBinop = boolBinop unpackNum 


boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal 
boolBoolBinop = boolBinop unpackBool 


strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal 
strBoolBinop = boolBinop unpackStr 



-- Get the first element from a list 
car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)] = return x
car [DottedList (x:_) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList 


-- Get the remainder of a list 
cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList [_] x] = return x 
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList 


-- Combine a value with a list
cons :: [LispVal] -> ThrowsError LispVal 
cons [a, List []] = return $ List [a] 
cons [a, List xs] = return $ List (a:xs) 
cons [a, DottedList xs x] = return $ DottedList (a:xs) x
cons [x, y] = return $ DottedList [x] y 
cons badArgList = throwError $ NumArgs 2 badArgList 


