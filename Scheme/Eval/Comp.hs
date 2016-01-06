

module Scheme.Eval.Comp where

import Text.ParserCombinators.Parsec
import Control.Monad.Error 

import Scheme.Lex.LispVal
import Scheme.Eval.LispError
import Scheme.Eval.List


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



-- Dynamic equality comparison 
eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool x), (Bool y)] = return $ Bool $ x == y
eqv [(Number x), (Number y)] = return $ Bool $ x == y
eqv [(String x), (String y)] = return $ Bool $ x == y
eqv [(Atom x), (Atom y)] = return $ Bool $ x == y
eqv [(DottedList x xs), (DottedList y ys)] = eqv [List $ x ++ [xs], List $ y ++ [ys]]
eqv [(List x), (List y)] = return $ Bool $ (length x == length y) && (all eqvPair $ zip x y)
  where eqvPair (x', y') = case eqv [x', y'] of
          Left err -> False
          Right (Bool val) -> val

eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList 





