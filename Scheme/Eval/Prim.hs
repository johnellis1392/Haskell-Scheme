

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
              ("symbol?", is_symbol)] 



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
--unpackNum _ = 0

--unpackNum (String s) = let parsed = reads s :: [(Integer, String)]
--                       in if null parsed
--                          then 0
--                          else fst $ parsed !! 0
--unpackNum (List [n]) = unpackNum n



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



