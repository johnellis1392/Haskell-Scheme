
module Eval where

import Text.ParserCombinators.Parsec hiding (spaces) 
import Control.Monad.Error 
import Lex 


-- Data type for handling Errors 
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String 


-- Function for evaluating a lisp error
showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values \"" ++ unwordsList found ++ "\"" 
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

-- Declare LispError type as being showable 
instance Show LispError where show = showError 


-- Have LispError type extend the error type class 
instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default 


-- Type for returning errors. Type constructors are
-- curried just like functions, so we can use ThrowsError
-- to construct a new Either type. 
type ThrowsError = Either LispError 


-- Trap an error 
trapError action = catchError action (return . show) 


-- Extract a value from an Either
extractValue :: ThrowsError a -> a
extractValue (Right val) = val 


-- Eval function for evaluating data types.
-- Here, the val@(String _) value binds "val" to
-- the whole (String _) expression instead of
-- unpacking the expression and binding a value
-- to its contents. In the event that we encounter
-- a piece of generic data, we just return the
-- value.
-- The last pattern here matches an entire list
-- of values recursively.
--
-- mapM maps a monadic function over a list of monadic
-- values. 
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val 
--eval (List (Atom f : args)) = apply f $ map eval args
eval (List (Atom f : args)) = mapM eval args >>= apply f 
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm 


-- Apply a function to a set of arguments.
-- The maybe function here is told to try to
-- evaluate the given lookup function, and if
-- something is found, apply it to the given
-- function ($ args), if not then return a
-- boolean false value. 
apply :: String -> [LispVal] -> ThrowsError LispVal
--apply f args = maybe (Bool False) ($ args) $ lookup f primitives
apply f args = maybe
               (throwError $ NotFunction "Unrecognized primitive function args" f)
               ($ args)
               (lookup f primitives) 


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

