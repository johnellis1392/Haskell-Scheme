

module Scheme.Eval.LispError where

import Scheme.Lex.LispVal
import Control.Monad.Error
import Text.ParserCombinators.Parsec 


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


