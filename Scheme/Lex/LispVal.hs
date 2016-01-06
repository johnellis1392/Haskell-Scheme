

module Scheme.Lex.LispVal where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces) 


-- Data types for lisp values 
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool 


-- Unpack contents of a list for showing values
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal 


-- Show value of a particular lisp type 
showVal :: LispVal -> String 
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")" 

-- Extend Show for LispVal's
instance Show LispVal where show = showVal 


