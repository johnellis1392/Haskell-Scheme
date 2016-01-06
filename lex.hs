

module Lex where 

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



-- Call parser and handle errors. 
-- Here, parse is a function that takes a parser
-- and parses the given input; "lisp" is the name
-- we have given to the parsed input (for errors)
-- parse :: Parser -> String -> String -> Either
-- where Either is a type that evaluates to one
-- of two values. 
-- readExpr :: String -> String
-- readExpr input = case parse symbol "lisp" input of
--   Left err  -> "No match: " ++ show err
--   Right val -> "Found value"


-- Here we use >> to bind together spcaes and symbol
-- in a strictly platonic way. 
-- readExpr :: String -> String
-- readExpr input = case parse (spaces >> symbol) "lisp" input of
--   Left err  -> "No match: " ++ show err
--   Right val -> "Found value"


-- Use parseExpr to parse the given input 
-- readExpr :: String -> String
-- readExpr input = case parse Lex.parseExpr "lisp" input of
--   Left err -> "No match: " ++ show err
--   Right _  -> "Found value" 


-- Define a parser for a generic symbol in scheme: 
-- creates a parser that matches any one of the
-- individual characters presented.
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~" 


-- Parser () is a type indicating a parser that
-- is parameterized with a void type.
-- skipMany1 takes one or more instances of a
-- particular lexeme. 
spaces :: Parser () 
spaces = skipMany1 space 


-- Parse a single quote or an escaped quote
parseQuote :: Parser Char 
parseQuote = char '"'
             <|> do char '\\'
                    char '"' 


-- Parse a string value:
-- ex: "String" 
parseString :: Parser LispVal
--parseString = do
--  char '"'
--  x <- many (noneOf "\"")
--  char '"'
--  return $ String x

parseString = do
  parseQuote
  x <- many (noneOf "\\\"")
  parseQuote
  return $ String x 


-- Function for parsing an atomic value
-- The <|> operator here is a try-parse that
-- attempts to evaluate the first expression,
-- and if it fails, moves on to the second
-- expression. 
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom 


-- Parse a number
-- liftM here "lifts" the value of the given function
-- into a monad, applies the value produces by the
-- second argument to the function, and turns the
-- result into a monad of the expected type.
-- This evaluates a string of digits, reads that string
-- into an integer, and then constructs a number type
-- based on that integer. 
parseNumber :: Parser LispVal
--parseNumber = liftM (Number . read) $ many1 digit 

-- parseNumber version using do-notation 
--parseNumber = do
--  n <- many1 digit
--  return $ (Number . read) n

-- Third version of parseNumber using bind notation 
parseNumber = many1 digit >>= \i -> return $ (Number . read) i


-- Parse either an atom or a string or a number 
parseExpr :: Parser LispVal
--parseExpr = parseAtom
--            <|> parseString
--            <|> parseNumber


-- New expression parser that takes lists into account
-- try is a function that tries to parse a particular
-- parser combinator and if it fails parses the expression
-- using the second value 
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber
            <|> parseQuoted
            <|> do char '('
                   x <- try parseList <|> parseDottedList
                   char ')'
                   return x 


-- Parse a list
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- Parse a dotted list
parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail


-- Parse a quoted expression by transforming it into
-- a quote function call 
parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]



