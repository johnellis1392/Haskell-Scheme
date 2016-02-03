

module Scheme.Lex.LispVal where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces) 
import Control.Monad.Error
import System.IO
import Data.IORef
-- import Scheme.Eval.LispError
-- import Scheme.Lex.LispVal



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










-- IORef is a datatype that defines a mutable variable.
-- Because the state monad is complicated and life is short. 
type Env = IORef [(String, IORef LispVal)]


-- This function instantiates a new instance of an
-- Environment. The newIORef function creates an IORef
-- object encapsulated inside an IO object, so the type
-- declaration of this function returns an IO object
-- encasulating the Environment. 
nullEnv :: IO Env 
nullEnv = newIORef [] 


-- A Monad Transformer is a type that allows us to use
-- two different monads together simulatneously. ErrorT
-- is a Monad Transformer that combines Errors with IO.
type IOThrowsError = ErrorT LispError IO


-- This function takes a value of a ThrowsError type and
-- converts it into an IOThrowsError. We can transform a
-- simple IO Monad into an IOThrowsError by simply lifting
-- it, but we need to do something more complicated to
-- properly handle this new error type.
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left  err) = throwError err
liftThrows (Right val) = return val


-- Run an action and catch error. 
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue 


-- Check if a particular value is bound.
isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var 


-- Retrieve a value 
getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)


-- Set a value in the environment 
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable" var)
    (liftIO . (flip writeIORef value))
    (lookup var env)
  return value


-- Define a new variable
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
         valueRef <- newIORef value
         env <- readIORef envRef
         writeIORef envRef ((var, valueRef) : env)
         return value


-- Bind a handful of vars en masse. 
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do
          ref <- newIORef value
          return (var, ref) 








