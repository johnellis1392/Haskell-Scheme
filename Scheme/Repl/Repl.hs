

module Scheme.Repl where

import Text.ParserCombinators.Parsec hiding (spaces) 
import Control.Monad
import Control.Monad.Error 
import System.IO
import Scheme.Eval
import Scheme.Lex 
import Scheme.Env 


-- New version of readExpr that handles errors 
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> throwError $ Parser err
  Right val -> return val 


-- Flush output with some string 
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout 


-- Prompt the user then read in a string and return 
readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine 


-- Read a string and evaluate it. First we read an
-- expression using readExpr, then we convert it into
-- a string and lift it into a monad using liftM. Then
-- we bind it to eval, so that it gets evaluated, and
-- subsequently we catch the error, extract the value,
-- and return an IO monad encapsulating the string.
-- (Self-explanatory except I wanted to explain it
-- to myslef). 
evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env 


-- Evaluate a string and print the result
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn


-- Self-generated infinite loop
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action 


-- Run one expression
runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr 


-- Run a reply INFINITLEY 
runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "Lisp >>> ") . evalAndPrint 




