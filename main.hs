

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Monad.Error
import System.IO 

import Scheme


-- Use parseExpr to parse the given input 
--readExpr :: String -> String
--readExpr input = case parse Lex.parseExpr "lisp" input of
--  Left err  -> "No match: " ++ show err
--  Right val -> "Found value: " ++ show val

-- Parse the expression and return it to the calling
-- expression 
--readExpr :: String -> LispVal
--readExpr input = case parse parseExpr "lisp" input of
--  Left err  -> String $ "No Match: " ++ show err
--  Right val -> val


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
evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval) 


-- Evaluate a string and print the result
evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn


-- Self-generated infinite loop
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action 


-- Run a reply INFINITLEY 
runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp >>> ") evalAndPrint 




-- New version of readExpr that handles errors 
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> throwError $ Parser err
  Right val -> return val 


-- Evaluate values passed from the command line 
--main = do
--  (expr:_) <- getArgs
--  putStrLn $ readExpr expr

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> evalAndPrint $ args !! 0
    otherwise -> putStrLn $ "Program takes only 1 or 0 arguments" 


--main = do
--  args <- getArgs
--  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
--  putStrLn $ extractValue $ trapError evaled 


--main = getArgs >>= print . eval . readExpr . head 


