

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



