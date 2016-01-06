

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Monad.Error 

import Lex
import Eval


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
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled 


--main = getArgs >>= print . eval . readExpr . head 


