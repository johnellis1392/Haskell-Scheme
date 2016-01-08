
module Scheme.Eval.List where

import Control.Monad.Error 
import Scheme.Lex.LispVal
import Scheme.Eval.LispError
import Scheme.Env 


-- Get the first element from a list 
car :: [LispVal] -> IOThrowsError LispVal
car [List (x:_)] = return x
car [DottedList (x:_) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList 


-- Get the remainder of a list 
cdr :: [LispVal] -> IOThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList [_] x] = return x 
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList 


-- Combine a value with a list
cons :: [LispVal] -> IOThrowsError LispVal 
cons [a, List []] = return $ List [a] 
cons [a, List xs] = return $ List (a:xs) 
cons [a, DottedList xs x] = return $ DottedList (a:xs) x
cons [x, y] = return $ DottedList [x] y 
cons badArgList = throwError $ NumArgs 2 badArgList 





