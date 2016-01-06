
module Scheme.Eval.EvalUtil where

--import Text.ParserCombinators.Parsec
import Control.Monad.Error 
import Scheme.Lex.LispVal
import Scheme.Eval.LispError
import Scheme.Eval.Prim 


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

-- Evaluate block for If Statement 
eval (List [Atom "if", pred, conseq, alt]) = do
  result <- eval pred
  case result of
    Bool False -> eval alt
    otherwise  -> eval conseq 

-- Eval's for list operations 
eval (List (Atom "car" : args)) = mapM eval args >>= car 
eval (List (Atom "cdr" : args)) = mapM eval args >>= cdr
eval (List (Atom "cons" : args)) = mapM eval args >>= cons 

-- Apply a function call to a list of arguments 
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


