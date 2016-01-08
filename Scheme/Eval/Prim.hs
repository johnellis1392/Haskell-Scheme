

module Scheme.Eval.Prim where

import Text.ParserCombinators.Parsec
import Control.Monad.Error 

import Scheme.Lex.LispVal
import Scheme.Eval.LispError
import Scheme.Eval.List
import Scheme.Eval.Comp
import Scheme.Env 


-- A list of default functions. These values
-- map a string name to a function mapping some
-- list of arguments to a new value.
-- The function lookup will look up some string
-- in a list of tuples like this, and return the
-- function associated with the given string. 
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("string?", is_string),
              ("number?", is_number),
              ("symbol?", is_symbol),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)), 
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eqv?", eqv),
              ("equal?", equal)] 



