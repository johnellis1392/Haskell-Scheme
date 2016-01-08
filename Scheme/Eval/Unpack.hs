

module Scheme.Eval.Unpack where

import Control.Monad.Error 
import Scheme.Eval.LispError 
import Scheme.Lex.LispVal 
import Scheme.Env


-- Unpacker value
data Unpacker = forall a . Eq a => AnyUnpacker (LispVal -> ThrowsError a)


-- Simultaneously unpack two values and compare 
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals x1 x2 (AnyUnpacker unpacker) = do
  u1 <- unpacker x1
  u2 <- unpacker x2
  return $ (u1 == u2)
  `catchError` (const $ return False) 
  


-- Unpack a numerical argument. If the argument is
-- a string, then try to convert it to a string. 
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n
                       in if null parsed
                          then throwError $ TypeMismatch "number" $ String n
                          else return $ fst $ parsed !! 0 
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum 



-- Unpack a string
unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString



-- Unpack a Boolean
unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool 






