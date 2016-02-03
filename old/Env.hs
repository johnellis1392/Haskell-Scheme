

-- module Scheme.Env where

-- import Control.Monad.Error
-- import System.IO
-- import Data.IORef
-- import Scheme.Eval.LispError
-- import Scheme.Lex.LispVal


-- -- IORef is a datatype that defines a mutable variable.
-- -- Because the state monad is complicated and life is short. 
-- type Env = IORef [(String, IORef LispVal)]


-- -- This function instantiates a new instance of an
-- -- Environment. The newIORef function creates an IORef
-- -- object encapsulated inside an IO object, so the type
-- -- declaration of this function returns an IO object
-- -- encasulating the Environment. 
-- nullEnv :: IO Env 
-- nullEnv = newIORef [] 


-- -- A Monad Transformer is a type that allows us to use
-- -- two different monads together simulatneously. ErrorT
-- -- is a Monad Transformer that combines Errors with IO.
-- type IOThrowsError = ErrorT LispError IO


-- -- This function takes a value of a ThrowsError type and
-- -- converts it into an IOThrowsError. We can transform a
-- -- simple IO Monad into an IOThrowsError by simply lifting
-- -- it, but we need to do something more complicated to
-- -- properly handle this new error type.
-- liftThrows :: ThrowsError a -> IOThrowsError a
-- liftThrows (Left  err) = throwError err
-- liftThrows (Right val) = return val


-- -- Run an action and catch error. 
-- runIOThrows :: IOThrowsError String -> IO String
-- runIOThrows action = runErrorT (trapError action) >>= return . extractValue 


-- -- Check if a particular value is bound.
-- isBound :: Env -> String -> IO Bool
-- isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var 


-- -- Retrieve a value 
-- getVar :: Env -> String -> IOThrowsError LispVal
-- getVar envRef var = do
--   env <- liftIO $ readIORef envRef
--   maybe (throwError $ UnboundVar "Getting an unbound variable" var)
--     (liftIO . readIORef)
--     (lookup var env)


-- -- Set a value in the environment 
-- setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
-- setVar envRef var value = do
--   env <- liftIO $ readIORef envRef
--   maybe (throwError $ UnboundVar "Setting an unbound variable" var)
--     (liftIO . (flip writeIORef value))
--     (lookup var env)
--   return value


-- -- Define a new variable
-- defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
-- defineVar envRef var value = do
--   alreadyDefined <- liftIO $ isBound envRef var
--   if alreadyDefined
--     then setVar envRef var value >> return value
--     else liftIO $ do
--          valueRef <- newIORef value
--          env <- readIORef envRef
--          writeIORef envRef ((var, valueRef) : env)
--          return value


-- -- Bind a handful of vars en masse. 
-- bindVars :: Env -> [(String, LispVal)] -> IO Env
-- bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
--   where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
--         addBinding (var, value) = do
--           ref <- newIORef value
--           return (var, ref) 



