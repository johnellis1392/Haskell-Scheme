

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Monad.Error
import System.IO 
import Data.IORef 

import Scheme

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> runOne $ args !! 0 
    otherwise -> putStrLn $ "Program takes only 1 or 0 arguments" 



