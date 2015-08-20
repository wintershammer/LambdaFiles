module Main where

import Syntax
import Parser
import Eval

import Control.Monad
import Control.Monad.Trans
import System.Console.Haskeline
import qualified Data.Map as Map


process :: String -> IO ()
process line = do
  let res = parseExpr line
  case res of
    Left err -> print err
    Right ex -> do
      out <- return $ runEval Map.empty (eval ex)
      print out

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "Untyped> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop