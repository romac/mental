
module Main where

import           Protolude

import           System.Environment (getArgs)
import           Mentalist.REPL     (runREPL, interactive, noninteractive)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> runREPL (noninteractive file)
    _      -> runREPL interactive

