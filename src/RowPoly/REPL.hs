
module RowPoly.REPL (repl) where

import qualified Data.Text as T
import           Text.Megaparsec (parseTest)
import           System.Console.Readline (readline, addHistory)

import RowPoly.Parser (parser)

doCmd :: String -> IO ()
doCmd "quit" = return ()
doCmd "q"    = return ()

parseAndPrint :: String -> IO ()
parseAndPrint code =
  parseTest parser (T.pack code)

repl :: IO ()
repl = do
  maybeLine <- readline "λ "
  case maybeLine of
    Nothing ->
      return ()

    Just (':' : cmd) ->
      doCmd cmd

    Just line -> do
      addHistory line
      parseAndPrint line
      repl

