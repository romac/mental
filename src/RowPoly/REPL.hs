
module RowPoly.REPL (repl) where

import qualified Data.Text as T
-- import qualified Data.Text.IO as T
import           Text.Megaparsec (parse, parseErrorPretty)
import           System.Console.Readline (readline, addHistory)
import           Text.PrettyPrint.Leijen.Text (putDoc)

import RowPoly.Parser      (parser)
import RowPoly.PrettyPrint (prettyPrint)

doCmd :: String -> IO ()
doCmd "quit" = putStrLn "" >> return ()
doCmd "q"    = putStrLn "" >> return ()
doCmd ""     = putStrLn "please specific a command" >> repl
doCmd s      = putStrLn ("unknown command '" ++ s ++ "'") >> repl

parseAndPrint :: String -> IO ()
parseAndPrint code =
  case parse parser "<repl>" (T.pack code) of
    Left err  -> putStr (parseErrorPretty err)
    Right res -> do
      putDoc (prettyPrint res)
      putStrLn ""

repl :: IO ()
repl = do
  maybeLine <- readline "% "
  case maybeLine of
    Nothing ->
      return ()

    Just "" ->
      repl

    Just (':' : cmd) ->
      doCmd cmd

    Just line -> do
      addHistory line
      parseAndPrint line
      repl

