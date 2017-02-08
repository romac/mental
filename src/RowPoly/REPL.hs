
module RowPoly.REPL (runREPL) where

import           Protolude

import qualified Data.Text as T
import           Text.Megaparsec (parse, parseErrorPretty)
import           System.Console.Readline (readline, addHistory)
import           Text.PrettyPrint.Leijen.Text (putDoc)

import           RowPoly.Parser      (parser)
import           RowPoly.PrettyPrint (prettyPrint)
import           RowPoly.Eval        (traceEval)


doCmd :: [Char] -> IO ()
doCmd "quit" = putStrLn ""
doCmd "q"    = putStrLn ""
doCmd ""     = putStrLn "please specific a command" >> runREPL
doCmd s      = putStrLn ("unknown command '" ++ s ++ "'") >> runREPL

parsePrintEval:: Text -> IO ()
parsePrintEval code =
  case parse parser "<repl>" code of
    Left err  -> putStr (parseErrorPretty err)
    Right tree -> do
      putStrLn "=== Parsed: ==="
      outputPretty tree
      putStrLn "=== Evaluation: ==="
      forM_ (traceEval tree) outputPretty
  where
    outputPretty t = putDoc (prettyPrint t) >> putStrLn ""

runREPL :: IO ()
runREPL = do
  maybeLine <- readline "> "
  case maybeLine of
    Nothing          -> return ()
    Just ""          -> runREPL
    Just (':' : cmd) -> doCmd cmd

    Just line -> do
      addHistory line
      parsePrintEval (T.pack line)
      runREPL

