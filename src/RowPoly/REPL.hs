{-# LANGUAGE OverloadedStrings #-}

module RowPoly.REPL (runREPL) where

import           Protolude

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import           Text.Megaparsec (parse, parseErrorPretty)
import           System.Console.Readline (readline, addHistory)
import           Text.PrettyPrint.Leijen.Text (putDoc)

import           RowPoly.Parser      (parser)
import           RowPoly.PrettyPrint (prettyTree, prettyEvalError)
import           RowPoly.Eval        (traceEval)

newline :: IO ()
newline = T.putStrLn ""

doCmd :: [Char] -> IO ()
doCmd "quit" = pure ()
doCmd "q"    = pure ()
doCmd ""     = T.putStrLn "please specific a command" >> runREPL
doCmd s      = T.putStrLn ("unknown command '" <> T.pack s <> "'") >> runREPL

header :: Text -> IO ()
header h = do
  newline
  putStrLn $ "=== " <> h <> ": ==="

parsePrintEval:: Text -> IO ()
parsePrintEval code =
  case parse parser "<repl>" code of
    Left err  ->
      putStr (parseErrorPretty err)

    Right tree -> do
      header "Parsed"
      outputPretty (prettyTree tree)

      header "Evaluation"
      case traceEval tree of
        Left err    -> outputPretty (prettyEvalError err)
        Right steps -> forM_ steps (outputPretty . prettyTree)
  where
    outputPretty d = putDoc d >> newline

runREPL :: IO ()
runREPL = do
  maybeLine <- readline "> "
  case maybeLine of
    Nothing          -> newline
    Just ""          -> runREPL
    Just (':' : cmd) -> doCmd cmd

    Just line -> do
      addHistory line
      parsePrintEval (T.pack line)
      newline
      runREPL

