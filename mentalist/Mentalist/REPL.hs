{-# LANGUAGE OverloadedStrings #-}

module Mentalist.REPL (runREPL) where

import           Protolude

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import           Text.Megaparsec              (parse, parseErrorPretty)
import           System.Console.Readline      (readline, addHistory)
import           Text.PrettyPrint.Leijen.Text (Doc, putDoc)

import           Mental.Tree                  (Tree)
import           Mental.Parser                (parser)
import           Mental.PrettyPrint           (prettyTree, prettyType, prettyEvalError, prettyTypeError)
import           Mental.Eval                  (traceEval)
import           Mental.Infer                 (infer)

import           Mentalist.Cmd                (Cmd( ..))
import           Mentalist.CmdParser          (parseCmd)

newline :: IO ()
newline = T.putStrLn ""

outputPretty :: Doc -> IO ()
outputPretty d = putDoc d >> newline

header :: Text -> IO ()
header h = do
  newline
  putStrLn $ "=== " <> h <> ": ==="

doCmd :: Cmd -> IO ()
doCmd CmdQuit          = pure ()
doCmd CmdNone          = T.putStrLn "please specific a command" >> runREPL
doCmd (CmdUnknown cmd) = T.putStrLn ("unknown command '" <> cmd <> "'") >> runREPL
doCmd (CmdLoad path)   = loadFile path >> runREPL
doCmd (CmdType expr)   = showType expr >> runREPL

over :: Applicative f => Maybe a -> (a -> f ()) -> f ()
over Nothing _  = pure ()
over (Just x) f = f x

showType :: Text -> IO ()
showType code = do
  tree <- parseCode "<repl>" code
  over tree inferTree

loadFile :: FilePath -> IO ()
loadFile path = do
  code <- T.readFile path
  parsePrintEval path code

parseCode :: FilePath -> Text -> IO (Maybe Tree)
parseCode file code =
  case parse parser file code of
    Left err -> do
      putStr (parseErrorPretty err)
      pure Nothing

    Right tree ->
      pure (Just tree)

evalTree :: Tree -> IO ()
evalTree tree =
  case traceEval tree of
    Left err    -> outputPretty (prettyEvalError err)
    Right steps -> forM_ steps (outputPretty . prettyTree)

inferTree :: Tree -> IO ()
inferTree tree =
  case infer tree of
    Left err -> outputPretty (prettyTypeError err)
    Right ty -> outputPretty (prettyType ty)

parsePrintEval :: FilePath -> Text -> IO ()
parsePrintEval file code = do
  maybeTree <- parseCode file code
  over maybeTree $ \tree -> do
    header "Parsed"
    outputPretty (prettyTree tree)

    header "Evaluation"
    evalTree tree

    header "Type"
    inferTree tree

runREPL :: IO ()
runREPL = do
  maybeLine <- readline "> "
  case maybeLine of
    Nothing ->
      newline

    Just "" ->
      runREPL

    Just (':' : cmd) -> do
      addHistory (':' : cmd)
      doCmd (parseCmd (T.pack cmd))

    Just line -> do
      addHistory line
      parsePrintEval "<repl>" (T.pack line)
      newline
      runREPL

