{-# LANGUAGE OverloadedStrings #-}

module Mentalist.REPL (runREPL) where

import           Protolude

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import           Text.Megaparsec              (parse, parseErrorPretty)
import           Text.Megaparsec.Text         (Parser)
import           System.Console.Readline      (readline, addHistory)
import           Text.PrettyPrint.Leijen.Text (Doc, putDoc)

import           Mental.Tree                  (Tree)
import           Mental.Parser                (termParser, moduleParser)
import           Mental.PrettyPrint           (prettyModule, prettyTree, prettyTy, prettyEvalError, prettyTypeError)
import           Mental.Eval                  (traceEval)
import           Mental.Infer                 (inferType)

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
  tree <- parseCode termParser "<repl>" code
  over tree inferTree

loadFile :: FilePath -> IO ()
loadFile path = do
  code <- T.readFile path
  parsePrintEvalModule path code

parseCode :: Parser a -> FilePath -> Text -> IO (Maybe a)
parseCode withParser file code =
  case parse withParser file code of
    Left err -> do
      putStr (parseErrorPretty err)
      pure Nothing

    Right res ->
      pure (Just res)

evalTree :: Tree -> IO ()
evalTree tree =
  case traceEval tree of
    Left err    -> outputPretty (prettyEvalError err)
    Right steps -> forM_ steps (outputPretty . prettyTree)

inferTree :: Tree -> IO ()
inferTree tree =
  case inferType tree of
    Left err -> outputPretty (prettyTypeError err)
    Right ty -> outputPretty (prettyTy ty)

parsePrintEvalTerm :: FilePath -> Text -> IO ()
parsePrintEvalTerm file code = do
  maybeTree <- parseCode termParser file code
  over maybeTree $ \tree -> do
    header "Parsed"
    outputPretty (prettyTree tree)

    header "Type"
    inferTree tree

    header "Evaluation"
    evalTree tree

parsePrintEvalModule :: FilePath -> Text -> IO ()
parsePrintEvalModule file code = do
  maybeMod <- parseCode moduleParser file code
  over maybeMod $ \mod' -> do
    header "Parsed"
    outputPretty (prettyModule mod')

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
      parsePrintEvalTerm "<repl>" (T.pack line)
      newline
      runREPL

