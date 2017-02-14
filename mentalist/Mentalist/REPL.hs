{-# LANGUAGE OverloadedStrings #-}

module Mentalist.REPL (runREPL) where

import           Protolude

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import           Text.Megaparsec              (parse, parseErrorPretty)
import           Text.Megaparsec.Text         (Parser)
import           System.Console.Readline      (readline, addHistory)
import           Text.PrettyPrint.Leijen.Text (Doc, putDoc)

import           Mental.Decl                  (Module)
import           Mental.Tree                  (Tree)
import           Mental.Parser                (termParser, moduleParser)
import           Mental.PrettyPrint           (prettyModule, prettyTree, prettyTy, prettyEvalError, prettyTypeError)
import           Mental.Eval                  (traceEvalTree)
import           Mental.Infer                 (inferTree, inferModule)

import           Mentalist.REPL.Cmd           (Cmd( ..), parseCmd)

type REPL a = IO a

newline :: REPL ()
newline = T.putStrLn ""

outputPretty :: Doc -> REPL ()
outputPretty d = putDoc d >> newline

header :: Text -> REPL ()
header h = do
  newline
  putStrLn $ "=== " <> h <> ": ==="

execCmd :: Cmd -> REPL ()
execCmd CmdQuit          = pure ()
execCmd CmdNone          = noCmd >> runREPL
execCmd (CmdUnknown cmd) = unknownCmd cmd >> runREPL
execCmd (CmdLoad path)   = loadFileCmd path >> runREPL
execCmd (CmdType expr)   = showTypeCmd expr >> runREPL

noCmd :: REPL ()
noCmd = T.putStrLn "please specific a command"

unknownCmd :: Text -> REPL ()
unknownCmd cmd = T.putStrLn ("unknown command '" <> cmd <> "'")

showTypeCmd :: Text -> REPL ()
showTypeCmd code = do
  tree <- parseCode termParser "<interactive>" code
  forM_ tree runInferTree

loadFileCmd :: FilePath -> REPL ()
loadFileCmd path = do
  code <- T.readFile path
  parsePrintEvalModule path code

parseCode :: Parser a -> FilePath -> Text -> REPL (Maybe a)
parseCode withParser file code =
  case parse withParser file code of
    Left err -> do
      putStr (parseErrorPretty err)
      pure Nothing

    Right res ->
      pure (Just res)

evalTree :: Tree -> REPL ()
evalTree tree =
  case traceEvalTree tree of
    Left err    -> outputPretty (prettyEvalError err)
    Right steps -> forM_ steps (outputPretty . prettyTree)

runInferTree :: Tree -> REPL ()
runInferTree tree =
  case inferTree tree of
    Left err -> outputPretty (prettyTypeError err)
    Right ty -> outputPretty (prettyTy ty)

runInferModule :: Module -> REPL () -> REPL ()
runInferModule mod' onSuccess =
  case inferModule mod' of
    Left err -> outputPretty (prettyTypeError err)
    Right () -> onSuccess

parsePrintEvalTerm :: FilePath -> Text -> REPL ()
parsePrintEvalTerm file code = do
  maybeTree <- parseCode termParser file code
  forM_ maybeTree $ \tree -> do
    header "Parsed"
    outputPretty (prettyTree tree)

    header "Type"
    runInferTree tree

    header "Evaluation"
    evalTree tree

    newline

parsePrintEvalModule :: FilePath -> Text -> REPL ()
parsePrintEvalModule file code = do
  maybeMod <- parseCode moduleParser file code
  forM_ maybeMod $ \mod' -> do
    header "Parsed"
    outputPretty (prettyModule mod')

    runInferModule mod' (header "Typechecks!")

runREPL :: REPL ()
runREPL = runREPL' $ do
  maybeLine <- readline "Mental> "
  case maybeLine of
    Nothing ->
      newline

    Just "" ->
      runREPL

    Just line@(':' : cmd) -> do
      addHistory line
      execCmd (parseCmd (T.pack cmd))

    Just line -> do
      addHistory line
      parsePrintEvalTerm "<repl>" (T.pack line)
      runREPL

  where runREPL' = identity

