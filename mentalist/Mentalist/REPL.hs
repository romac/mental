{-# LANGUAGE OverloadedStrings #-}

module Mentalist.REPL (runREPL) where

import           Protolude

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import           Text.Megaparsec              (parse, parseErrorPretty)
import           Text.Megaparsec.Text         (Parser)
import           Text.PrettyPrint.Leijen.Text (Doc)
import           System.Console.Haskeline

import           Mental.Decl                  (Module)
import           Mental.Tree                  (Tree)
import           Mental.Parser                (termParser, moduleParser)
import           Mental.PrettyPrint           (prettyModule, prettyTree, prettyTy, prettyEvalError, prettyTypeError)
import           Mental.Eval                  (traceEvalTree)
import           Mental.Infer                 (inferTree, inferModule)

import           Mentalist.REPL.Cmd           (Cmd( ..), parseCmd)

type REPL a = InputT IO a

outputText :: Text -> REPL ()
outputText = outputStr . T.unpack

outputTextLn :: Text -> REPL ()
outputTextLn = outputStrLn . T.unpack

outputNewline :: REPL ()
outputNewline = outputText "\n"

outputPretty :: Doc -> REPL ()
outputPretty = outputTextLn . show

header :: Text -> REPL ()
header h = do
  outputNewline
  outputTextLn $ "=== " <> h <> ": ==="

execCmd :: Cmd -> REPL ()
execCmd CmdQuit          = pure ()
execCmd CmdNone          = noCmd            >> repl
execCmd (CmdUnknown cmd) = unknownCmd cmd   >> repl
execCmd (CmdLoad path)   = loadFileCmd path >> repl
execCmd (CmdType expr)   = showTypeCmd expr >> repl

noCmd :: REPL ()
noCmd = outputTextLn "please specific a command"

unknownCmd :: Text -> REPL ()
unknownCmd cmd = outputTextLn $ "unknown command '" <> cmd <> "'"

showTypeCmd :: Text -> REPL ()
showTypeCmd code = do
  tree <- parseCode termParser "<interactive>" code
  forM_ tree runInferTree

loadFileCmd :: FilePath -> REPL ()
loadFileCmd path = do
  code <- liftIO $ T.readFile path
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
    Left err    -> outputPretty (prettyEvalError err) >> outputNewline
    Right steps -> forM_ steps (outputPretty . prettyTree)

runInferTree :: Tree -> REPL ()
runInferTree tree =
  case inferTree tree of
    Left err -> outputPretty (prettyTypeError err) >> outputNewline
    Right ty -> outputPretty (prettyTy ty)

runInferModule :: Module -> REPL () -> REPL ()
runInferModule mod' onSuccess =
  case inferModule mod' of
    Left err -> outputPretty (prettyTypeError err) >> outputNewline
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

    outputNewline

parsePrintEvalModule :: FilePath -> Text -> REPL ()
parsePrintEvalModule file code = do
  maybeMod <- parseCode moduleParser file code
  forM_ maybeMod $ \mod' -> do
    header "Parsed"
    outputPretty (prettyModule mod')

    runInferModule mod' (header "Typechecks!")

runREPL :: IO ()
runREPL = runInputT defaultSettings repl

repl :: REPL ()
repl = do
  maybeLine <- getInputLine "Mental> "
  case maybeLine of
    Nothing ->
      outputNewline

    Just "" ->
      repl

    Just (':' : cmd) -> do
      execCmd (parseCmd (T.pack cmd))

    Just line -> do
      parsePrintEvalTerm "<repl>" (T.pack line)
      repl

