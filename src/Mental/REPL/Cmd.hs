
module Mental.REPL.Cmd where

import Protolude hiding (try)

import qualified Data.Text as T

import Text.Megaparsec
import Text.Megaparsec.Text

data Cmd
  = CmdQuit
  | CmdLoad FilePath
  | CmdType Text
  | CmdUnknown Text
  | CmdNone
  deriving (Eq, Show)

parseCmd :: Text -> Cmd
parseCmd input | T.null input = CmdNone
parseCmd input = case parse cmdParser "" input of
                   Left _    -> CmdUnknown input
                   Right cmd -> cmd

cmdParser :: Parser Cmd
cmdParser = cmdQuit <|> cmdLoad <|> cmdType

cmdQuit, cmdLoad, cmdType :: Parser Cmd
cmdQuit = do
  void $ string "quit" <|> string "q"
  space
  eof
  pure CmdQuit

cmdLoad = do
  void $ string "load" <|> string "l"
  space
  cmd <- CmdLoad <$> pathParser
  space
  eof
  pure cmd

cmdType = do
  void $ string "type" <|> string "t"
  space
  expr <- T.pack <$> many anyChar
  space
  eof
  pure (CmdType expr)

pathParser :: Parser FilePath
pathParser = some (noneOf ['\t', ' '])

