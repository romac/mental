
module Mentalist.Cmd where

import Protolude

data Cmd
  = CmdQuit
  | CmdLoad FilePath
  | CmdType Text
  | CmdUnknown Text
  | CmdNone
  deriving (Eq, Show)

