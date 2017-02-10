
module Mental.Error where

import Mental.Tree
import Mental.Type

data TypeError
  = ValueNotFound VarName
  | UnificationError Ty Ty
  | InfiniteTypeError Ty Ty

data EvalError
  = NoRuleApplies Tree
  | VarNotInScope (Name Tree)

