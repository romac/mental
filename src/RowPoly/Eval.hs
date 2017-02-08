{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module RowPoly.Eval
  ( eval
  , traceEval
  , EvalError(..)
  ) where

import Protolude hiding (head)

import Control.Monad.Except
import Data.List.NonEmpty

import Unbound.Generics.LocallyNameless

import RowPoly.Tree

data EvalError
  = NoRuleApplies Tree
  | VarNotInScope (Name Tree)

type EvalResult = Either EvalError

newtype EvalM a = EvalM (ExceptT EvalError FreshM a)
  deriving (Functor, Applicative, Monad, Fresh, MonadError EvalError)

runEvalM :: EvalM a -> EvalResult a
runEvalM (EvalM x) = runFreshM (runExceptT x)

eval :: Tree -> EvalResult Tree
eval tree = last <$> traceEval tree

traceEval :: Tree -> EvalResult (NonEmpty Tree)
traceEval tree =
  case runEvalM (path tree eval') of
    Right ts               -> Right ts
    Left (NoRuleApplies t) -> Right (t :| [])
    Left err               -> Left err

path :: Tree -> (Tree -> EvalM Tree) -> EvalM (NonEmpty Tree)
path t f = do
  t' <- f t
  ts <- path t' f
  pure (t <| ts)
  `catchError` handleError
  where
    handleError :: EvalError -> EvalM (NonEmpty Tree)
    handleError (NoRuleApplies _) = pure (t :| [])
    handleError err               = throwError err

isValue :: Tree -> Bool
isValue Tru            = True
isValue Fals           = True
isValue (Abs _ _)      = True
isValue v              = isNumericValue v

isNumericValue :: Tree -> Bool
isNumericValue Zero     = True
isNumericValue (Succ t) = isNumericValue t
isNumericValue _        = False

pattern IsValue :: Tree
pattern IsValue <- (isValue -> True)

pattern IsNumericValue :: Tree
pattern IsNumericValue <- (isNumericValue -> True)

eval' :: Tree -> EvalM Tree
eval' (Var name)                     = throwError (VarNotInScope name)
eval' (IsZero Zero)                  = pure Tru
eval' (IsZero (Succ IsNumericValue)) = pure Fals
eval' (Pred (Succ t@IsNumericValue)) = pure t
eval' (If Tru thn _)                 = pure thn
eval' (If Fals _ els)                = pure els
eval' (Let _ v@IsValue bnd) = do
  (x, body) <- unbind bnd
  pure $ subst x v body

eval' (App (Abs _ bnd) v@IsValue) = do
  (x, body) <- unbind bnd
  pure $ subst x v body

eval' (App f@IsValue x) = App f  <$> eval' x
eval' (App f x)         = App    <$> eval' f <*> pure x
eval' (Let tp v bnd)    = Let tp <$> eval' v <*> pure bnd
eval' (IsZero t)        = IsZero <$> eval' t
eval' (Succ t)          = Succ   <$> eval' t
eval' (Pred t)          = Pred   <$> eval' t
eval' (If cnd thn els)  = If     <$> eval' cnd <*> pure thn <*> pure els

eval' t = throwError (NoRuleApplies t)

