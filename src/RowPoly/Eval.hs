{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module RowPoly.Eval
  ( eval
  , traceEval
  ) where

import Protolude hiding (head)

import Control.Monad.Except
import Data.List.NonEmpty

import Unbound.Generics.LocallyNameless

import RowPoly.Tree

data EvalError
 = NoRuleApplies Tree

newtype EvalM a = EvalM (ExceptT EvalError FreshM a)
  deriving (Functor, Applicative, Monad, Fresh, MonadError EvalError)

runEvalM :: EvalM a -> Either EvalError a
runEvalM (EvalM x) = runFreshM (runExceptT x)

eval :: Tree -> Tree
eval tree = last (traceEval tree)

traceEval :: Tree -> NonEmpty Tree
traceEval tree =
  case runEvalM (path tree eval') of
    Right ts               -> ts
    Left (NoRuleApplies t) -> t :| []

path :: Tree -> (Tree -> EvalM Tree) -> EvalM (NonEmpty Tree)
path t f = do
  t' <- f t
  ts <- path t' f
  pure (t <| ts)
  `catchError` done
  where
    done (NoRuleApplies _) = pure (t :| [])

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
eval' (IsZero Zero)                  = pure Tru
eval' (IsZero (Succ IsNumericValue)) = pure Fals
eval' (Pred (Succ t@IsNumericValue)) = pure t
eval' (If Tru thn _)                 = pure thn
eval' (If Fals _ els)                = pure els

eval' (App (Abs _ bnd) v@IsValue) = do
  (x, body) <- unbind bnd
  pure $ subst x v body

eval' (App f@IsValue x) = App f  <$> eval' x
eval' (App f x)         = App    <$> eval' f <*> pure x
eval' (IsZero t)        = IsZero <$> eval' t
eval' (Succ t)          = Succ   <$> eval' t
eval' (Pred t)          = Pred   <$> eval' t
eval' (If cnd thn els)  = If     <$> eval' cnd <*> pure thn <*> pure els

eval' t = throwError (NoRuleApplies t)

