{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mental.Eval
  ( evalTree
  , traceEvalTree
  , EvalError(..)
  ) where

import           Protolude hiding (head)

import           Data.List.NonEmpty

import           Unbound.Generics.LocallyNameless

import           Mental.Tree
import           Mental.Error
import           Mental.Primitive

type EvalResult = Either EvalError

data EvalState
  = EvalState
    { _treeEnv :: Map VarName Tree
    }

instance Monoid EvalState where
  mempty = EvalState mempty
  EvalState a `mappend` EvalState b = EvalState (a `mappend` b)

newtype Eval a = Eval (StateT EvalState (ExceptT EvalError FreshM) a)
  deriving (Functor, Applicative, Monad, Fresh, MonadState EvalState, MonadError EvalError)

runEval :: Eval a -> EvalResult a
runEval (Eval x) = runFreshM (runExceptT (evalStateT x mempty))

evalTree :: Tree -> EvalResult Tree
evalTree tree = last <$> traceEvalTree tree

traceEvalTree :: Tree -> EvalResult (NonEmpty Tree)
traceEvalTree tree =
  case runEval (path tree eval) of
    Right ts               -> Right ts
    Left (NoRuleApplies t) -> Right (t :| [])
    Left err               -> Left err

path :: Tree -> (Tree -> Eval Tree) -> Eval (NonEmpty Tree)
path t f = do
  t' <- f t
  ts <- path t' f
  pure (t <| ts)
  `catchError` handleError
  where
    handleError :: EvalError -> Eval (NonEmpty Tree)
    handleError (NoRuleApplies _) = pure (t :| [])
    handleError err               = throwError err

eval :: Tree -> Eval Tree
eval (Var name) =
  throwError (VarNotInScope name)

eval (If Tru thn _) =
  pure thn

eval (If Fals _ els) =
  pure els

eval (Let _ v@IsValue bnd) = do
  (x, body) <- unbind bnd
  pure $ subst x v body

eval (App (Abs _ bnd) v@IsValue) = do
  (x, body) <- unbind bnd
  pure $ subst x v body

eval (PrimApp prim v@IsValue) =
  evalPrim prim v

eval (App f@IsValue x)   = App f  <$> eval x
eval (App f x)           = App    <$> eval f <*> pure x
eval (Let tp v bnd)      = Let tp <$> eval v <*> pure bnd
eval (If cnd thn els)    = If     <$> eval cnd <*> pure thn <*> pure els

eval t =
  throwError (NoRuleApplies t)

evalPrim :: Primitive -> Tree -> Eval Tree
evalPrim IsZero Zero                          = pure Tru
evalPrim IsZero (PrimApp Succ IsNumericValue) = pure Fals
evalPrim Pred (PrimApp Succ t@IsNumericValue) = pure t

evalPrim Fix t@(Abs _ bnd) = do
  (x, body) <- unbind bnd
  pure $ subst x (PrimApp Fix t) body

evalPrim prim t =
  throwError (NoRuleApplies (PrimApp prim t))

