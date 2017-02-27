{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

-- FIXME: Figure out a way to make `eval` and `evalPrim` prettier

module Mental.Eval
  ( evalUntypedTree
  , traceEvalUntypedTree
  , EvalError(..)
  ) where

import           Protolude hiding (head, for)


import           Data.Functor.Foldable (project, embed)
import           Data.List.NonEmpty
import qualified Data.Map as Map

import           Mental.Error
import           Mental.Name
import           Mental.Primitive
import           Mental.Tree
import           Mental.Tree.Untyped

type EvalResult = Either EvalError

type Env = Map VarName Tree

newtype Eval a = Eval (ReaderT Env (Except EvalError) a)
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadError EvalError)

runEval :: Eval a -> EvalResult a
runEval (Eval x) = runExcept (runReaderT x (Map.singleton (mkName "test") (embed (IntLit 42))))

evalUntypedTree :: UntypedTree -> EvalResult Tree
evalUntypedTree tree = last <$> traceEvalUntypedTree tree

traceEvalUntypedTree :: UntypedTree -> EvalResult (NonEmpty Tree)
traceEvalUntypedTree tree =
  case runEval (path (unAnnotateTree tree) eval) of
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

-- subst :: VarName -> Tree -> Tree -> Tree
-- subst var for tree | Set.member var (treeFv tree) = para alg tree
--   where alg (o, m) |

subst :: VarName -> Tree -> Tree -> Tree
subst var for tree =
  let s = subst var for
   in case project tree of

      Var name | name == var ->
        for

      Let x tp v body | x == var ->
        embed (Let x tp (s v) body)

      Let x tp v body ->
        embed (Let x tp (s v) (s body))

      Abs _ (x, _) | x == var ->
        tree

      Abs tp (x, body) ->
        embed (Abs tp (x, s body))

      App f x ->
        embed (App (s f) (s x))

      Pair a b ->
        embed (Pair (s a) (s b))

      If cnd thn els ->
        embed (If (s cnd) (s thn) (s els))

      _ -> tree

eval :: Tree -> Eval Tree
eval tree =
  case project tree of
    Var name -> do
      value <- asks (Map.lookup name)
      maybe (throwError (VarNotInScope name)) pure value

    If (project -> Tru) thn _ ->
      pure thn

    If (project -> Tru) _ els ->
      pure els

    Let x _ v body | isValue v ->
      -- local (Map.insert x v) (eval body)
      pure $ subst x v body

    App (project -> Abs _ (x, body)) v | isValue v ->
      -- local (Map.insert x v) (eval body)
      pure $ subst x v body

    App (project -> Prim prim) v | isValue v ->
      evalPrim prim v

    App f x | isValue f -> do
      x' <- eval x
      pure $ embed (App f x')

    App f x -> do
      f' <- eval f
      pure $ embed (App f' x)

    Let x tp v body -> do
      v' <- eval v
      pure $ embed (Let x tp v' body)

    If cnd thn els -> do
      cnd' <- eval cnd
      pure $ embed (If cnd' thn els)

    Pair a b | isValue a -> do
      b' <- eval b
      pure $ embed (Pair a b')

    Pair a b -> do
      a' <- eval a
      pure $ embed (Pair a' b)

    t ->
      throwError (NoRuleApplies (embed t))

-- evalPrim IsZero Zero                          = pure Tru
-- evalPrim IsZero (PrimApp Succ IsNumericValue) = pure Fals
-- evalPrim Pred (PrimApp Succ t@IsNumericValue) = pure t

evalPrim :: Primitive -> Tree -> Eval Tree
evalPrim PFirst (project -> Pair a _) =
  pure a

evalPrim PSecond (project -> Pair _ b) =
  pure b

evalPrim PFix t@(project -> Abs _ (x, body)) =
  pure $ (error "subst") x (embed (App (embed (Prim PFix)) t)) body

evalPrim prim t =
  throwError $ NoRuleApplies (embed (App (embed (Prim prim)) t))

