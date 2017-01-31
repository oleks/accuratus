{-# LANGUAGE DeriveGeneric #-}

module Ast where

import Text.PrettyPrint.GenericPretty

data Lit
  = LitCon String
  | LitVar String
  deriving (Eq, Ord, Show, Generic)

instance Out Lit

data Exp a
  = ExpLit a
  | ExpAdd (Exp a) (Exp a)
  | ExpSub (Exp a) (Exp a)
  | ExpSum [Exp a]
  | ExpMul (Exp a) (Exp a)
  deriving (Eq, Ord, Show, Generic)

instance (Out a) => Out (Exp a)

instance Functor Exp where
  fmap f (ExpLit lit) = ExpLit (f lit)
  fmap f (ExpAdd l r) = ExpAdd (fmap f l) (fmap f r)
  fmap f (ExpSub l r) = ExpSub (fmap f l) (fmap f r)
  fmap f (ExpSum es)  = ExpSum (map (fmap f) es)
  fmap f (ExpMul l r) = ExpMul (fmap f l) (fmap f r)

instance Foldable Exp where
  foldMap f (ExpLit lit) = f lit
  foldMap f (ExpAdd l r) = mappend (foldMap f l) (foldMap f r)
  foldMap f (ExpSub l r) = mappend (foldMap f l) (foldMap f r)
  foldMap f (ExpSum es)  = mconcat (map (foldMap f) es)
  foldMap f (ExpMul l r) = mappend (foldMap f l) (foldMap f r)

type LitExp = Exp Lit

newtype Prog
  = Prog LitExp
  deriving (Eq, Ord, Show, Generic)

instance Out Prog
