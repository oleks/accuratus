module Analyses.FreeVars where

import Ast

import Data.Set

expFreeVars :: LitExp -> Set String
expFreeVars = foldMap litVars
  where
    litVars :: Lit -> Set String
    litVars (LitVar s) = singleton s
    litVars _ = empty

freeVars :: Prog -> Set String
freeVars (Prog e) = expFreeVars e

freeVarList :: Prog -> [String]
freeVarList = toList . freeVars
