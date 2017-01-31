module Analyses.SymDiff where

import Ast
import Analyses.FreeVars
import Analyses.Reduce

litDiff :: String -> Lit -> Lit
litDiff _  (LitCon c) = LitCon c
litDiff n  (LitVar m) | n == m = LitCon "1"
litDiff _  (LitVar m) = LitVar m

expDiff :: String -> LitExp -> LitExp
expDiff n (ExpLit lit)  = ExpLit (litDiff n lit)
expDiff n (ExpAdd l r)  = ExpAdd (expDiff n l) (expDiff n r)
expDiff n (ExpSub l r)  = ExpSub (expDiff n l) (expDiff n r)
expDiff n (ExpSum es)   = ExpSum (map (expDiff n) es)
expDiff n (ExpMul l r)  =
  ExpAdd (ExpMul (expDiff n l) r) (ExpMul l (expDiff n r))

progDiff :: String -> Prog -> Prog
progDiff n (Prog e) = Prog $ expDiff n e

allFstDerivs :: Prog -> [Prog]
allFstDerivs p = fmap (reduce . flip progDiff p) (freeVarList p)
