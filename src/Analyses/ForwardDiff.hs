module Analyses.ForwardDiff where

import Ast
import Analyses.FreeVars

litDiff :: String -> Lit -> Lit
litDiff _  (LitCon c) = LitCon c
litDiff n  (LitVar m) | n == m     = LitCon "1"
litDiff _  (LitVar m) | otherwise  = LitVar m

expDiff :: String -> LitExp -> LitExp
expDiff n (ExpLit lit)  = ExpLit (litDiff n lit)
expDiff n (ExpAdd l r)  = ExpAdd (expDiff n l) (expDiff n r)
expDiff n (ExpSub l r)  = ExpSub (expDiff n l) (expDiff n r)
expDiff n (ExpSum es)   = ExpSum (map (expDiff n) es)
expDiff n (ExpMul l r)  =
  ExpAdd (ExpMul (expDiff n l) r) (ExpMul l (expDiff n r))

progDiff :: String -> Prog -> Prog
progDiff n (Prog e) = Prog $ expDiff n e

diff :: Prog -> [Prog]
diff p = fmap (flip progDiff $ p) (freeVarList p)
