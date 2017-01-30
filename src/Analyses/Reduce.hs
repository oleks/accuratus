module Analyses.Reduce ( reduce ) where

import Ast

reduceExp :: LitExp -> LitExp
reduceExp (ExpAdd (ExpLit (LitCon s)) (ExpLit (LitCon t))) =
  ExpLit (LitCon $ s ++ "+" ++ t)
reduceExp (ExpAdd l r) =
  ExpAdd (reduceExp l) (reduceExp r)
reduceExp e = e

fix :: Eq a => (a -> a) -> a -> a
fix f x =
  let y = f x
  in if x == y then x else fix f y

reduce :: Prog -> Prog
reduce (Prog e) = Prog $ fix reduceExp e
