module Ast where

import Data.Number.MPFR.Mutable

data Exp s
  = Con (MMPFR s)
  | Add (Exp s) (Exp s)
  | Sub (Exp s) (Exp s)
  | Mul (Exp s) (Exp s)
  | Div (Exp s) (Exp s)
  deriving (Eq)
