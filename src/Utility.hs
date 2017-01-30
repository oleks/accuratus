module Utility where

fix :: Eq a => (a -> a) -> a -> a
fix f x =
  let y = f x
  in if x == y then x else fix f y
