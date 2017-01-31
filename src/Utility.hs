module Utility where

import Data.Set ( fromList, toList )

fix :: Eq a => (a -> a) -> a -> a
fix f x =
  let y = f x
  in if x == y then x else fix f y

fixn :: Eq a => Int -> (a -> a) -> a -> a
fixn 0 _ x = x
fixn n f x =
  let y = f x
  in if x == y then x else fixn (n-1) f y

unique :: (Ord a) => [a] -> [a]
unique = toList . fromList
