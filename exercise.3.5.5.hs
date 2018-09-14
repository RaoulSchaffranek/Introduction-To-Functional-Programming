module Exercise355 where

-- The function remdups removes adjacent duplicates from a list. For example,
-- remdups [1, 2, 2, 3,3,3, 1, 1] = [1, 2, 3, 1] . Define remdups using
-- either foldl or foldr.
remdupsr :: Eq a => [a] -> [a]
remdupsr xs = foldr f [] xs where
  f :: Eq a => a -> [a] -> [a]
  f x []              = [x]
  f x (y:ys) | x == y = y:ys
  f x ys              = x:ys

remdupsl :: Eq a => [a] -> [a]
remdupsl xs = foldl f [] xs where
  f :: Eq a => [a] -> a -> [a]
  f [] x              = [x]
  f (y:ys) x | x == y = y:ys
  f ys x              = x:ys

