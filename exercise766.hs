module Exercise766 where

import Data.List (sort)

merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys) | x == y = x : merge xs ys
merge (x:xs) (y:ys) | x <  y = x : merge xs (y:ys)
merge (x:xs) (y:ys) | x >  y = y : merge (x:xs) ys
merge xs     []              = xs
merge []     ys              = ys

hamming :: [Integer]
hamming = 1 : merge (map (2*) hamming)
                    (merge (map (3*) hamming)
                           (map (5*) hamming))

hamming' :: Integer -> Integer -> Integer -> [Integer]
hamming' a b c = zs where
  zs = 1 : merge (map (a*) zs)
                 (merge (map (b*) zs)
                        (map (c*) zs))

hamming'' :: [Integer] -> [Integer]
hamming'' [] = []
hamming'' xs = zs where
  (y:ys) = sort xs
  zs = y : merge ys (merge (map (2*) zs)
                           (merge (map (3*) zs)
                                  (map (5*) zs)))
