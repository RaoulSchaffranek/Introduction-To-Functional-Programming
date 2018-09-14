module Excercise532 where

takewhile :: (a -> Bool) -> [a] -> [a]
takewhile p xs = takewhile' p xs [] where
  takewhile' _ []     ys       = ys
  takewhile' p (x:xs) ys | p x = takewhile' p xs (ys ++ [x])
  takewhile' _ _      ys       = ys

dropwhile :: (a -> Bool) -> [a] -> [a]
dropwhile _ []           = []
dropwhile p (x:xs) | p x = dropwhile p xs
dropwhile _ xs           = xs
