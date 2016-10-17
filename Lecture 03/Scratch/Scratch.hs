module Scratch where

mapInteger :: (Integer -> Integer) -> [Integer] -> [Integer]
mapInteger _ [] = []
mapInteger f (x:xs) = f x : mapInteger f xs

plus1 :: Integer -> Integer
plus1 x = x + 1

filterInteger :: (Integer -> Bool) -> [Integer] -> [Integer]
filterInteger _ [] = []
filterInteger f (x:xs)
  | f x == True = x : filterInteger f xs
  | otherwise = filterInteger f xs
  
greaterThanTwo :: Integer -> Bool
greaterThanTwo x = x > 2

data Tree = Leaf Char
          | Node Tree Int Tree
  deriving Show
  
tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))
