module Scratch where

neg :: Integer -> Integer
neg x = (-x)

sumTo20 :: [Integer] -> Integer
sumTo20 nums = sumTo20Acc 0 nums

sumTo20Acc :: Integer -> [Integer] -> Integer
sumTo20Acc acc [] = acc
sumTo20Acc acc (x:xs) 
  | acc >= 20 = acc
  | otherwise = sumTo20Acc (acc + x) xs

