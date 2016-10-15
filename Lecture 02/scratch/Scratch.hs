module Scratch where

neg :: Integer -> Integer
neg x = (-x)

sumTo :: Integer -> [Integer] -> Integer
sumTo n nums = sumToAcc 0 n nums

sumTo20 :: [Integer] -> Integer
sumTo20 nums = sumToAcc 0 20 nums

sumToAcc :: Integer -> Integer -> [Integer] -> Integer
sumToAcc acc _ [] = acc
sumToAcc acc n (x:xs) 
  | acc >= n = acc
  | otherwise = sumToAcc (acc + x) n xs

