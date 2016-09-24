module CreditCard where

lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits(dropLastDigit(n)) ++ [lastDigit(n)]

doubleEveryOtherLeft :: [Integer] -> [Integer]
doubleEveryOtherLeft [] = []
doubleEveryOtherLeft (x:[]) = [x]
doubleEveryOtherLeft (x:y:zs) = x : (y * 2) : doubleEveryOtherLeft(zs)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse(doubleEveryOtherLeft(reverse(x)))

crossSum :: Integer -> Integer
crossSum x
  | x < 10 = x
  | otherwise = lastDigit(x) + crossSum(dropLastDigit(x))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = crossSum(x) + sumDigits(xs)

validate :: Integer -> Bool
validate x = lastDigit(sumDigits(doubleEveryOther(toDigits(x)))) == 0
