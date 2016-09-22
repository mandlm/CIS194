sumFun :: Integer -> Integer
sumFun 0 = 0
sumFun n
  | n < 0 = 0
  | otherwise = sumFun(n - 1) + n

sumList :: Integer -> [Integer]
sumList 0 = []
sumList n = sumFun(n) : sumList(n - 1)

