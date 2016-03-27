This is an implementation of the Luhn Algorithm. It's used to validate
credit card numbers.

Return the last digit of an integer number

> lastDigit :: Integer -> Integer
> lastDigit n = mod n 10

Remove the last digit of an integer number and return the rest

> dropLastDigit :: Integer -> Integer
> dropLastDigit n = div n 10

Put all digits of an integer number in reverse order into a list

> toRevDigits :: Integer -> [Integer]
> toRevDigits n
>   | n < 1     = []
>   | otherwise = lastDigit(n) : toRevDigits(dropLastDigit(n))

Put all digits of an integer number into a list

> toDigits :: Integer -> [Integer]
> toDigits n = reverse(toRevDigits(n))

Double every second value in the input list.

> doubleEveryOther :: [Integer] -> [Integer]
> doubleEveryOther([]) = []
> doubleEveryOther(x:[]) = [x]
> doubleEveryOther(a:(b:xs)) = a : 2*b : doubleEveryOther(xs)

Sum up all digits from all integers.

> sumDigits :: [Integer] -> Integer
> sumDigits([]) = 0
> sumDigits(0:xs) = sumDigits(xs)
> sumDigits(x:xs) = lastDigit(x) + sumDigits(dropLastDigit(x) : xs)

Evaluate if a credit card number is 'valid'.

> luhn :: Integer -> Bool
> luhn x = lastDigit(sumDigits(doubleEveryOther(toRevDigits(x)))) == 0
