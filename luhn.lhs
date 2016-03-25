This is an implementation of the Luhn Algorithm. It's used to validate credit card numbers.

> lastDigit :: Integer -> Integer
> lastDigit n = mod n 10

> dropLastDigit :: Integer -> Integer
> dropLastDigit n = div n 10

> toRevDigits :: Integer -> [Integer]
> toRevDigits n
>   | n < 1     = []
>   | otherwise = lastDigit(n) : toRevDigits(dropLastDigit(n))

