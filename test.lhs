
Compute the factorial of a given integer number n.

> fac :: Integer -> Integer
> fac 0 = 1
> fac n = n * fac(n-1)

Compute the sum of of all integer numbers from 1 to n.

> sumTo :: Integer -> Integer
> sumTo 0 = 0
> sumTo n = n + sumTo(n-1)

