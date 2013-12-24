Largest prime factor
====================

The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143?

---

> module Problem3 where
> import Problem1 (divBy)

> main :: IO ()
> main = print . maximum . factor $ 600851475143

> factor :: Integral a => a -> [a]
> factor n = factor' n primes
>   where
>     factor' :: Integral a => a -> [a] -> [a]
>     factor' n (p:ps)
>       | p ^ 2 > n   = [n]
>       | n `divBy` p = p : factor' (n `div` p) (p:ps)
>       | otherwise   = factor' n ps

My initial solution used the Sieve of Eratosthenes to calculate the prime numbers,
as can be seen below.

    primes :: Integral a => [a]
    primes = 2 : sieve [3,5..]
      where
        sieve :: Integral a => [a] -> [a]
        sieve (x:xs) = x : sieve (filter (\y -> not (y `divBy` x)) xs)

Upon reading the solutions on [haskell.org](http://www.haskell.org/haskellwiki/Euler_problems/1_to_10#Problem_3) it seemed that defining `primes` mutually recursive with the factorization function was much more beneficial.

The reason for this is---amongst others---that in the below implementation,
a number can be discarded as soon as two of its prime factors are found---i.e.
it's divisable by two of the smaller primes---whereas in the implementation
above we have to check divisability with all smaller primes no matter what.

> primes :: Integral a => [a]
> primes = 2 : filter (single . factor) [3,5..]
>   where
>     single [_] = True
>     single _ = False
