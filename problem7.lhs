10001st prime
=============

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?

---

> main :: IO ()
> main = print $ primes !! 10001

> primes :: Integral a => [a]
> primes = sieve [2..]
>   where
>     sieve :: Integral a => [a] -> [a]
>     sieve (x:xs) = x : sieve (filter (\y -> not (y `divBy` x)) xs)

> divBy :: Integral a => a -> a -> Bool
> divBy n d = rem n d == 0