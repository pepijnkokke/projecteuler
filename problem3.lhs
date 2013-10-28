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
>     factor' 1 _ = []
>     factor' n (p:ps)
>       | n `divBy` p = p : factor (n `div` p) 
>       | otherwise   = factor' n ps

> primes :: Integral a => [a]
> primes = sieve [2..]
>   where
>     sieve :: Integral a => [a] -> [a]
>     sieve (x:xs) = x : sieve (filter (\y -> not (y `divBy` x)) xs)

> isPrime :: Integral a => a -> Bool
> isPrime n = all (not . divBy n) [2 .. n - 1]

> divBy :: Integral a => a -> a -> Bool
> divBy n d = rem n d == 0