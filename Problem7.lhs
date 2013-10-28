10001st prime
=============

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?

---

> module Problem7 where
> import Problem3 (primes)

> main :: IO ()
> main = print $ primes !! 10000

Why `(!! 10000)`? Because we start counting from `0`.
