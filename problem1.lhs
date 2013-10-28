Multiples of 3 and 5
====================

If we list all the natural numbers below 10 that are multiples of 3 or 5,
we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.

---

> module Problem1 where

> main :: IO ()
> main = print . sum . filter divBy3or5 $ [1 .. 999]
>   where
>     divBy3or5 :: Integral a => a -> Bool
>     divBy3or5 n = n `divBy` 3 || n `divBy` 5

> divBy :: Integral a => a -> a -> Bool
> divBy n d = rem n d == 0
