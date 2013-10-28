Special Pythagorean triplet
===========================

A Pythagorean triplet is a set of three natural numbers, $a < b < c$, for which,

  $a^2 + b^2 = c^2$

For example, $3^2 + 4^2 = 9 + 16 = 25 = 52$.

There exists exactly one Pythagorean triplet for which $a + b + c = 1000$.

Find the product $abc$.

---

> module Problem9 where
> import Control.Monad (guard)

> main :: IO ()
> main = print . product . head . triplets $ 1000

> triplets :: Integral a => a -> [[a]]
> triplets l =
>   [ [a, b, c]
>   | m <- [2 .. limit]
>   , n <- [1 .. m - 1]
>   , let a = m ^ 2 - n ^ 2
>   , let b = 2 * m * n
>   , let c = m ^ 2 + n ^ 2
>   , a + b + c == l
>   ]
>   where
>     limit = floor . sqrt . fromIntegral $ l

> isPythagoreanTriplet :: Integral a => (a,a,a) -> Bool
> isPythagoreanTriplet (a,b,c) = a < b && b < c && a ^ 2 + b ^ 2 == c ^ 2
