Power digit sum
===============

$2^{15} = 32768$ and the sum of its digits is $3 + 2 + 7 + 6 + 8 = 26$.

What is the sum of the digits of the number $2^{1000}$?

---

> module Problem16 where
> import Problem4 (toDigits)

> main :: IO ()
> main = print . sumOfDigits $ 2^1000

> -- | compute the sum of the digits
> sumOfDigits :: Integral a => a -> a
> sumOfDigits = sum . toDigits 10
