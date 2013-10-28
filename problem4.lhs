Largest palindrome product
==========================

A palindromic number reads the same both ways. The largest palindrome made from
the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.

---

> main :: IO ()
> main = print . maximum . palindromes $ threeDigitSums
>   where
>     palindromes :: Integral a => [a] -> [a]
>     palindromes = filter (isPalindrome . toDigits 10)
>     threeDigitSums :: Integral a => [a]
>     threeDigitSums = [ x * y | x <- [100..999], y <- [100..999] ]

> isPalindrome :: (Eq a, Integral a) => [a] -> Bool
> isPalindrome xs = xs == reverse xs

> fromDigits :: Integral a => a -> [a] -> a
> fromDigits b ds = foldl (\d r -> d * b + r) 0 ds

> toDigits :: Integral a => a -> a -> [a]
> toDigits _ 0 = [0]
> toDigits b n = reverse (toDigits' n)
>   where
>     toDigits' 0 = []
>     toDigits' n = rem n b : toDigits' (div n b)