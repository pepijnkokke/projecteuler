Largest palindrome product
==========================

A palindromic number reads the same both ways. The largest palindrome made from
the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.

---

> isPalindrome :: (Eq a, Integral a) => [a] -> Bool
> isPalindrome xs = xs == reverse xs

> digits :: Integral a => a -> a -> [a]
> digits _ 0 = [0]
> digits b n = reverse (digits' n)
>   where
>     digits' 0 = []
>     digits' n = rem n b : digits' (div n b)