Longest Collatz sequence
========================

The following iterative sequence is defined for the set of positive integers:

    n -> n/2 (n is even)
    n -> 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

    13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1

It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.
Although it has not been proved yet (Collatz Problem), it is thought that all starting
numbers finish at 1.

Which starting number, under one million, produces the longest chain?

---

> module Problem14 where
> import Data.List (elemIndex)
> import Data.Maybe (fromJust)

> main :: IO ()
> main = print . (+1) . fromJust . elemIndex (maximum collatzlens) $ collatzlens
>   where
>     collatzlens = map collatzlen [1..1000000]

> -- | compute the next value in the collatz sequence
> collatz :: Integer -> Integer
> collatz n
>   | even n = n `div` 2
>   | odd  n = 3 * n + 1

> -- | compute the length of the collatz sequence
> collatzlen :: Integer -> Integer
> collatzlen = count (==1) collatz

> -- | iterates a function until a certain predicate holds
> --   and records the number of iterations
> count :: (a -> Bool) -> (a -> a) -> a -> Integer
> count p f x = countAcc p f x 1
>   where
>     countAcc :: (a -> Bool) -> (a -> a) -> a -> Integer -> Integer
>     countAcc p f x n
>       | p x       = n
>       | otherwise = countAcc p f (f x) (n + 1)
