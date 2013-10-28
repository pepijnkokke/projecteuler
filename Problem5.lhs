Smallest multiple
=================

2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

---

> module Problem5 where
> import Problem1 (divBy)
> import Data.List (find)
> import Data.Maybe (fromJust)

> main :: IO ()
> main = print . fromJust . find (\n -> n `divByAll` [2..20]) $ [1..] 

> divByAll :: Integral a => a -> [a] -> Bool
> divByAll n ds = all (\d -> n `divBy` d) ds
