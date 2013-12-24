Maximum path sum II
===================

By starting at the top of the triangle below and moving to adjacent numbers on
the row below, the maximum total from top to bottom is 23.

       3
      7 4
     2 4 6
    8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom in the attached file
(renamed to Problem67.txt).

---

module Problem67 where

> import Problem18 (maxPathSum)

> main :: IO ()
> main = print . maxPathSum . parse =<< readFile "Problem67.txt"
>   where
>     parse :: String -> [[Integer]]
>     parse = map ((map read) . words) . lines
