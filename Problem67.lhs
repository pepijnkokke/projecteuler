Maximum path sum II
===================

By starting at the top of the triangle below and moving to adjacent numbers on
the row below, the maximum total from top to bottom is 23.

  <p align="center"><img alt="Example triangle 1." src="Problem18_0.png" /></p>

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom in the attached file
(renamed to Problem67.input).

---

module Problem67 where

> import Problem18 (maxPathSum)

> main :: IO ()
> main = print . maxPathSum . parse =<< readFile "Problem67.input"
>   where
>     parse :: String -> [[Integer]]
>     parse = map ((map read) . words) . lines
