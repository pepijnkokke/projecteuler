Lattice paths
=============

Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.

  ![Routes in a 2×2 grid.](Problem15_0.gif)

How many such routes are there through a 20×20 grid?

---

> module Problem15 where

Since all routes only moving right and down have (in a 2×2 grid) length 4, so the
question boils down to "What is the length of a route in an m×n grid?" (m+n) and
"How many possible routes of length m×n are there only moving right or down in an?
m×n grid?" ($\frac{(m + n)!}{m! * n!}$)

> main :: IO ()
> main = print (routes 20 20)

> routes :: Integral a => a -> a -> a
> routes m n = fac (m + n) `div` (fac m * fac n)

> fac :: Integral a => a -> a
> fac 0 = 1
> fac n = n * fac (n - 1)
