Maximum path sum I
==================

By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.

  <p align="center"><img alt="

       3
      7 4
     2 4 6
    8 5 9 3

  " src="Problem18_0.png" /></p>

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom of the triangle below:

  <p align="center"><img alt="

                                75
                              95  64
                            17  47  82
                          18  35  87  10
                        20  04  82  47  65
                      19  01  23  75  03  34
                    88  02  77  73  07  63  67
                  99  65  04  28  06  16  70  92
                41  41  26  56  83  40  80  70  33
              41  48  72  33  47  32  37  16  94  29
            53  71  44  65  25  43  91  52  97  51  14
          70  11  33  28  77  73  17  78  39  68  17  57
        91  71  52  38  17  14  91  43  58  50  27  29  48
      63  66  04  68  89  53  67  30  73  16  69  87  40  31
    04  62  98  27  23  09  70  98  73  93  38  53  60  04  23

  " src="Problem18_1.png" /></p>

---

> module Problem18 where
> import Data.List (zipWith3,maximum)

We can reify the lists of numbers into a tree structure using the
following algorithm---the key feature of which is in the embedded
`go` function, that moves the list of subtrees one place and then
joins it with the orignal into a list od nodes.

> data Tree a = Leaf
>             | Node a (Tree a) (Tree a)
>               deriving (Eq,Show)

> toTree :: [[a]] -> Tree a
> toTree = head . foldr go (repeat Leaf)
>   where
>     go :: [a] -> [Tree a] -> [Tree a]
>     go xs ts = zipWith3 Node xs ts (tail ts)

Once we have the trees, we can simply collect all possible paths
and sum over them to find the maximal path sum.

> paths :: Tree a -> [[a]]
> paths Leaf = [[]]
> paths (Node x l r) = fmap (x:) (paths l ++ paths r)

    maxPathSum :: Integral a => [[a]] -> a
    maxPathSum = maximum . map sum . paths . toTree

Which computes the correct answer---but at what cost? As the problem
statement already hints at, this brute-force solution is a bit slow,
even on the simple triangles above.

To make it faster we can perform a simple optimalisation: we can remove
the intermediate data structure. The easiest way to do this is to just
replace our constructor by a function that already computes our desired
result.

> maxPathSum :: Integral a => [[a]] -> a
> maxPathSum = head . foldr1 go
>   where
>     go :: Integral a => [a] -> [a] -> [a]
>     go xs ts = zipWith3 node xs ts (tail ts)
>       where
>         node :: Integral a => a -> a -> a -> a
>         node x l r = x + max l r

Using this function we can easily solve problem 18 *and* problem 67.

> main :: IO ()
> main = print . maxPathSum . parse =<< readFile "Problem18.lhs"
>   where
>     parse :: String -> [[Integer]]
>     parse = map ((map read) . words) . take 15 . drop 20 . lines
