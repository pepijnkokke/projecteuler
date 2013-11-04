Number letter counts
====================

If the numbers 1 to 5 are written out in words: one, two, three, four, five,
then there are $3 + 3 + 5 + 4 + 4 = 19$ letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words,
how many letters would be used?

**NOTE**: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two)
contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of
"and" when writing out numbers is in compliance with British usage.

---

> module Problem17 where
> import Prelude hiding (and)
> import Data.Char (isLetter)

> main :: IO ()
> main = print . sum . map (countLetters . toEnglish) $ [1..1000]

> countLetters :: String -> Int
> countLetters = length . filter isLetter

> toEnglish :: Int -> String
> toEnglish n
>   | n <  0 = "minus" >#< naturals (abs n)
>   | n == 0 = "zero"
>   | n  > 0 = naturals n
>   where
>     -- | print numbers in english with the exception of zero
>     --   due to this being an exceptional case in the recursion
>     naturals :: Int -> String
>     naturals n
>       | n == 0         = ""
>       | n <= 19        = small     n
>       | n <= 99        = tens      n  >-<  (naturals $ n `rem` 10)
>       | n <= 999       = hundreds  n `and` (naturals $ n `rem` 100)
>       | n <= 999999    = thousands n  >#<  (naturals $ n `rem` 1000)
>       | n <= 999999999 = millions  n  >#<  (naturals $ n `rem` 1000000)
>
>     small :: Int -> String
>     small n =
>       ["one","two","three","four","five","six","seven","eight","nine","ten"
>       ,"eleven","twelve","thirteen","fourteen","fifteen"
>       ,"sixteen","seventeen","eighteen","nineteen"]
>       !! (n - 1)
>
>     tens :: Int -> String
>     tens n = ["ten","twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]
>              !! (n `div` 10 - 1)
>
>     hundreds :: Int -> String
>     hundreds n = naturals (n `div` 100) >#< "hundred"
>
>     thousands :: Int -> String
>     thousands n = naturals (n `div` 1000) >#< "thousand"
>
>     millions :: Int -> String
>     millions n = naturals (n `div` 1000000) >#< "million"


> -- | compose strings with a space, hyphen or "and"
> s1  >#<  s2 = compose s1 " " s2
> s1  >-<  s2 = compose s1 "-" s2
> s1 `and` s2 = compose s1 " and " s2

> -- | compose strings with a separator and with regard to emptyness
> compose :: String -> String -> String -> String
> compose s1 _ "" = s1
> compose "" _ s2 = s2
> compose s1 s s2 = s1 ++ s ++ s2
