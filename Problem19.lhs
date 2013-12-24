Counting Sundays
================

You are given the following information, but you may prefer to do some research
for yourself.

  - 1 Jan 1900 was a Monday.

  - Thirty days has September,
    April, June and November.
    All the rest have thirty-one,
    Saving February alone,
    Which has twenty-eight, rain or shine.
    And on leap years, twenty-nine.

  - A leap year occurs on any year evenly divisible by 4,
    but not on a century unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth century
(1 Jan 1901 to 31 Dec 2000)?

---

> module Problem19 where
> import Problem1 (divBy)

> type Year  = Int
> type Month = Int
> type Day   = Int
> data Date  = Date Year Month Day
>      deriving (Eq,Show)

> isLeapYear :: Year -> Bool
> isLeapYear y = (y `divBy` 4) && not (y `divBy` 400)

> daysOfMonth :: Year -> Month -> Int
> daysOfMonth y m = days !! (m - 1)
>   where
>     days =
>       [31, if isLeapYear y then 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

> datesFrom :: Date -> [Date]
> datesFrom (Date y m d) =
>   [ Date y m d | y <- [y..], m <- [1..12], d <- [1..daysOfMonth y m] ]

> datesFromTo :: Date -> Date -> [Date]
> datesFromTo d1 d2 = takeWhile (/= d2) (datesFrom d1)

> data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
>      deriving (Eq,Enum,Bounded,Show)

> addWeekdays :: [Date] -> [(Weekday,Date)]
> addWeekdays = zip (cycle [minBound..])

> main :: IO ()
> main = print (length firstMons)
>   where
>     since1900 = addWeekdays (datesFromTo (Date 1900 1 1) (Date 2000 12 31))
>     since1901 = dropWhile ((/= Date 1901 1 1) . snd) since1900
>     firstMons = filter (\(wd,Date _ _ md) -> wd == Mon && md == 1) since1901
