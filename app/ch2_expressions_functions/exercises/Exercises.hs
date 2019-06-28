module Exercises2 where

-- ch2.5, ex2
squareByPi x = 3.14 * x^2
-- ch2.5, ex3
squareByPi2 x = pi * x^2

-- ch2.7, ex1
area x = 3.14 * (x * x)
-- ch2.7, ex2
double b = b * 2

-- ch2.10, ex
mult1       = x * y
    where x = 5
          y = 6

term1 = x * 3 + y
    where
        x = 3
        y = 1000

term2 = x * 5
    where
        y = 10
        x = 10 * 5 + y

term3 = z / x + y
    where
        x = 7
        y = (-x)
        z = y * 10

waxOn = x * 5
    where
        z = 7
        x = y ^ 2
        y = z + 8

triple = (*3)

waxOff x = triple x