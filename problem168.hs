{-
#Consider the number 142857. We can right-rotate this number by moving the last digit (7) to the front of it, giving us 714285.
#It can be verified that 714285=5×142857.
#This demonstrates an unusual property of 142857: it is a divisor of its right-rotation.

#Find the last 5 digits of the sum of all integers n, 10 < n < 10^100, that have this property.
-}

import Data.Ratio ( (%), denominator, numerator )

cds :: [(Integer,Integer)]
cds = [(c, d) | c <- [1..9], d <- [1..9], c >= d]

r c d = c % (10*d - 1)

f n r = (10^n - 1) * r

mrs = filter (\(r,c,d,n) -> (== 1) . denominator $ r) [(f n (r c d), c, d, n) | (c,d) <- cds, n <- [2..100]]

ans = sum $ map (\(m,c,d,n) -> numerator m) mrs

main = print ans

