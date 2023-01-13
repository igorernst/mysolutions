
f n = if m == 1 then k + 1 else f (m `div` 2) + f (m - 1) * k
    where 
        (k, m) = divideAll 2 n

degreeOf p 1 = 0
degreeOf p n = if n `mod` p == 0 then 1 + degreeOf p (n `div` p) else 0

-- divideAll p n = (k, n `div` p^k) where k = degreeOf p n
divideAll p 1 = (0, 1)
divideAll p n = if n `mod` p == 0 then (1 + d, c) else (0, n)
    where
        (d, c) = divideAll p (n `div` p)

main = print $ f (10^25)
