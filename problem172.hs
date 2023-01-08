
l = 18

digits = [0..3]

all_digit_combos :: [[Integer]]
all_digit_combos = [ [d0,d1,d2,d3,d4,d5,d6,d7,d8,d9] | 
    d0 <- digits, d1 <- digits, d2 <- digits, d3 <- digits, 
    d4 <- digits, d5 <- digits, d6 <- digits, d7 <- digits, 
    d8 <- digits, d9 <- digits, 
    d0 + d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 == l ]

leading_zero_combos :: [[Integer]]
leading_zero_combos = [ [d0,d1,d2,d3,d4,d5,d6,d7,d8,d9] | 
    d0 <- [0,1,2], d1 <- digits, d2 <- digits, d3 <- digits, 
    d4 <- digits, d5 <- digits, d6 <- digits, d7 <- digits, 
    d8 <- digits, d9 <- digits, 
    d0 + d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 == l - 1 ]

-- sum_{digits} C(18,digits!!0) * C(18 - digits !! 0,digits!!1) * ... = sum_digits factorial(18)/ (factorial(d_0) * ... * factorial(d9))

factorial :: (Num a, Enum a) => a -> a
factorial n = product [1..n]

summand :: [Integer] -> Integer
summand ds = factorial (fromIntegral $ sum ds) `div` product (map factorial ds)

all_combos_sum :: Integer
all_combos_sum = sum (map summand all_digit_combos)

leading_zero_sum :: Integer
leading_zero_sum = sum (map summand leading_zero_combos)

main = putStrLn $ show $ all_combos_sum - leading_zero_sum
