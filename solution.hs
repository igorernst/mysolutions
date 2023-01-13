import Data.Ratio ( (%), denominator, numerator )

isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime n = not (any (\x -> n `mod` x == 0) $ takeWhile (\x -> x*x <= n) $ 2 : [3, 5..])

primes :: [Integer]
primes = filter isPrime $ 2 : [3, 5..]

sievePrimes :: [Integer]
sievePrimes = sieve $ 2 : [3, 5..]

sieve :: Integral a => [a] -> [a]
sieve (x:xs) = x : sieve (filter (\y -> y `mod` x /= 0) xs)

primeDivisors' n = pd n 1 []
    where 
        pd 1 d ds = reverse ds
        pd n d ds = case pds of 
            [] -> reverse ds 
            (d':_) -> if d' == d then 
                pd (n `div` d') d' ds 
                else
                    pd (n `div` d') d' (d':ds) 
            where 
                pds = filter (\x -> n `mod` x == 0) $ takeWhile (<=n) (2 : [3,5..])


totient :: Integer -> Integer
totient 1 = 1
totient n = numerator ratio `div` denominator ratio
 where ratio = foldl (\acc x -> acc * (1 - (1 % x))) 
                 (n % 1) $ primeDivisors' n

-- https://projecteuler.net/problem=3
-- largest prime factor 
-- p - least factor and p /= n then lpf n == lpf (n / p)
lpf :: Integer -> Integer 
lpf 1 = 1
lpf n = if p == n then n else lpf $ n `div` p 
    where p = head $ filter (\x -> n `mod` x == 0) (2 : [3,5..])

ans3 = lpf 600851475143 

-- https://projecteuler.net/problem=5
-- Smallest multiple
ans5 = foldl lcm 1 [1..20]

ans6 = primes !! (10001 - 1)


