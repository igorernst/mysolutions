
isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime n = not (any (\x -> n `mod` x == 0) $ takeWhile (\x -> x*x <= n) $ 2 : [3, 5..])

primes :: [Integer]
primes = filter isPrime $ 2 : [3, 5..]

sievePrimes :: [Integer]
sievePrimes = sieve $ 2 : [3, 5..]

sieve :: Integral a => [a] -> [a]
sieve (x:xs) = x : sieve (filter (\y -> y `mod` x /= 0) xs)

-- https://projecteuler.net/problem=3
-- largest prime factor 
-- p - least factor and p /= n then lpf n == lpf (n / p)
lpf :: Integer -> Integer 
lpf 1 = 1
lpf n = if p == n then n else lpf $ n `div` p 
    where p = head $ filter (\x -> n `mod` x == 0) [2..n]

ans3 = lpf 600851475143 

-- https://projecteuler.net/problem=5
-- Smallest multiple
ans5 = foldl lcm 1 [1..20]

ans6 = primes !! (10001 - 1)

-- is there a prime p such that 10^p - 1 is divisible by a prime q < p ? 
testDiv :: Integer -> [Integer]
testDiv p = filter (\x -> ones `mod` x == 0) $ takeWhile (<p) primes 
    where ones = (10^p - 1) `div` 9
-- map testDiv $ take 2000 primes 
-- все пустые

-- 169
input = 10

toBinary :: (Integral t, Show t) => t -> [Char]
toBinary 0 = show 0
toBinary 1 = show 1
toBinary n = toBinary n1 ++ d
   where 
   r = mod n 2
   d = show r
   n1 = div n 2


