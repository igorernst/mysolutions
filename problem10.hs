
isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime n = not (any (\x -> n `mod` x == 0) $ takeWhile (\x -> x*x <= n) $ 2 : [3, 5..])

primes :: [Int]
primes = filter isPrime $ 2 : [3, 5..]

ans = sum $ takeWhile (<2000000) primes 
main = print ans
