import GHC.Base (assert)
--import GHC.Float

-- faster version of [ (a,b) | a <- [1..n], b <- [1..n], gcd a b == 1, a^2 + b^2 <= n ]
-- takeWhile (\x -> a^2 + x^2 <= n) [1..n]
coprimes :: Int -> [(Int, Int)]
coprimes n = do 
    a <- [1..n]
    b <- [1..n] 
    if gcd a b == 1 then return (a,b) else []


f a b = do 
    k <- [0..2*a - 1]
    let mMax = min (2*a*b - k*b) (k*b + 1)
    m <- takeWhile (\x -> x*a < mMax) [ head (filter (\x -> x `mod` a == 0) [-mMax + 1 .. mMax]) `div` a ..]
    let 
        t = pi * fromIntegral(k*b + m*a) / fromIntegral(a*b)
        s = pi * fromIntegral(k*b - m*a) / fromIntegral(a*b)
    if m /= 0 && k*b + m*a < 2*a*b && k*b - m*a < 2*a*b then
        return (k,m, (t,s), xy a b t, xy a b s, t < 2*pi && s < 2*pi )
        else []



xy a b t = (cos (a'*t), cos (b'*(t - pi/10)) )
    where 
        a' = fromIntegral a
        b' = fromIntegral b

testf a b = do 
    (k,m,(t,s), _, _, fl) <- f a b
    let 
        a' = fromIntegral a
        b' = fromIntegral b
    if not $ abs (cos (a'*t) - cos (a'*s) ) < 1e-11 && abs (cos (b'*(t - pi/10)) - cos (b'*(t - pi/10)) ) < 1e-11 then return (k,m,(t,s)) else []

testts a b = filter (\(k,m,(t,s), _, _, fl) -> (t >= 2*pi || t < 0) || (s >= 2*pi || s < 0)) $ f a b

