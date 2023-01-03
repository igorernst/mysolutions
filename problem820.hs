
powMod a p n = pm a p 1
    where 
        pm a 0 acc = acc `mod` n 
        pm 1 p acc = acc
        pm a p acc = if even p 
            then pm ((a * a) `mod` n) (p `div` 2) acc
            else if acc == 0 then 0 else pm a (p - 1) ((acc * a) `mod` n)

g' l x = (powMod 10 l (10*x) `div` x ) `mod` 10
naive'' l n = sum $ map (g' l) $ [1..n]

ans x = naive'' x x

main = putStrLn $ show $ ans (10^7)
