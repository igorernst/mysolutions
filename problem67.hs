
main = do  
        contents <- readFile "p067_triangle.txt"
        let array = map (map readInt . words) . lines $ contents
        print $ ans array

readInt :: String -> Int
readInt = read

-- maxOfPrev [a_1, ..., a_n] = [a_1, max(a_1,a_2), ..., max(a_{n-1}, a_n), a_n]
maxOfPrev as = zipWith0 max 0 (0:as) as

zipWith0 :: (a -> a -> b) -> a -> [a] -> [a] -> [b]
zipWith0 f d = go
  where
    go [] [] = []
    go [] (b:bs) = f d b : go [] bs
    go (a:as) [] = f a d : go as []
    go (x:xs) (y:ys) = f x y : go xs ys

ans array = foldr max 0 $ foldl (zipWith (+) . maxOfPrev ) [] array
