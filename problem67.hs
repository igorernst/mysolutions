
main = do  
        contents <- readFile "p067_triangle.txt"
        let array = map (map readInt . words) . lines $ contents
        print $ ans array

readInt :: String -> Int
readInt = read

-- maxOfPrev [a_1, ..., a_n] = [a_1, max(a_1,a_2), ..., max(a_{n-1}, a_n), a_n]
maxOfPrev as = zipWith max (0:as) (as ++ [0])

ans array = foldr max 0 $ foldl (zipWith (+) . maxOfPrev ) [] array
