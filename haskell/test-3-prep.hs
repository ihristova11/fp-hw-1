--maximum 
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise  = maxTail
    where maxTail = maximum' xs

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "losho"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum' xs)

--replicate
replicate' :: (Eq a) => Int -> a -> [a]
replicate' n x 
    | n <= 0 = []
    | otherwise  = x : replicate (n - 1) x

