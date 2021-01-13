-- maximum 
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

-- replicate
replicate' :: (Eq a) => Int -> a -> [a]
replicate' n x 
    | n <= 0 = []
    | otherwise  = x : replicate (n - 1) x

-- take
take' :: (Num a, Ord a) => a -> [a] -> [a]
take' 0 xs = []
take' n (x:xs) = x : take' (n - 1) xs 

--reverse
reverse' :: (Ord a, Eq a) => [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- repeat
repeat' :: a -> [a]  
repeat' x = x:repeat' x 

-- zip 
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [x] [y] = [(x,y)]
zip' (x:xs) (y:ys) = (x,y): zip' xs ys