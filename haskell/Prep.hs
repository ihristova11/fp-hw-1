-- -- maximum 
-- maximum' :: (Ord a) => [a] -> a
-- maximum' [] = error "empty list"
-- maximum' [x] = x
-- maximum' (x:xs)
--     | x > maxTail = x
--     | otherwise  = maxTail
--     where maxTail = maximum' xs

-- maximum'' :: (Ord a) => [a] -> a
-- maximum'' [] = error "losho"
-- maximum'' [x] = x
-- maximum'' (x:xs) = max x (maximum' xs)

-- -- replicate
-- replicate' :: (Eq a) => Int -> a -> [a]
-- replicate' n x 
--     | n <= 0 = []
--     | otherwise  = x : replicate (n - 1) x

-- -- take
-- take' :: (Num a, Ord a) => a -> [a] -> [a]
-- take' 0 xs = []
-- take' n (x:xs) = x : take' (n - 1) xs 

-- --reverse
-- reverse' :: (Ord a, Eq a) => [a] -> [a]
-- reverse' [] = []
-- reverse' (x:xs) = reverse' xs ++ [x]

-- -- repeat
-- repeat' :: a -> [a]  
-- repeat' x = x:repeat' x 

-- -- zip 
-- zip' :: [a] -> [b] -> [(a,b)]
-- zip' _ [] = []
-- zip' [x] [y] = [(x,y)]
-- zip' (x:xs) (y:ys) = (x,y): zip' xs ys

-- -- elem
-- elem' :: (Eq a) => a -> [a] -> Bool 
-- elem' _ [] = False 
-- elem' y (x:xs) = x == y || elem' y xs

-- sum' :: (Num i) => [i] -> i
-- sum' = foldl (+) 0

-- elem'' :: (Eq a) => a -> [a] -> Bool 
-- elem'' el xs = foldl (\acc x -> acc || x == el) False xs



-- --  https://github.com/semerdzhiev/fp-2020-21/blob/master/group-g/ex11-20210105-tasks.md

-- minimum' :: (Num a, Ord a) => [a] -> a
-- minimum' (x:xs) = foldl (\acc x -> if acc > x then x else acc) x xs

-- rev :: [a] -> [a]
-- rev = foldl (flip (:)) []

-- len :: [a] -> Int 
-- len = foldl (\acc x -> acc + 1) 0

-- countDivisors :: Int -> Int 
-- countDivisors n = length [x | x <- [2..n-1], mod n x == 0]

-- prime n = countDivisors n == 0

-- primes = filter prime [2..]

module Prep where
    
isPal :: Int -> Bool 
isPal x 
    | x < 10 = True 
    | otherwise = numToList x == reverse (numToList x)
        where numToList :: Int -> [Int]
              numToList n = if n > 0 then (n `mod` 10) :  numToList (n `div` 10) else []
