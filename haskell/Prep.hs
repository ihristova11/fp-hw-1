-- Контролно 3 по ФП 2015

module Prep where
    
isPal :: Int -> Bool 
isPal x 
    | x < 10 = True 
    | otherwise = numToList x == reverse (numToList x)
        where numToList :: Int -> [Int]
              numToList n = if n > 0 then (n `mod` 10) :  numToList (n `div` 10) else []
