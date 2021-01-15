-- Контролно 3 по ФП 2015

module Prep where
    
-- Задача 1. Да се напише функция isPalindrome n на езика Haskell, която проверява дали цифрите на цялото неотрицателно число n образуват палиндром.
-- Пример: isPalindrome 12321 - True | isPalindrome 4544 - False

isPal :: Int -> Bool 
isPal x 
    | x < 10 = True 
    | otherwise = numToList x == reverse (numToList x)
        where numToList :: Int -> [Int]
              numToList n = if n > 0 then (n `mod` 10) :  numToList (n `div` 10) else []

-- Задача 2. Да се напише функция frequency dictionary на езика Haskell, която за даден списък от цели числа генерира честотен речник. 
-- Пример: За списъка [12, 8, 17, 12, 4, 8, 3, 17, 8, 4, 12, 8], да се генерира честотният речник [[12, 3], [8, 4], [17, 2], [4, 2], [3, 1]].
