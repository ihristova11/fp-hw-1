import Prelude

a :: Int 
a = 5

mySum :: Int -> Int -> Int 
mySum a b = a + b

factorial :: Int -> Int 
factorial n = if n == 0 then 1 else factorial (n - 1) * n

-- pattern matching (preffered because of readability and etc.)
fact :: Int -> Int 
fact 0 = 1
fact 1 = 1
fact n = fact (n - 1) * n

--fibonacci
fib :: Int -> Int 
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

sumElements :: [Int] -> Int 
sumElements xs = if null xs then 0 else head xs + sumElements (tail xs)

sumElements' :: [Int] -> Int 
sumElements' [] = 0
sumElements' xs = head xs + sumElements' (tail xs)

sumElements'' :: [Int] -> Int 
sumElements'' [] = 0
sumElements'' (x:xs) = x + sumElements'' xs

len :: [Int] -> Int 
len [] = 0
len xs = 1 + len (tail xs)

countDigits :: Int -> Int  -- | should be just under the func; it matters
countDigits n 
    |n < 0 = countDigits (-n)
    |n < 10 = 1
    |otherwise = 1 + countDigits (div n 10)