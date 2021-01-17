-- Контролно 3 по ФП 2015

module Prep where
    
import Prelude

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
sampleList = [12, 8, 17, 12, 4, 8, 3, 17, 8, 4, 12, 8]

occurences :: (Eq a) => [a] -> a -> Int
occurences (x:xs) n
    | null xs = 0
    | otherwise = if n == x then 1 + occurences xs n else occurences xs n

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = x : unique (filter (x /=) xs)

frequencyDict :: Eq a => [a] -> [(a, Int)]
frequencyDict xs = [(x, occurences xs x) | x <- unique xs]

-- Да се напише функция variations k l p q на езика Haskell, която намира всички вариации без повторения на k
-- елемента на списъка от цели числа l, разликата от елементите на които принадлежи на интервала [p, q].
-- Пример: variations 2 [10, 20, 30] -15 15 = [ [10, 20], [20, 10], [20, 30], [30, 20]]. 
-- Всички вариации без повторения на елементите на списъка [10, 20, 30] при k = 2 са 
-- [ [10, 20], [10, 30], [20, 10], [20, 30], [30, 10], [30, 20]].
-- Забележкa: k е не по-голямо от дължината на списъка l.

-- Да се генерират всички подсписъци на даден такъв
-- [1,2,3] -> [[1,2,3],[1,2],[2,3],[1],[2],[3]]
-- prefixes = [1,2,3], [1,2], [1]
-- suffixes of [1,2,3] = [1,2,3], [2,3], [3]
-- suffixes of [1,2] = [1,2], [2]
-- suffixes of [1] = [1]
-- subsequences :: [a] -> [[a]]
-- subsequences [] = []
-- subsequences xs = [] : [suffix | prefixes <- inits xs, suffix <- tails prefixes]
--   where inits [] = []
--         inits xs = xs : inits (init xs)
--         tails [] = []
--         tails xs = xs : tails (tail xs)

inits :: [a] -> [[a]]
inits [] = []
inits xs = xs : inits (init xs)

tails :: [a] -> [[a]]
tails [] = []
tails xs = xs : tails (tail xs)

subsequences :: [a] -> [[a]]
subsequences [] = []
subsequences xs = [] : [suffix | prefixes <- inits xs, suffix <- tails prefixes]

-- Вариант 2
-- Симетрично на дадено положително цяло число се нарича число със същите цифри, 
-- но записани в обратен ред. 
-- Да се напише функция symNumber n на езика Haskell, 
-- която намира симетричното число на дадено естествено число.
-- Пример:
-- symNumber 1232 връща 2321 

symNumber :: Int -> Int 
symNumber x 
    | x < 10 = x
    | otherwise = symNumberUtil x 0
        where symNumberUtil :: Int -> Int -> Int
              symNumberUtil x res = if x <= 0 then res 
                                    else symNumberUtil (x `div` 10) (res * 10 + (x `mod` 10))

-- sumLast init n - безкраен поток, който започва с init, а всеки следващ елемент е сума на последните n от потока
-- sumLast 3 5 → [3, 3, 6, 12, 24, 48, 93, 183, ... ]

sumLast :: Int -> Int -> [Int]
sumLast initial n = initial : generate [initial]
    where generate result = sum result : generate (sum result : (if length result >= n then generate (init result) else result))

-- Даден е списък, съдържащ координатите на точки от равнината. 
-- Например [(1,2), (3,2), (7,4)] е списък от координатите на три точки от равнината. 
-- Координатите са целочислени. Да се състави функция, която:
-- а) проверява дали редицата от точки е строго монотонно растяща относно абсцисите на точките;
-- б) изтрива точките, които нарушават строгата монотонност относно абсцисите на точките;
-- в) намира и извежда лицето на фигурата, ограничена от начупената линия, определена от точките, 
-- останали след изтриването от б), и абсцисната ос.

sample = [(1,2), (3,2), (7,4)]
invalid_sample = [(10,2), (3,2), (7,4)]

-- a
increasing xs = length (filter (==True) (map (> head (map (fst) xs)) (map (fst) xs))) == length xs - 1
-- heads xs res 
--     | null xs = [] 
--     | otherwise = fst (head xs) : heads (tail xs) res

-- generate xs = map (fst)

-- t xs = length (filter (==True) (map (> head xs) xs)) == length xs - 1

-- b
removeBadPoints xs = let min = fst (head xs)
                     in head xs : filter (\x -> fst x > min) xs

-- v?

---------


lst = [(1.0,1.0), (2,2), (3, 3), (4,4)] -- find length

generatePairs xs = (init $ pairs xs []) ++ [[last xs] ++ [head xs]]
        where pairs lst res = if not $ null lst then take 2 lst : pairs (tail lst) res else res

dist xs = sum (allDist (generatePairs xs))
    where allDist :: [[(Double, Double )]] -> [Double]
          allDist = map (\el -> sqrt (((fst $ head el) - (fst $ head $ tail el))^2 + ((snd $ head el) - (snd $ head $ tail el))^2))

matrix1 :: [[Int]]
matrix1 = [[1, 0, -1], [2, 0, -2], [1, 0, -1]]

matrix2 :: [[Int]]
matrix2 = [[1, 2, 1], [0, 0, 0], [-1, -2, -1]]

at2D :: [[a]] -> Int -> Int -> a
at2D matrix x y = matrix !! x !! y

