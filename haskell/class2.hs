import Prelude

data List = EmptyList | Cons Int List deriving Show 

exampleList :: List
exampleList = Cons 1 (Cons 2 (Cons 3 EmptyList))

sum' :: List -> Int
sum' EmptyList = 0
sum' (Cons x xs) = x + sum' xs

-- премахване на първото срещане на елемент
remove :: Int -> List -> List
remove _ EmptyList = EmptyList
remove a (Cons x xs) 
        | a == x = xs
        | otherwise = Cons x (remove a xs)

-- индекс на срещане на дадено число в списък
find :: Int -> List -> Int
find _ EmptyList = -1
find el (Cons x xs)
    | el == x = 0
    | otherwise = 1 + find el xs

data Tree a = EmptyTree | Node  
                            { value :: a, 
                            left :: Tree a, 
                            right :: Tree a} 
                            deriving (Show, Read)