-- Да се напише функция transformSum, която преобразува дърво с елементи цели числа в ново дърво със същата структура,
-- в което всеки елемент е заменен със сумата на елементите в поддървото с този корен в началното дърво.

data Tree = EmptyTree | Node {val:: Int, left :: Tree, right :: Tree} deriving (Show, Read)

sumSubTree :: Tree -> Int 
sumSubTree EmptyTree = 0
sumSubTree (Node val left right) = val + sumSubTree left + sumSubTree right

transformSum :: Tree -> Tree 
transformSum EmptyTree = EmptyTree
transformSum (Node val left right) = Node (val + sumSubTree left + sumSubTree right) (transformSum left) (transformSum right)

-- Да се генерират всички подсписъци на даден такъв


-- Път от корен до възел в двоично дърво кодираме с поредица от цифри 0 и 1, която започва с цифрата 1, 
--а за всяка следваща цифра 0 означава завиване по левия клон, а 1 — по десния. 
--Да се реализира функция sameAsCode, която в двоично дърво от числа връща такова число x, 
--което съвпада по стойност с двоичното число, кодиращо пътя от корена до x, или 0, ако такова число няма. 
--Представянето на дървото е по ваш избор.

