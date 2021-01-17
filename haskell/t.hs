import Data.List

data Tree a = Empty | Node {value :: a, left :: Tree a, right :: Tree a}

sampleTree = (Node 'a' 
                    (Node 'b' (Node 'x' Empty Empty) (Node 'g' Empty Empty)) 
                    (Node 'c' Empty Empty ))

wordss :: Tree Char -> [String]
wordss Empty = []
wordss (Node val Empty Empty) = [[val]]
wordss t = [value t : remaining | remaining <- wordss (left t) ++ wordss (right t)]

mapp :: Integral t => (t -> t) -> t -> t -> (t,t)
mapp f a b = (min, max)
    where min = minimum l
          max = maximum l
          l = [f x | x <- [a..b]]