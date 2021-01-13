module Task1 where

data Tree a = EmptyTree | Node {
                            value :: a,
                            left  :: Tree a,
                            right :: Tree a
                          } deriving (Show,Read)

data Strategy = Inorder | Postorder | Preorder deriving (Show, Read, Eq)

inorder :: Tree a -> [a]
inorder EmptyTree = []
inorder (Node val l r) = inorder l ++ [val] ++ inorder r

preorder :: Tree a -> [a]
preorder EmptyTree = []
preorder (Node val l r) = [val] ++ preorder l ++ preorder r

postorder :: Tree a -> [a]
postorder EmptyTree = []
postorder (Node val l r) = postorder l ++ postorder r ++ [val]

values :: Strategy -> (Tree a) -> [a]
values Inorder t = inorder t
values Preorder t = preorder t
values Postorder t = postorder t

testTree = Node 5
                (Node 12
                      EmptyTree
                      (Node 9 EmptyTree EmptyTree))
                (Node (-6)
                      (Node 20 EmptyTree EmptyTree)
                      (Node 2 EmptyTree EmptyTree))

