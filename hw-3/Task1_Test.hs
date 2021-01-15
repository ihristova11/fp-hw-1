import Task1
  ( Strategy (Inorder, Postorder, Preorder),
    Tree (EmptyTree, Node, left, right, value),
    values,
  )

import Test.HUnit (Test (TestCase, TestList), assertEqual, runTestTT)

sampleTree :: Tree Integer
sampleTree =
  Node
    { value = 5,
      left =
        Node
          { value = 22,
            left =
              Node
                { value = 2,
                  left = EmptyTree,
                  right = EmptyTree
                },
            right =
              Node
                { value = 6,
                  left = EmptyTree,
                  right = EmptyTree
                }
          },
      right =
        Node
          { value = 1,
            left = EmptyTree,
            right =
              Node
                { value = 3,
                  left =
                    Node
                      { value = 111,
                        left = EmptyTree,
                        right = EmptyTree
                      },
                  right = EmptyTree
                }
          }
    }

oneNodeTree :: Tree Int 
oneNodeTree = Node 3 EmptyTree EmptyTree

emptyTree :: Tree a
emptyTree = EmptyTree 

test1 :: Test 
test1 = TestCase 
        (assertEqual 
            "Sample test inorder" 
            [2, 22, 6, 5, 1, 111, 3] 
            (values Inorder sampleTree)
        )

tl = TestList [test1]

main = runTestTT tl