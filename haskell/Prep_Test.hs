import Prep (isPal)

import Test.HUnit (Test( TestCase, TestList ), assertEqual, runTestTT, assertBool)

test1 :: Test 
test1 = TestCase $ assertEqual "1 is pal" True (isPal 1)

test2 :: Test 
test2 = TestCase $ assertEqual "123 is not pal" False (isPal 123)

test3 :: Test
test3 = TestCase $ assertEqual "12321 is pal" True (isPal 12321)

test4 :: Test 
test4 = TestCase $ assertEqual "123321 is pal" True (isPal 123321)


tl = TestList [test1, test2, test3, test4]
main = runTestTT tl