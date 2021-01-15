import Prep (isPal, frequencyDict, increasing)

import Test.HUnit (Test( TestCase, TestList ), assertEqual, runTestTT, assertBool)

test1 :: Test 
test1 = TestCase $ assertEqual "1 is pal" True (isPal 1)

test2 :: Test 
test2 = TestCase $ assertEqual "123 is not pal" False (isPal 123)

test3 :: Test
test3 = TestCase $ assertEqual "12321 is pal" True (isPal 12321)

test4 :: Test 
test4 = TestCase $ assertEqual "123321 is pal" True (isPal 123321)

-- 
sampleList = [12, 8, 17, 12, 4, 8, 3, 17, 8, 4, 12, 8]
test5 :: Test 
test5 = TestCase $ assertEqual "frequencyDict []" [(12,3),(8,3),(17,2),(4,2),(3,1)] (frequencyDict sampleList)

valid_sample = [(1,2), (3,2), (7,4)]
invalid_sample = [(10,2), (3,2), (7,4)]

test6 :: Test 
test6 = TestCase $ assertEqual "" True (increasing valid_sample)

test7 :: Test 
test7 = TestCase $ assertEqual "" False (increasing invalid_sample)

tl_1 = TestList [test1, test2, test3, test4]
tl_2 = TestList [test5]

tl_3 = TestList [test6, test7]
main = do 
    runTestTT tl_1
    runTestTT tl_2
    runTestTT tl_3