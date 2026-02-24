{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main where

import Exercise01

-- 测试函数
main :: IO ()
main = do
    putStrLn "Testing Exercise 01...\n"
    
    -- 测试 multiply
    putStrLn "1. Testing multiply:"
    test "multiply 3 4" (multiply 3 4) 12
    test "multiply 5 6" (multiply 5 6) 30
    test "multiply 0 10" (multiply 0 10) 0
    test "multiply 7 1" (multiply 7 1) 7
    
    -- 测试 sumList
    putStrLn "\n2. Testing sumList:"
    test "sumList [1,2,3,4,5]" (sumList [1,2,3,4,5]) 15
    test "sumList []" (sumList []) 0
    test "sumList [10]" (sumList [10]) 10
    
    -- 测试 power
    putStrLn "\n3. Testing power:"
    test "power 2 10" (power 2 10) 1024
    test "power 5 0" (power 5 0) 1
    test "power 3 3" (power 3 3) 27
    test "power 0 0" (power 0 0) 1
    
    -- 测试 isPrime
    putStrLn "\n4. Testing isPrime:"
    test "isPrime 17" (isPrime 17) True
    test "isPrime 18" (isPrime 18) False
    test "isPrime 2" (isPrime 2) True
    test "isPrime 1" (isPrime 1) False
    test "isPrime 97" (isPrime 97) True
    
    -- 测试 myGcd
    putStrLn "\n5. Testing myGcd:"
    test "myGcd 48 18" (myGcd 48 18) 6
    test "myGcd 100 25" (myGcd 100 25) 25
    test "myGcd 17 13" (myGcd 17 13) 1
    
    -- 测试 fibs
    putStrLn "\n6. Testing fibs:"
    test "fibs 10" (fibs 10) [0,1,1,2,3,5,8,13,21,34]
    test "fibs 5" (fibs 5) [0,1,1,2,3]
    test "fibs 1" (fibs 1) [0]
    test "fibs 0" (fibs 0) []
    
    putStrLn "\n✓ All tests completed!"
  where
    test name actual expected = 
        if actual == expected
        then putStrLn $ "  ✓ " ++ name ++ " = " ++ show actual
        else putStrLn $ "  ✗ " ++ name ++ " = " ++ show actual ++ " (expected: " ++ show expected ++ ")"
