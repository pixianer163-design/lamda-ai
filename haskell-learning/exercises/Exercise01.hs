{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Exercise01 where

-- ============================================
-- Exercise 01: 基础函数与递归 (C程序员入门)
-- ============================================
-- 目标：掌握 Haskell 基本语法、模式匹配和递归
-- 难度：★☆☆☆☆

-- --------------------------------------------
-- 任务 1: 实现乘法（不使用 * 运算符）
-- --------------------------------------------
-- 提示：使用递归和加法
-- C版本参考：
-- int multiply(int a, int b) {
--     if (b == 0) return 0;
--     return a + multiply(a, b - 1);
-- }

multiply :: Int -> Int -> Int
multiply _ 0 = 0
multiply a b = a + multiply a (b-1)

-- --------------------------------------------
-- 任务 2: 计算列表元素之和
-- --------------------------------------------
-- 提示：对比 C 的数组求和
-- C版本参考：
-- int sum(int* arr, int n) {
--     if (n == 0) return 0;
--     return arr[0] + sum(arr + 1, n - 1);
-- }

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList(xs)

-- --------------------------------------------
-- 任务 3: 计算幂运算
-- --------------------------------------------
-- 实现 base^exponent
-- 注意：0^0 应该返回 1（数学惯例）

power :: Int -> Int -> Int
power _ 0 = 1
power base exp = base * power base (exp-1)

-- --------------------------------------------
-- 任务 4: 判断素数
-- --------------------------------------------
-- 提示：只需要检查到平方根
-- C版本思路：
-- bool isPrime(int n) {
--     if (n < 2) return false;
--     for (int i = 2; i * i <= n; i++)
--         if (n % i == 0) return false;
--     return true;
-- }

isPrime :: Int -> Bool
isPrime n
    | n < 2 = False
    | n == 2 = True
    | even n = False
    | otherwise = checkDivisor 3
    where
        checkDivisor d
            | d * d > n = True
            | mod n d == 0 = False
            | otherwise = checkDivisor (d + 2)		

-- --------------------------------------------
-- 任务 5: 计算最大公约数 (GCD)
-- --------------------------------------------
-- 使用欧几里得算法
-- C版本参考：
-- int gcd(int a, int b) {
--     if (b == 0) return a;
--     return gcd(b, a % b);
-- }

myGcd :: Int -> Int -> Int
myGcd a 0 = a
myGcd a b = myGcd b (mod a b) 

-- --------------------------------------------
-- 任务 6: 生成斐波那契数列（列表版本）
-- --------------------------------------------
-- 生成前 n 个斐波那契数
-- fibs 10 = [0,1,1,2,3,5,8,13,21,34]
-- 注意：使用尾递归优化效率

fibs :: Int -> [Int]
fibs n = go n 0 1 []
  where
    go :: Int -> Int -> Int -> [Int] -> [Int]
    go 0 _ _ acc = reverse acc
    go count a b acc = go (count - 1) b (a + b) (a : acc)

-- ============================================
-- 测试数据（你可以在 GHCi 中测试）
-- ============================================
-- 运行：stack ghci
-- 然后：:load exercises/Exercise01.hs
-- 测试：multiply 3 4 应该返回 12
-- 测试：sumList [1,2,3,4,5] 应该返回 15
-- 测试：power 2 10 应该返回 1024
-- 测试：isPrime 17 应该返回 True
-- 测试：isPrime 18 应该返回 False
-- 测试：myGcd 48 18 应该返回 6
-- 测试：fibs 10 应该返回 [0,1,1,2,3,5,8,13,21,34]
