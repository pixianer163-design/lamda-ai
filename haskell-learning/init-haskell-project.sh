#!/bin/bash
# Haskell 学习项目初始化脚本
# 使用方法: ./init-haskell-project.sh <project-name>

set -e

PROJECT_NAME=${1:-"haskell-learning"}

echo "🚀 初始化 Haskell 学习项目: $PROJECT_NAME"

# 创建项目目录
mkdir -p $PROJECT_NAME
cd $PROJECT_NAME

# 创建项目结构
echo "📁 创建项目结构..."
mkdir -p src app test

# 创建 stack.yaml
cat > stack.yaml << 'EOF'
resolver: lts-21.0  # GHC 9.4

packages:
- .

extra-deps: []
EOF

# 创建 package.yaml
cat > package.yaml << EOF
name:                $PROJECT_NAME
version:             0.1.0.0
github:              "githubuser/$PROJECT_NAME"
license:             BSD3
author:              "Your Name"
maintainer:          "your.email@example.com"
copyright:           "2024 Your Name"

extra-source-files:
- README.md
- CHANGELOG.md

description:         Haskell learning project for C programmers

dependencies:
- base >= 4.14 && < 5
- text
- containers
- mtl

ghc-options:
- -Wall
- -Wcompat
- -Widentities
- -Wincomplete-record-updates
- -Wincomplete-uni-patterns
- -Wmissing-export-lists
- -Wmissing-home-modules
- -Wpartial-fields
- -Wredundant-constraints

library:
  source-dirs: src

executables:
  $PROJECT_NAME-exe:
    main:                Main.hs
    source-dirs:         app
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - $PROJECT_NAME

tests:
  $PROJECT_NAME-test:
    main:                Spec.hs
    source-dirs:         test
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - $PROJECT_NAME
    - hspec
    - QuickCheck
EOF

# 创建 README
cat > README.md << 'EOF'
# Haskell Learning Project

为C/C++程序员准备的Haskell学习项目。

## 项目结构

```
.
├── app/              # 可执行文件入口
│   └── Main.hs      # 主程序
├── src/             # 库代码
│   ├── Basics.hs    # 基础语法练习
│   ├── Types.hs     # 类型系统练习
│   └── Concurrency.hs # 并发编程练习
├── test/            # 测试
│   └── Spec.hs      # 测试套件
├── package.yaml     # 项目配置
└── stack.yaml       # Stack配置
```

## 快速开始

```bash
# 构建项目
stack build

# 运行程序
stack exec haskell-learning-exe

# 运行测试
stack test

# 进入交互环境
stack ghci
```

## 练习模块

### 1. Basics.hs - 基础语法
- 变量和函数
- 列表操作
- 递归

### 2. Types.hs - 类型系统
- 代数数据类型
- Typeclass
- 错误处理

### 3. Concurrency.hs - 并发编程
- 轻量级线程
- MVar
- STM

## 学习路径

1. **第1周**: 完成 Basics.hs
2. **第2周**: 完成 Types.hs
3. **第3周**: 完成 Concurrency.hs
4. **第4周**: 综合练习

## 资源

- [Haskell Tutorial for C Programmers](http://www.haskell.org/haskellwiki/Haskell_tutorial_for_C_programmers)
- [What I Wish I Knew When Learning Haskell](http://dev.stephendiehl.com/hask/)
- [Learn You a Haskell](https://learnyouahaskell.github.io/)
EOF

# 创建 CHANGELOG
cat > CHANGELOG.md << 'EOF'
# Changelog

## 0.1.0.0 - 2024-01-01

- 初始化项目
- 添加基础练习模块
EOF

# 创建 .gitignore
cat > .gitignore << 'EOF'
.stack-work/
*.cabal
*~
*.hi
*.o
.DS_Store
EOF

# 创建 src/Basics.hs
cat > src/Basics.hs << 'EOF'
module Basics
    ( -- * 基础函数
      add
    , factorial
    , fibonacci
      -- * 列表操作
    , myLength
    , myMap
    , myFilter
    , myFoldl
    , myFoldr
      -- * 排序
    , quickSort
    , mergeSort
    ) where

-- | 加法函数（类似C的 int add(int a, int b) { return a + b; }）
add :: Int -> Int -> Int
add x y = x + y

-- | 阶乘（递归版本）
-- C版本:
-- int factorial(int n) {
--     if (n <= 1) return 1;
--     return n * factorial(n - 1);
-- }
factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)

-- | 斐波那契数列
-- 注意：这个实现效率低，仅用于演示递归
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- | 列表长度（递归实现）
-- C版本:
-- int length(int* arr, int n) {
--     if (n == 0) return 0;
--     return 1 + length(arr + 1, n - 1);
-- }
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- | map函数（类似C++ std::transform）
-- C++版本:
-- template<typename T, typename F>
-- void map(const std::vector<T>& input, std::vector<T>& output, F f) {
--     std::transform(input.begin(), input.end(), std::back_inserter(output), f);
-- }
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

-- | filter函数（类似C++ std::copy_if）
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs)
    | p x       = x : myFilter p xs
    | otherwise = myFilter p xs

-- | foldl（类似C++ std::accumulate）
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

-- | foldr（从右折叠）
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc [] = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)

-- | 快速排序
-- C版本比较复杂，Haskell版本非常简洁
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = 
    let smaller = quickSort [y | y <- xs, y <= x]
        larger  = quickSort [y | y <- xs, y > x]
    in smaller ++ [x] ++ larger

-- | 归并排序
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
  where
    (left, right) = splitAt (length xs `div` 2) xs
    
    merge :: Ord a => [a] -> [a] -> [a]
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
        | x <= y    = x : merge xs (y:ys)
        | otherwise = y : merge (x:xs) ys
EOF

# 创建 src/Types.hs
cat > src/Types.hs << 'EOF'
{-# LANGUAGE RecordWildCards #-}
module Types
    ( -- * 基础类型
      Point(..)
    , Shape(..)
    , area
      -- * 错误处理
    , safeDiv
    , safeHead
      -- * Typeclass实例
    , Money(..)
    , Currency(..)
    ) where

import Data.Maybe (fromMaybe)

-- | 点坐标（类似C struct Point { int x; int y; }）
data Point = Point
    { x :: Int
    , y :: Int
    } deriving (Show, Eq)

-- | 距离原点的距离
pointDistance :: Point -> Double
pointDistance Point{..} = sqrt (fromIntegral (x*x + y*y))

-- | 形状（带参数的代数数据类型）
data Shape 
    = Circle Point Float      -- 圆心和半径
    | Rectangle Point Float Float  -- 左上角、宽、高
    | Triangle Point Point Point   -- 三个顶点
    deriving (Show)

-- | 计算面积
area :: Shape -> Float
area (Circle _ r) = pi * r * r
area (Rectangle _ w h) = w * h
area (Triangle p1 p2 p3) = 
    let a = distance p1 p2
        b = distance p2 p3
        c = distance p3 p1
        s = (a + b + c) / 2
    in sqrt (s * (s - a) * (s - b) * (s - c))
  where
    distance (Point x1 y1) (Point x2 y2) = 
        sqrt (fromIntegral ((x2-x1)^2 + (y2-y1)^2))

-- | 安全除法（返回Maybe，处理除零）
-- C版本需要检查返回值或使用指针参数
-- Maybe a = Nothing | Just a
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv a b = Just (a `div` b)

-- | 安全取列表头部
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- | 货币类型
data Currency = USD | EUR | GBP | CNY
    deriving (Show, Eq, Enum, Bounded)

-- | 金额（类型安全的货币）
-- 比C的 double amount; 更安全，因为货币类型是编译期检查的
data Money = Money
    { amount :: Double
    , currency :: Currency
    } deriving (Show, Eq)

-- | 货币转换（简化版本）
convert :: Money -> Currency -> Double -> Money
convert Money{..} to rate = Money (amount * rate) to

-- | 只能相加同种货币
addMoney :: Money -> Money -> Either String Money
addMoney (Money a1 c1) (Money a2 c2)
    | c1 == c2  = Right $ Money (a1 + a2) c1
    | otherwise = Left $ "Cannot add " ++ show c1 ++ " and " ++ show c2

-- | Monoid实例（可选）
instance Semigroup Money where
    m1 <> m2 = case addMoney m1 m2 of
        Right m -> m
        Left _  -> error "Cannot combine different currencies"

instance Monoid Money where
    mempty = Money 0 USD
EOF

# 创建 src/Concurrency.hs
cat > src/Concurrency.hs << 'EOF'
{-# LANGUAGE NumericUnderscores #-}
module Concurrency
    ( -- * 基础并发
      counterExample
    , producerConsumer
      -- * STM
    , bankTransfer
    , retryExample
    ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever, replicateM_, when)
import System.Random (randomRIO)

-- | 简单的计数器（使用MVar）
counterExample :: IO ()
counterExample = do
    counter <- newMVar 0
    
    -- 启动10个线程，每个增加100次
    replicateM_ 10 $ forkIO $ 
        replicateM_ 100 $ modifyMVar_ counter (return . (+1))
    
    threadDelay 1_000_000  -- 等待1秒
    final <- readMVar counter
    putStrLn $ "Final counter: " ++ show final

-- | 生产者-消费者模式
producerConsumer :: IO ()
producerConsumer = do
    chan <- newChan
    done <- newMVar False
    
    -- 生产者
    forkIO $ do
        replicateM_ 10 $ do
            n <- randomRIO (1, 100)
            writeChan chan n
            threadDelay 100_000  -- 0.1秒
        putMVar done True
    
    -- 消费者
    forkIO $ forever $ do
        isDone <- tryReadMVar done
        when (isDone == Just True) $ do
            empty <- isEmptyChan chan
            when empty $ return ()
        
        val <- readChan chan
        putStrLn $ "Consumed: " ++ show val
        threadDelay 150_000  -- 0.15秒（比生产者慢）
    
    threadDelay 3_000_000  -- 等待3秒

-- | 银行账户转账（使用STM）
type Account = TVar Int

-- 创建账户
newAccount :: Int -> STM Account
newAccount = newTVar

-- 查询余额
getBalance :: Account -> STM Int
getBalance = readTVar

-- 存款
deposit :: Account -> Int -> STM ()
deposit acc amount = modifyTVar acc (+ amount)

-- 取款（可能失败）
withdraw :: Account -> Int -> STM Bool
withdraw acc amount = do
    bal <- readTVar acc
    if bal >= amount
        then do
            writeTVar acc (bal - amount)
            return True
        else return False

-- 转账（原子操作）
transfer :: Account -> Account -> Int -> STM Bool
transfer from to amount = do
    success <- withdraw from amount
    if success
        then do
            deposit to amount
            return True
        else return False

-- | 银行转账示例
bankTransfer :: IO ()
bankTransfer = do
    -- 初始化账户
    acc1 <- atomically $ newAccount 1000
    acc2 <- atomically $ newAccount 500
    
    putStrLn "Initial balances:"
    bal1 <- atomically $ getBalance acc1
    bal2 <- atomically $ getBalance acc2
    putStrLn $ "Account 1: " ++ show bal1
    putStrLn $ "Account 2: " ++ show bal2
    
    -- 执行转账
    success <- atomically $ transfer acc1 acc2 300
    if success
        then putStrLn "Transfer successful!"
        else putStrLn "Transfer failed!"
    
    putStrLn "\nFinal balances:"
    bal1' <- atomically $ getBalance acc1
    bal2' <- atomically $ getBalance acc2
    putStrLn $ "Account 1: " ++ show bal1'
    putStrLn $ "Account 2: " ++ show bal2'

-- | STM retry示例（条件等待）
retryExample :: IO ()
retryExample = do
    account <- atomically $ newTVar 100
    
    -- 线程1：等待余额>=200然后消费
    forkIO $ do
        putStrLn "Thread 1: Waiting for balance >= 200..."
        atomically $ do
            bal <- readTVar account
            when (bal < 200) retry  -- 重试直到条件满足
            writeTVar account (bal - 200)
        putStrLn "Thread 1: Consumed 200!"
    
    -- 线程2：3秒后存入150
    forkIO $ do
        threadDelay 3_000_000
        putStrLn "Thread 2: Depositing 150..."
        atomically $ modifyTVar account (+150)
    
    -- 线程3：5秒后再存入100
    forkIO $ do
        threadDelay 5_000_000
        putStrLn "Thread 3: Depositing 100..."
        atomically $ modifyTVar account (+100)
    
    -- 观察账户变化
    forever $ do
        threadDelay 1_000_000
        bal <- atomically $ readTVar account
        putStrLn $ "Current balance: " ++ show bal
EOF

# 创建 app/Main.hs
cat > app/Main.hs << 'EOF'
module Main where

import Basics
import Types
import Concurrency
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    putStrLn "========================================"
    putStrLn "Haskell Learning Project for C Programmers"
    putStrLn "========================================\n"
    
    -- 基础练习
    putStrLn "=== Basics ==="
    putStrLn $ "add 3 5 = " ++ show (add 3 5)
    putStrLn $ "factorial 5 = " ++ show (factorial 5)
    putStrLn $ "fibonacci 10 = " ++ show (fibonacci 10)
    putStrLn $ "myLength [1,2,3] = " ++ show (myLength [1,2,3])
    putStrLn $ "myMap (*2) [1,2,3] = " ++ show (myMap (*2) [1,2,3])
    putStrLn $ "quickSort [3,1,4,1,5,9,2,6] = " ++ show (quickSort [3,1,4,1,5,9,2,6])
    
    -- 类型练习
    putStrLn "\n=== Types ==="
    let p = Point 3 4
    putStrLn $ "Point 3 4 = " ++ show p
    
    let circle = Circle p 5.0
    putStrLn $ "Circle area = " ++ show (area circle)
    
    let rect = Rectangle p 10.0 20.0
    putStrLn $ "Rectangle area = " ++ show (area rect)
    
    putStrLn $ "safeDiv 10 2 = " ++ show (safeDiv 10 2)
    putStrLn $ "safeDiv 10 0 = " ++ show (safeDiv 10 0)
    
    let m1 = Money 100 USD
        m2 = Money 50 USD
        m3 = Money 30 EUR
    putStrLn $ "Money 1: " ++ show m1
    putStrLn $ "Money 2: " ++ show m2
    putStrLn $ "addMoney m1 m2 = " ++ show (addMoney m1 m2)
    putStrLn $ "addMoney m1 m3 = " ++ show (addMoney m1 m3)
    
    -- 并发练习
    putStrLn "\n=== Concurrency ==="
    putStrLn "Running counter example..."
    counterExample
    
    putStrLn "\nRunning bank transfer..."
    bankTransfer
    
    putStrLn "\n========================================"
    putStrLn "All examples completed!"
    putStrLn "Run 'stack test' to see the test suite."
    putStrLn "========================================"
EOF

# 创建 test/Spec.hs
cat > test/Spec.hs << 'EOF'
module Main where

import Test.Hspec
import Test.QuickCheck
import Basics
import Types

main :: IO ()
main = hspec $ do
    describe "Basics" $ do
        describe "add" $ do
            it "adds two numbers" $ do
                add 3 5 `shouldBe` 8
            
            it "is commutative" $ property $
                \x y -> add x y == add (y :: Int) (x :: Int)
        
        describe "factorial" $ do
            it "computes factorial correctly" $ do
                factorial 0 `shouldBe` 1
                factorial 5 `shouldBe` 120
        
        describe "myMap" $ do
            it "maps over a list" $ do
                myMap (*2) [1,2,3] `shouldBe` [2,4,6]
            
            it "preserves length" $ property $
                \xs -> length (myMap id (xs :: [Int])) == length xs
        
        describe "quickSort" $ do
            it "sorts a list" $ do
                quickSort [3,1,4,1,5,9,2,6] `shouldBe` [1,1,2,3,4,5,6,9]
            
            it "produces sorted output" $ property $
                \xs -> isSorted (quickSort (xs :: [Int]))
    
    describe "Types" $ do
        describe "safeDiv" $ do
            it "returns Just for valid division" $ do
                safeDiv 10 2 `shouldBe` Just 5
            
            it "returns Nothing for division by zero" $ do
                safeDiv 10 0 `shouldBe` (Nothing :: Maybe Int)
        
        describe "safeHead" $ do
            it "returns Just for non-empty list" $ do
                safeHead [1,2,3] `shouldBe` Just 1
            
            it "returns Nothing for empty list" $ do
                safeHead ([] :: [Int]) `shouldBe` Nothing
        
        describe "addMoney" $ do
            it "adds same currency" $ do
                let m1 = Money 100 USD
                    m2 = Money 50 USD
                addMoney m1 m2 `shouldBe` Right (Money 150 USD)
            
            it "fails for different currencies" $ do
                let m1 = Money 100 USD
                    m2 = Money 50 EUR
                case addMoney m1 m2 of
                    Left _ -> return ()
                    Right _ -> expectationFailure "Should have failed"

-- 辅助函数
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)
EOF

echo "✅ 项目结构创建完成！"
echo ""
echo "📚 接下来："
echo "1. cd $PROJECT_NAME"
echo "2. stack build      # 构建项目"
echo "3. stack exec ${PROJECT_NAME}-exe  # 运行程序"
echo "4. stack test       # 运行测试"
echo "5. stack ghci       # 进入交互环境"
echo ""
echo "💡 提示：如果遇到编译错误，尝试运行 'stack setup' 先安装GHC"
