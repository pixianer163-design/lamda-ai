{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE InstanceSigs #-}
module Exercise04 where

-- ============================================
-- Exercise 04: 高阶函数与 Typeclass
-- ============================================
-- 目标：理解 Functor、Applicative、Monoid 概念
-- 难度：★★★★☆
-- 提示：这是 Haskell 最 powerful 的特性

-- --------------------------------------------
-- 任务 1: 实现自定义二叉树
-- --------------------------------------------

data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving (Show, Eq)

-- 实现 Functor 实例（使 Tree 可以被 fmap）
-- fmap (*2) (Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf))
-- = Node 2 (Node 4 Leaf Leaf) (Node 6 Leaf Leaf)

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap _ Leaf = Leaf
    fmap f (Node x left right) = error "TODO: 实现 Tree Functor"

-- 实现 Foldable 实例（使 Tree 可以被 fold）
instance Foldable Tree where
    foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr f acc Leaf = acc
    foldr f acc (Node x left right) = error "TODO: 实现 Tree Foldable"

-- --------------------------------------------
-- 任务 2: 自定义 Maybe 类型（重新发明轮子来学习）
-- --------------------------------------------

data MyMaybe a = MyNothing | MyJust a
    deriving (Show, Eq)

-- 实现 Functor
instance Functor MyMaybe where
    fmap _ MyNothing = MyNothing
    fmap f (MyJust x) = error "TODO: 实现 MyMaybe Functor"

-- 实现 Applicative
-- MyJust (+1) <*> MyJust 5 = MyJust 6
-- MyJust (+1) <*> MyNothing = MyNothing
-- MyNothing <*> MyJust 5 = MyNothing
instance Applicative MyMaybe where
    pure :: a -> MyMaybe a
    pure = MyJust
    
    (<*>) :: MyMaybe (a -> b) -> MyMaybe a -> MyMaybe b
    MyNothing <*> _ = MyNothing
    MyJust f <*> mx = error "TODO: 实现 MyMaybe Applicative"

-- 实现 Monad
-- MyJust 3 >>= (\x -> MyJust (x * 2)) = MyJust 6
-- MyNothing >>= f = MyNothing
instance Monad MyMaybe where
    (>>=) :: MyMaybe a -> (a -> MyMaybe b) -> MyMaybe b
    MyNothing >>= _ = MyNothing
    MyJust x >>= f = error "TODO: 实现 MyMaybe Monad"

-- --------------------------------------------
-- 任务 3: 验证器组合子（Applicative 的实际应用）
-- --------------------------------------------
-- 用于表单验证等场景

data Validation e a = Failure e | Success a
    deriving (Show, Eq)

-- 实现 Functor
instance Functor (Validation e) where
    fmap f (Success a) = Success (f a)
    fmap _ (Failure e) = Failure e

-- 实现 Applicative（收集所有错误，而不是第一个就失败）
-- 注意：这需要 e 是 Monoid 才能组合多个错误
instance Monoid e => Applicative (Validation e) where
    pure :: a -> Validation e a
    pure = Success
    
    (<*>) :: Validation e (a -> b) -> Validation e a -> Validation e b
    Success f <*> Success x = Success (f x)
    Failure e1 <*> Failure e2 = Failure (e1 <> e2)  -- 组合错误
    Failure e <*> _ = Failure e
    _ <*> Failure e = Failure e

-- 使用 Validation 实现表单验证器

validateNonEmpty :: String -> Validation [String] String
validateNonEmpty "" = Failure ["Field cannot be empty"]
validateNonEmpty s = Success s

validateMinLength :: Int -> String -> Validation [String] String
validateMinLength n s 
    | length s < n = Failure ["Must be at least " ++ show n ++ " characters"]
    | otherwise = Success s

validateEmail :: String -> Validation [String] String
validateEmail s
    | '@' `elem` s = Success s
    | otherwise = Failure ["Invalid email format"]

-- 组合多个验证器
-- validateUser "john" "john@example.com" = Success ("john", "john@example.com")
-- validateUser "" "" = Failure ["Field cannot be empty", "Field cannot be empty", "Invalid email format"]
validateUser :: String -> String -> Validation [String] (String, String)
validateUser name email = 
    (,) <$> validateNonEmpty name 
        <*> (validateNonEmpty email *> validateEmail email)

-- --------------------------------------------
-- 任务 4: 函数组合练习
-- --------------------------------------------
-- 不使用 lambda，只用 . 和 $ 重写以下函数

-- 原函数：\x -> length (filter (> 0) x)
positiveCount :: [Int] -> Int
positiveCount = error "TODO: 使用函数组合重写"

-- 原函数：\x -> map (*2) (filter odd x)
doubleOdds :: [Int] -> [Int]
doubleOdds = error "TODO: 使用函数组合重写"

-- 原函数：\f g x -> f (g x)
compose :: (b -> c) -> (a -> b) -> a -> c
compose = error "TODO: 使用函数组合实现"

-- --------------------------------------------
-- 任务 5: 实现链式计算 DSL
-- --------------------------------------------
-- 模拟 fluent API 风格

newtype Calculator a = Calculator { runCalc :: a }
    deriving (Show)

instance Functor Calculator where
    fmap f (Calculator x) = Calculator (f x)

instance Applicative Calculator where
    pure = Calculator
    Calculator f <*> Calculator x = Calculator (f x)

instance Monad Calculator where
    Calculator x >>= f = f x

-- 实现计算函数
add :: Num a => a -> Calculator a -> Calculator a
add n (Calculator x) = error "TODO: 实现 add"

subtract' :: Num a => a -> Calculator a -> Calculator a
subtract' n (Calculator x) = error "TODO: 实现 subtract"

multiply' :: Num a => a -> Calculator a -> Calculator a
multiply' n (Calculator x) = error "TODO: 实现 multiply"

divide :: Fractional a => a -> Calculator a -> Calculator a
divide n (Calculator x) = Calculator (x / n)

-- 使用示例（完成后应该可以这样用）：
-- runCalc $ Calculator 10 `add` 5 `multiply'` 2 `subtract'` 10 = Calculator 20

-- --------------------------------------------
-- 任务 6: Monoid 实例实现
-- --------------------------------------------
-- 实现 First（取第一个非 Nothing 值）

newtype First a = First { getFirst :: Maybe a }
    deriving (Show, Eq)

instance Semigroup (First a) where
    First (Just x) <> _ = First (Just x)
    First Nothing <> y = error "TODO: 实现 First Semigroup"

instance Monoid (First a) where
    mempty = First Nothing

-- 使用示例：
-- mconcat [First Nothing, First (Just 1), First (Just 2)] = First (Just 1)

-- --------------------------------------------
-- 任务 7: 自定义列表排序函数
-- --------------------------------------------
-- 使用高阶函数实现更灵活的排序

-- 按指定字段排序
sortBy :: Ord b => (a -> b) -> [a] -> [a]
sortBy key [] = []
sortBy key (x:xs) = 
    let smaller = filter (\y -> key y <= key x) xs
        larger  = filter (\y -> key y > key x) xs
    in error "TODO: 实现 sortBy"

-- 降序排序
sortDesc :: Ord a => [a] -> [a]
sortDesc = error "TODO: 使用 sortBy 实现降序排序"

-- ============================================
-- 进阶挑战
-- ============================================

-- 挑战：实现 Traversable for Tree
-- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)

-- ============================================
-- 测试数据
-- ============================================
-- :load exercises/Exercise04.hs
-- fmap (*2) (Node 1 (Node 2 Leaf Leaf) Leaf) = Node 2 (Node 4 Leaf Leaf) Leaf
-- MyJust 3 >>= (\x -> MyJust (x * 2)) = MyJust 6
-- validateUser "john" "john@example.com" = Success ("john", "john@example.com")
-- validateUser "" "invalid" = Failure [...]
-- runCalc $ Calculator 10 `add` 5 = Calculator 15
