# C/C++ → Haskell 快速参考速查表

## 📝 基础语法

### 变量和类型

```c
// C/C++
int x = 5;
float y = 3.14;
char* s = "hello";
const int MAX = 100;
```

```haskell
-- Haskell
x :: Int
x = 5

y :: Float
y = 3.14

s :: String
s = "hello"

maxVal :: Int
maxVal = 100  -- 默认就是"const"，不可变
```

### 函数定义

```c
// C
int add(int x, int y) {
    return x + y;
}

// C++
auto add(int x, int y) -> int {
    return x + y;
}
```

```haskell
-- Haskell
add :: Int -> Int -> Int
add x y = x + y

-- 等价写法（柯里化）
add :: Int -> (Int -> Int)
add x = \y -> x + y

-- Lambda表达式
add = \x y -> x + y
```

### 条件语句

```c
// C/C++
int max(int a, int b) {
    if (a > b) {
        return a;
    } else {
        return b;
    }
}

// 或三元运算符
int max(int a, int b) {
    return (a > b) ? a : b;
}
```

```haskell
-- Haskell
max :: Int -> Int -> Int
max a b = if a > b then a else b

-- 或模式匹配（推荐）
max' :: Int -> Int -> Int
max' a b
    | a > b     = a
    | otherwise = b

-- 使用内置函数
max'' :: Int -> Int -> Int
max'' = max  -- 直接用Prelude里的max
```

---

## 🏗️ 数据结构

### 结构体/记录

```c
// C
struct Point {
    int x;
    int y;
};

struct Point p = {10, 20};
int x_coord = p.x;
```

```cpp
// C++
struct Point {
    int x;
    int y;
    
    int sum() const {
        return x + y;
    }
};

Point p{10, 20};
int x_coord = p.x;
```

```haskell
-- Haskell
data Point = Point {
    x :: Int,
    y :: Int
} deriving (Show, Eq)

-- 使用记录语法创建
p :: Point
p = Point { x = 10, y = 20 }

-- 或使用位置语法
p' :: Point
p' = Point 10 20

-- 访问字段
xCoord :: Int
xCoord = x p

-- 添加方法（用函数）
pointSum :: Point -> Int
pointSum p = x p + y p
```

### 枚举类型

```c
// C
enum Color {
    RED,
    GREEN,
    BLUE
};

enum Color c = RED;
```

```cpp
// C++11
enum class Color {
    Red,
    Green,
    Blue
};

Color c = Color::Red;
```

```haskell
-- Haskell
data Color = Red | Green | Blue
    deriving (Show, Eq)

c :: Color
c = Red

-- 带参数的枚举（类似变体）
data Shape = Circle Float           -- 半径
           | Rectangle Float Float  -- 宽 高
           | Triangle Float Float Float  -- 三边
           deriving (Show)

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h
area (Triangle a b c) = 
    let s = (a + b + c) / 2
    in sqrt (s * (s-a) * (s-b) * (s-c))
```

### 数组/列表

```c
// C
int arr[] = {1, 2, 3, 4, 5};
int first = arr[0];
int len = sizeof(arr) / sizeof(arr[0]);

// 动态数组（C++）
#include <vector>
std::vector<int> vec = {1, 2, 3, 4, 5};
vec.push_back(6);
int first = vec[0];
```

```haskell
-- Haskell（列表是链表，不是数组）
list :: [Int]
list = [1, 2, 3, 4, 5]

first :: Int
first = head list  -- 1

-- 遍历（递归）
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

-- 或用高阶函数
sumList' :: [Int] -> Int
sumList' = sum

-- 添加元素（在头部，O(1)）
newList :: [Int]
newList = 0 : list  -- [0, 1, 2, 3, 4, 5]

-- 如果需要数组（O(1)索引），用Vector
import qualified Data.Vector as V

vec :: V.Vector Int
vec = V.fromList [1, 2, 3, 4, 5]

first' :: Int
first' = V.head vec
```

---

## 🔄 控制流

### 循环

```c
// C - for循环
for (int i = 0; i < 10; i++) {
    printf("%d\n", i);
}

// while循环
int i = 0;
while (i < 10) {
    printf("%d\n", i);
    i++;
}
```

```cpp
// C++11 - range-based for
std::vector<int> vec = {1, 2, 3};
for (auto x : vec) {
    std::cout << x << std::endl;
}

// STL算法
std::for_each(vec.begin(), vec.end(), [](int x) {
    std::cout << x << std::endl;
});
```

```haskell
-- Haskell - 没有循环，用递归或高阶函数

-- 方法1: 递归
printNumbers :: Int -> IO ()
printNumbers n = go 0
  where
    go i
      | i >= n    = return ()
      | otherwise = do
          print i
          go (i + 1)

-- 方法2: 用列表（推荐）
printNumbers' :: Int -> IO ()
printNumbers' n = mapM_ print [0..n-1]

-- 方法3: 处理列表元素
processList :: [Int] -> [Int]
processList xs = map (*2) xs  -- 每个元素乘以2

-- 等价于C++的std::transform
-- 或用列表推导式（类似Python）
processList' :: [Int] -> [Int]
processList' xs = [x * 2 | x <- xs, x > 0]  -- 只处理正数

-- 过滤（类似C++ std::copy_if）
filterPositive :: [Int] -> [Int]
filterPositive = filter (>0)

-- 折叠（类似C++ std::accumulate）
sum' :: [Int] -> Int
sum' = foldl (+) 0
```

### 模式匹配（替代switch/if-else链）

```c
// C - switch
enum Op { ADD, SUB, MUL, DIV };

int calculate(enum Op op, int a, int b) {
    switch (op) {
        case ADD: return a + b;
        case SUB: return a - b;
        case MUL: return a * b;
        case DIV: return b != 0 ? a / b : 0;
        default: return 0;
    }
}
```

```haskell
-- Haskell - 模式匹配（更强大）
data Op = Add | Sub | Mul | Div

calculate :: Op -> Int -> Int -> Maybe Int
calculate Add a b = Just (a + b)
calculate Sub a b = Just (a - b)
calculate Mul a b = Just (a * b)
calculate Div a 0 = Nothing  -- 除零错误
calculate Div a b = Just (a `div` b)

-- 或守卫语法（类似if-else链）
calculate' :: Op -> Int -> Int -> Maybe Int
calculate' op a b = case op of
    Add -> Just (a + b)
    Sub -> Just (a - b)
    Mul -> Just (a * b)
    Div -> if b == 0 then Nothing else Just (a `div` b)

-- 解构嵌套数据
data Expr = Lit Int
          | Add' Expr Expr
          | Mul' Expr Expr

eval :: Expr -> Int
eval (Lit n)     = n
eval (Add' a b)  = eval a + eval b
eval (Mul' a b)  = eval a * eval b

-- 例子：eval (Add' (Lit 1) (Mul' (Lit 2) (Lit 3))) = 7
```

---

## 🧩 类型系统

### 类型别名

```c
// C - typedef
typedef int Age;
typedef float Price;
```

```cpp
// C++ - using
using Age = int;
using Price = float;
```

```haskell
-- Haskell
type Age = Int
type Price = Float

-- 或使用newtype（编译期区分，无运行时开销）
newtype Age' = Age' Int
newtype Price' = Price' Float

-- 使用
age :: Age
age = 25

age' :: Age'
age' = Age' 25  -- 需要显式包装

getAge :: Age' -> Int
getAge (Age' n) = n  -- 需要显式解包
```

### 泛型/模板

```cpp
// C++ - template
template<typename T>
T identity(T x) {
    return x;
}

template<typename T>
T myMax(T a, T b) {
    return (a > b) ? a : b;
}
```

```haskell
-- Haskell - 参数多态（类似泛型，但更简洁）
identity :: a -> a  -- 'a'是类型变量
identity x = x

myMax :: Ord a => a -> a -> a  -- Ord a表示a必须可比较
myMax a b = if a > b then a else b

-- 类型约束
add :: Num a => a -> a -> a  -- Num a表示a必须是数字
add x y = x + y

-- 多个约束
foo :: (Eq a, Show a) => a -> String
foo x = if x == x then show x else "not equal"

-- 高阶类型（类似C++的template template parameter）
map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]
```

### Typeclass（类似C++ Concept/C# Interface）

```cpp
// C++ - concept (C++20)
template<typename T>
concept Printable = requires(T t) {
    { std::to_string(t) } -> std::convertible_to<std::string>;
};

template<Printable T>
void print(T x) {
    std::cout << std::to_string(x) << std::endl;
}

// C++ - interface (抽象基类)
class Drawable {
public:
    virtual void draw() const = 0;
    virtual ~Drawable() = default;
};
```

```haskell
-- Haskell - Typeclass
class Printable a where
    toString :: a -> String

instance Printable Int where
    toString = show

instance Printable Bool where
    toString True  = "true"
    toString False = "false"

print' :: Printable a => a -> IO ()
print' x = putStrLn (toString x)

-- 类似接口（更强大，因为可以自动推导）
class Drawable a where
    draw :: a -> IO ()

data Circle = Circle { radius :: Float }

data Rectangle = Rectangle { width :: Float, height :: Float }

instance Drawable Circle where
    draw c = putStrLn $ "Drawing circle with radius " ++ show (radius c)

instance Drawable Rectangle where
    draw r = putStrLn $ "Drawing rectangle " ++ show (width r) ++ "x" ++ show (height r)

-- 使用
drawAll :: Drawable a => [a] -> IO ()
drawAll = mapM_ draw
```

---

## 💾 内存管理

### 栈 vs 堆

```c
// C - 手动管理
void func() {
    int stack_var = 10;        // 栈
    int* heap_var = malloc(sizeof(int));  // 堆
    *heap_var = 20;
    
    // 使用...
    
    free(heap_var);  // 必须释放！
}
```

```cpp
// C++ - RAII
void func() {
    int stack_var = 10;
    auto heap_var = std::make_unique<int>(20);  // 智能指针
    
    // 使用...
    
}  // 自动释放 heap_var
```

```haskell
-- Haskell - 垃圾回收，无需管理
func :: IO ()
func = do
    let stackVar = 10  -- 可能在栈或堆，由运行时决定
    heapVar <- newIORef 20  -- IORef是可变引用
    
    -- 使用...
    val <- readIORef heapVar
    print val
    
-- 函数退出时，垃圾回收器会自动清理

-- 纯函数（无状态）
pureFunc :: Int -> Int
pureFunc x = x * 2  -- 无副作用，x在哪都无所谓
```

### 可变状态（什么时候需要）

```cpp
// C++ - 可变状态
class Counter {
    int count = 0;
public:
    void increment() { count++; }
    int get() const { return count; }
};
```

```haskell
-- Haskell - 使用IORef（类似C++的shared_ptr<int>）
import Data.IORef

type Counter = IORef Int

newCounter :: IO Counter
newCounter = newIORef 0

increment :: Counter -> IO ()
increment c = modifyIORef c (+1)

getCount :: Counter -> IO Int
getCount = readIORef

-- 使用
testCounter :: IO ()
testCounter = do
    c <- newCounter
    increment c
    increment c
    val <- getCount c
    print val  -- 2

-- 或者：用State Monad（更函数式）
import Control.Monad.State

type CounterState = State Int

increment' :: CounterState ()
increment' = modify (+1)

getCount' :: CounterState Int
getCount' = get

-- 使用（纯函数，可测试）
testCounter' :: Int
testCounter' = execState (increment' >> increment' >> getCount') 0  -- 2
```

---

## 🔧 错误处理

### 空指针/可选值

```cpp
// C++17 - std::optional
std::optional<int> maybeDivide(int a, int b) {
    if (b == 0) return std::nullopt;
    return a / b;
}

auto result = maybeDivide(10, 2);
if (result) {
    std::cout << *result << std::endl;
}
```

```haskell
-- Haskell - Maybe
data Maybe a = Nothing | Just a

maybeDivide :: Int -> Int -> Maybe Int
maybeDivide _ 0 = Nothing
maybeDivide a b = Just (a `div` b)

-- 使用
result :: Maybe Int
result = maybeDivide 10 2  -- Just 5

-- 提取值（必须处理Nothing）
case result of
    Nothing -> putStrLn "Division by zero!"
    Just x  -> print x

-- 或者用Functor/Applicative（更简洁）
doubleResult :: Maybe Int
doubleResult = fmap (*2) result  -- Just 10

-- 链式操作（遇到Nothing自动传播）
chainExample :: Maybe Int
chainExample = do
    x <- maybeDivide 10 2   -- Just 5
    y <- maybeDivide x 0    -- Nothing!
    return (y + 1)          -- 不会执行到这里

-- 结果：Nothing（不是异常！）
```

### 错误传播（类似C++异常，但更安全）

```cpp
// C++ - 异常
double divide(double a, double b) {
    if (b == 0) throw std::runtime_error("Divide by zero");
    return a / b;
}

try {
    auto x = divide(10, 0);
} catch (const std::exception& e) {
    std::cerr << e.what() << std::endl;
}
```

```haskell
-- Haskell - Either
data Either a b = Left a | Right b

divide :: Double -> Double -> Either String Double
divide _ 0 = Left "Divide by zero"
divide a b = Right (a / b)

-- 使用（必须处理错误）
result :: Either String Double
result = divide 10 0

case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right x  -> print x

-- 链式操作（错误传播）
calculate :: Double -> Double -> Either String Double
calculate a b = do
    x <- divide a b
    y <- divide x 2
    return (y + 1)

-- 或显式处理错误
calculate' :: Double -> Double -> Either String Double
calculate' a b =
    case divide a b of
        Left err -> Left ("First division failed: " ++ err)
        Right x  -> case divide x 2 of
            Left err -> Left ("Second division failed: " ++ err)
            Right y  -> Right (y + 1)
```

---

## 🧵 并发编程

### 线程

```c
// C - pthread
#include <pthread.h>

void* thread_func(void* arg) {
    int* val = (int*)arg;
    printf("Thread: %d\n", *val);
    return NULL;
}

int main() {
    pthread_t thread;
    int arg = 42;
    pthread_create(&thread, NULL, thread_func, &arg);
    pthread_join(thread, NULL);
    return 0;
}
```

```cpp
// C++11 - std::thread
#include <thread>
#include <iostream>

void thread_func(int x) {
    std::cout << "Thread: " << x << std::endl;
}

int main() {
    std::thread t(thread_func, 42);
    t.join();
    return 0;
}
```

```haskell
-- Haskell - forkIO（轻量级线程）
import Control.Concurrent

threadFunc :: Int -> IO ()
threadFunc x = putStrLn $ "Thread: " ++ show x

main :: IO ()
main = do
    -- forkIO创建轻量级线程（类似goroutine，不是OS线程）
    tid <- forkIO (threadFunc 42)
    
    -- 做一些工作...
    putStrLn "Main thread working..."
    
    -- 等待线程完成（可选）
    threadDelay 1000000  -- 1秒（微秒）
    
    putStrLn "Done"

-- 使用OS线程（真正的多核并行）
import Control.Concurrent (forkOS)

main' :: IO ()
main' = do
    tid <- forkOS (threadFunc 42)  -- 绑定到OS线程
    -- ...
```

### 共享状态（MVar = 带锁的变量）

```cpp
// C++ - mutex + condition_variable
#include <mutex>
#include <condition_variable>
#include <queue>

std::mutex mtx;
std::condition_variable cv;
std::queue<int> queue;
bool done = false;

void producer() {
    for (int i = 0; i < 10; i++) {
        std::lock_guard<std::mutex> lock(mtx);
        queue.push(i);
        cv.notify_one();
    }
    {
        std::lock_guard<std::mutex> lock(mtx);
        done = true;
    }
    cv.notify_all();
}

void consumer() {
    while (true) {
        std::unique_lock<std::mutex> lock(mtx);
        cv.wait(lock, [] { return !queue.empty() || done; });
        
        if (queue.empty() && done) break;
        
        int val = queue.front();
        queue.pop();
        lock.unlock();
        
        std::cout << val << std::endl;
    }
}
```

```haskell
-- Haskell - MVar（类似带锁的Maybe）
import Control.Concurrent
import Control.Monad (forever)

type Queue a = MVar [a]  -- MVar要么有值，要么为空（阻塞）

producer :: Queue Int -> IO ()
producer q = do
    mapM_ (\i -> modifyMVar_ q (return . (++[i]))) [0..9]
    putMVar q []  -- 发送结束信号

consumer :: Queue Int -> IO ()
consumer q = forever $ do
    val <- takeMVar q  -- 阻塞直到有值
    case val of
        []  -> return ()  -- 结束
        xs  -> do
            mapM_ print xs
            putMVar q []  -- 继续等待

main :: IO ()
main = do
    q <- newMVar []
    forkIO (producer q)
    consumer q

-- 更简洁：用Chan（通道，类似Go的channel）
import Control.Concurrent.Chan

producer' :: Chan Int -> IO ()
producer' ch = do
    mapM_ (writeChan ch) [0..9]
    writeChan ch (-1)  -- 结束信号

consumer' :: Chan Int -> IO ()
consumer' ch = forever $ do
    val <- readChan ch
    if val == -1 
        then return ()
        else print val
```

### STM（软件事务内存）- Haskell独有！

```cpp
// C++ - 没有直接对应，最接近的是数据库事务或STM库（复杂）
// 通常用mutex锁，容易死锁
```

```haskell
-- Haskell - STM（原子操作，自动处理冲突）
import Control.Concurrent.STM

type Account = TVar Int  -- TVar = Transactional Variable

transfer :: Int -> Account -> Account -> STM ()
transfer amount from to = do
    fromBal <- readTVar from
    toBal <- readTVar to
    
    if fromBal >= amount
        then do
            writeTVar from (fromBal - amount)
            writeTVar to (toBal + amount)
        else 
            retry  -- 等待条件满足（类似条件变量）

-- 原子执行
atomically :: STM a -> IO a

main :: IO ()
main = do
    acc1 <- atomically $ newTVar 100
    acc2 <- atomically $ newTVar 50
    
    -- 原子转账
    atomically $ transfer 30 acc1 acc2
    
    bal1 <- atomically $ readTVar acc1
    bal2 <- atomically $ readTVar acc2
    
    putStrLn $ "Account 1: " ++ show bal1  -- 70
    putStrLn $ "Account 2: " ++ show bal2  -- 80

-- 关键优势：
-- 1. 自动处理锁（没有死锁风险）
-- 2. 可组合（多个STM操作可以组合成一个大事务）
-- 3. 乐观并发（重试机制）
```

---

## 📦 模块系统

### 头文件 vs 模块

```c
// C - math.h
#ifndef MATH_H
#define MATH_H

int add(int a, int b);
int sub(int a, int b);

#endif
```

```cpp
// C++ - math.hpp
#pragma once

namespace math {
    int add(int a, int b);
    int sub(int a, int b);
}
```

```haskell
-- Haskell - Math.hs
module Math (
    add,
    sub,
    Point(..),  -- 导出Point类型及其所有构造器
    Shape(Circle, Rectangle),  -- 只导出指定的构造器
) where

-- 函数实现可以直接写在模块里
add :: Int -> Int -> Int
add x y = x + y

sub :: Int -> Int -> Int
sub x y = x - y

data Point = Point { x :: Int, y :: Int }

data Shape = Circle Float
           | Rectangle Float Float
           | Triangle Float Float Float  -- 不导出

-- 内部函数（不导出，类似C static）
internalHelper :: Int -> Int
internalHelper = (*2)
```

### 导入模块

```c
// C
#include "math.h"
#include <stdio.h>

int main() {
    int result = add(1, 2);
    printf("%d\n", result);
    return 0;
}
```

```haskell
-- Haskell
import Math (add)  -- 只导入add
import qualified Math  -- 必须用Math.add调用
import qualified Math as M  -- 用M.add调用
import Math hiding (sub)  -- 导入除了sub之外的所有

-- 标准模块（类似C标准库）
import Data.List (sort, nub)
import Data.Maybe (fromMaybe, isJust)
import Control.Monad (when, unless, forM_)

main :: IO ()
main = do
    let result = add 1 2
    print result
    
    -- 使用qualified导入
    let result2 = Math.sub 5 3
    print result2
    
    -- 使用别名
    let result3 = M.add 2 3
    print result3
```

---

## 🔨 编译和工具

### 编译命令

```bash
# C
gcc -o program main.c -Wall -O2

# C++
g++ -std=c++17 -o program main.cpp -Wall -O2
```

```bash
# Haskell - 直接编译
ghc -O2 -o program Main.hs

# 或使用Stack（推荐，类似Rust Cargo）
stack build
stack exec my-program

# 或使用Cabal（类似npm）
cabal build
cabal run
```

### 交互式环境（REPL）

```bash
# Python风格
$ ghci
GHCi, version 9.4.7: https://www.haskell.org/ghc/  :? for help
ghci> 1 + 2
3
ghci> let x = 5
ghci> x * 2
10
ghci> :type x
x :: Num a => a
ghci> :info Num
class Num a where
  ...
ghci> :load MyModule.hs
ghci> :quit
```

### 项目结构

```
my-project/              # 项目根目录
├── my-project.cabal     # 项目配置（类似CMakeLists.txt）
├── package.yaml         # Stack配置（推荐）
├── stack.yaml           # Stack工具配置
├── src/                 # 源代码
│   ├── Main.hs
│   └── MyModule.hs
├── test/                # 测试
│   └── Spec.hs
└── app/                 # 可执行文件入口
    └── Main.hs
```

```yaml
# package.yaml (Stack)
name: my-project
version: 0.1.0.0
dependencies:
  - base >= 4.14 && < 5
  - text
  - aeson

library:
  source-dirs: src

executables:
  my-project:
    main: Main.hs
    source-dirs: app
    dependencies:
      - my-project

tests:
  my-project-test:
    main: Spec.hs
    source-dirs: test
    dependencies:
      - my-project
      - hspec
```

---

## ⚡ 性能优化

### 严格性（避免thunk堆积）

```haskell
-- 问题：惰性求值可能导致内存泄漏
sumLazy :: [Int] -> Int
sumLazy = foldl (+) 0
-- 实际上构建了一个巨大的表达式：(((0+1)+2)+3)...而不是立即计算

-- 解决：使用严格fold
import Data.List (foldl')

sumStrict :: [Int] -> Int
sumStrict = foldl' (+) 0  -- 立即计算，不堆积thunk

-- 或在类型中使用Bang Patterns
{-# LANGUAGE BangPatterns #-}

sumBang :: [Int] -> Int
sumBang xs = go 0 xs
  where
    go !acc [] = acc  -- !acc表示严格求值
    go !acc (x:xs) = go (acc + x) xs
```

### 内联（类似C inline）

```haskell
-- 使用INLINE/INLINABLE pragma
{-# INLINE add #-}
add :: Int -> Int -> Int
add x y = x + y

-- GHC会自动内联小函数，但你可以强制控制
```

### 未装箱类型（避免GC，类似C）

```haskell
import GHC.Types
import GHC.Prim

-- Int是装箱类型（带GC元数据）
-- Int#是未装箱类型（原始机器整数）

-- 通常不需要手动使用，GHC会自动优化
-- 但性能关键代码可能需要
```

---

## 📝 常用快捷键（GHCi）

| 命令 | 作用 |
|------|------|
| `:load <file>` | 加载模块 |
| `:reload` | 重新加载（修改后） |
| `:type <expr>` | 查看表达式类型 |
| `:info <name>` | 查看函数/类型的信息 |
| `:kind <type>` | 查看类型的kind |
| `:browse <module>` | 浏览模块导出内容 |
| `:set +t` | 每次输出后显示类型 |
| `:set +s` | 显示执行时间和内存 |
| `:quit` | 退出 |

---

## 🔗 更多资源

- **在线转换器**：试试把简单C代码发给OpenCode，让它转成Haskell
- **对比学习**：看到Haskell代码时，想想C++怎么实现
- **实践建议**：每周用Haskell重写一个之前用C写的小工具

**遇到问题？查看 `learning-resources.md` 获取更详细的学习路径！**