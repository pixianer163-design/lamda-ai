# Haskell 快速参考卡

## 🎯 核心概念

### 函数定义
```haskell
-- 模式匹配
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Guards
grade score
    | score >= 90 = "A"
    | score >= 80 = "B"
    | otherwise   = "F"
```

### 类型签名
```haskell
add :: Int -> Int -> Int    -- 接受两个Int，返回Int
add x y = x + y

safeDiv :: Int -> Int -> Maybe Int  -- 可能失败
```

### 列表操作
```haskell
[1,2,3] ++ [4,5]    -- [1,2,3,4,5] (连接)
1 : [2,3]           -- [1,2,3] (cons)
head [1,2,3]        -- 1
 tail [1,2,3]       -- [2,3]
 take 2 [1,2,3]     -- [1,2]
 drop 2 [1,2,3]     -- [3]
```

### 递归模式
```haskell
-- 基本情况 + 递归情况
sumList []     = 0
sumList (x:xs) = x + sumList xs

-- 尾递归（高效）
 factorial n = go n 1
   where
     go 0 acc = acc
     go n acc = go (n - 1) (n * acc)
```

## ⚠️ C→Haskell 注意点

| C | Haskell | 说明 |
|---|---------|------|
| `func(a,b)` | `func a b` | 空格分隔，非括号 |
| `a % b` | `mod a b` 或 ``a `mod` b`` | 函数调用 |
| `arr[i]` | `arr !! i` | 少用，O(n) |
| `if (a) {..}` | `if a then .. else ..` | 必须有两个分支 |
| `for/while` | 递归/高阶函数 | 函数式思维 |
| `x = 5; x = 6` | ❌ 错误！ | 变量不可变 |

## 🔧 常用命令

```bash
# GHCi 交互环境
ghci                           # 启动
:load File.hs                  # 加载文件
:reload                        # 重新加载
:type func                     # 查看类型
:quit                          # 退出

# Stack 项目
stack build                    # 构建项目
stack exec project-name        # 运行
stack test                     # 运行测试
stack ghci                     # 项目REPL
```

## 📐 缩进规则

- 使用 **4个空格**（不用Tab）
- 同层级必须对齐
- `where` 缩进2-4格

```haskell
func x y =
    let a = x + y
        b = x * y      -- 对齐
    in a + b
```

## 🎓 学习路线图

```
Week 1: 基础语法 ✓
   → 函数、递归、列表

Week 2: 类型系统
   → Maybe/Either、自定义类型

Week 3-4: 高阶函数
   → Functor、Applicative、Monad

Week 5: 工程实践
   → Stack项目、测试、性能

Week 6+: 并发 & 实际项目
   → STM、λ-Py代码阅读
```

## 📚 推荐资源

- **入门**: Learn You a Haskell (免费在线)
- **进阶**: Real World Haskell (免费在线)
- **数学**: Category Theory for Programmers (GitHub免费PDF)
- **类型**: Types and Programming Languages (需购买)

---
*λ-Py EAP Haskell Learning*
