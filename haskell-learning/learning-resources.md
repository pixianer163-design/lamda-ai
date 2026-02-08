# Haskell 学习资料大全（C/C++程序员专用版）

> **适用对象**: 有C/C++基础，想快速上手Haskell的程序员  
> **学习目标**: 能看懂并修改 λ-Py EAP 项目的Haskell代码  
> **预计时间**: 4-6周达到可用水平

---

## 📚 必读资源（按优先级排序）

### 🥇 第一优先级：快速入门（Week 1-2）

#### 1. 《Haskell Tutorial for C Programmers》⭐⭐⭐⭐⭐
- **链接**: http://www.haskell.org/haskellwiki/Haskell_tutorial_for_C_programmers
- **为什么适合你**: 专门写给C程序员，直接对比概念
- **内容**: 
  - C指针 vs Haskell函数
  - C结构体 vs Haskell代数数据类型
  - C回调函数 vs Haskell高阶函数
- **时间**: 3-4小时

#### 2. 《What I Wish I Knew When Learning Haskell》⭐⭐⭐⭐⭐
- **链接**: http://dev.stephendiehl.com/hask/
- **为什么适合你**: 作者也是C++背景，理解你的痛点
- **内容**: 
  - 语法速查（对比C++）
  - 常见陷阱（内存管理思维转换）
  - 工具链（cabal/stack/ghc 对比 gcc/make）
- **时间**: 2-3小时

#### 3. 《Learn You a Haskell for Great Good!》（社区版）⭐⭐⭐⭐
- **链接**: https://learnyouahaskell.github.io/
- **为什么适合你**: 最友好的入门书，有图有真相
- **重点章节**:
  - Chapter 1-3: 基础语法（读完就能写简单程序）
  - Chapter 8: 类型和Typeclass（理解多态）
  - Chapter 11: Functor/Applicative/Monad（难点，慢慢啃）
- **时间**: 每天1章，共12章

---

### 🥈 第二优先级：深入理解（Week 3-4）

#### 4. 《Real World Haskell》⭐⭐⭐⭐
- **链接**: http://book.realworldhaskell.org/
- **为什么适合你**: 工程实践导向，不是纯理论
- **重点章节**:
  - Chapter 1-3: 快速复习基础
  - Chapter 4: 函数式思维（思维转换关键）
  - Chapter 24: 并发和并行（STM、MVar）
  - Chapter 27: 性能调优（C程序员最关心的）
- **时间**: 选择性阅读，重点章节精读

#### 5. 《CIS 194: Introduction to Haskell》(UPenn课程)⭐⭐⭐⭐⭐
- **链接**: http://www.seas.upenn.edu/~cis194/spring13/lectures.html
- **为什么适合你**: 大学课程，系统且深入
- **内容**:
  - 10个讲座，从基础到Monad
  - 有作业可以练习
  - 有视频（YouTube搜 CIS 194 Haskell）
- **时间**: 每周2讲，共5周

#### 6. 《You Could Have Invented Monads》⭐⭐⭐⭐
- **链接**: http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html
- **为什么适合你**: 从直觉出发理解Monad，不用数学背景
- **时间**: 30分钟阅读，终身受益

---

### 🥉 第三优先级：工程实战（Week 5-6）

#### 7. 《Write Yourself a Scheme in 48 Hours》⭐⭐⭐⭐
- **链接**: https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours
- **为什么适合你**: 动手写一个解释器，理解函数式编程精髓
- **时间**: 实际上需要2周，但非常值得

#### 8. 《24 Days of Hackage》⭐⭐⭐
- **链接**: https://ocharles.org.uk/blog/
- **为什么适合你**: 介绍Haskell生态的常用库
- **重点**: 
  - Day 1: Stack（项目管理）
  - Day 5: Lens（数据操作）
  - Day 12: Servant（Web API）
  - Day 18: STM（并发）

#### 9. 《Haskell for Rust Programmers》⭐⭐⭐⭐
- **链接**: https://github.com/haskellfoundation/haskell-for-rust-programmers（搜索相关博客）
- **为什么适合你**: Rust和C++都有RAII和所有权概念，对比学习效果好
- **内容**:
  - 所有权系统对比
  - 类型系统对比
  - 性能优化对比

---

## 🛠️ 工具和环境

### 安装Haskell（选择一种）

#### 方案A: GHCup（推荐）
```bash
# Linux/Mac
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Windows（PowerShell）
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072;Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true
```

#### 方案B: Stack（项目管理友好）
```bash
curl -sSL https://get.haskellstack.org/ | sh
```

### 编辑器配置

| 编辑器 | 推荐插件/配置 |
|--------|--------------|
| **VS Code** | Haskell插件（由Haskell Foundation维护） |
| **Vim/Neovim** | coc.nvim + coc-haskell 或 haskell-language-server |
| **Emacs** | haskell-mode + lsp-haskell |
| **IntelliJ** | IntelliJ-Haskell插件 |

### 在线练习环境

- **Haskell Playground**: https://play.haskell.org/（无需安装）
- **Try Haskell**: http://tryhaskell.org/（交互式教程）
- **CodeWorld**: https://code.world/haskell（图形化学习）

---

## 🎯 C/C++ → Haskell 概念映射表

### 基础语法对比

| C/C++ | Haskell | 说明 |
|-------|---------|------|
| `int x = 5;` | `x = 5` | 变量不可变（默认） |
| `int* ptr = &x;` | 无直接对应 | 没有指针，用引用透明 |
| `struct Point { int x; int y; };` | `data Point = Point { x :: Int, y :: Int }` | 代数数据类型 |
| `typedef enum { Red, Green, Blue } Color;` | `data Color = Red | Green | Blue` | 代数数据类型 |
| `if (x > 0) { ... }` | `if x > 0 then ... else ...` | 必须有两个分支 |
| `for (int i=0; i<n; i++)` | `map f [0..n-1]` 或 `forM_ [0..n-1] $ \i -> ...` | 用高阶函数代替循环 |
| `void func(int x)` | `func :: Int -> ()` | 无返回值用 `()` |
| `int func(int x)` | `func :: Int -> Int` | 纯函数签名 |

### 内存管理对比

| C/C++ | Haskell | 说明 |
|-------|---------|------|
| `malloc/free` | 无 | 垃圾回收器自动管理 |
| `new/delete` | 无 | 垃圾回收器自动管理 |
| `shared_ptr<T>` | 引用计数（较少用） | 通常依赖GC |
| `unique_ptr<T>` | 线性类型（GHC 9.0+）| 编译期保证唯一所有权 |
| RAII | `bracket` 模式 | 使用 `bracket acquire release use` |

### 错误处理对比

| C/C++ | Haskell | 说明 |
|-------|---------|------|
| `return NULL` | `Maybe a` | `Just x` 或 `Nothing` |
| `errno` | `Either Error a` | `Left err` 或 `Right x` |
| `throw/catch` | `Exception` Monad | 较少用，推荐用Either |
| `assert` | 编译期类型检查 | 类型系统保证 |

### 并发对比

| C/C++ | Haskell | 说明 |
|-------|---------|------|
| `pthread_create` | `forkIO` | 轻量级线程 |
| `mutex` | `MVar` | 可变变量 |
| `pthread_rwlock` | `TVar` + `STM` | 软件事务内存 |
| `atomic` | `atomicModifyIORef` | 原子操作 |
| `thread_local` | 无 | 使用Reader Monad传递上下文 |

---

## 📖 专题深入

### Monad 理解路径（给C程序员）

```
第一步：忘掉"Monad"这个词
       ↓
第二步：理解 Functor（fmap 就是 C++ 的 transform）
       ↓
第三步：理解 Applicative（pure 就是包装，<*> 就是应用）
       ↓
第四步：理解 Monad（>>= 就是链式操作，类似 Promise.then）
       ↓
第五步：实际使用 Maybe/IO/List Monad
       ↓
第六步：回头看理论（如果你感兴趣）
```

**关键顿悟**：
- Monad 是**设计模式**，不是数学概念
- 就像 C++ 的 `operator<<` 链式调用
- 就像 JavaScript 的 Promise `.then().then()`

### 类型系统深入

| C++ 概念 | Haskell 对应 | 学习资源 |
|----------|-------------|----------|
| Template | Parametric Polymorphism | GADTs章节 |
| Concept (C++20) | Typeclass | Typeclassopedia |
| SFINAE | Type Families | GHC User Guide |
| CRTP | Typeclass + Associated Types | 24 Days of Hackage |

---

## 💻 练习项目建议

### Level 1: 语法熟悉（Week 1）
1. **计算器**: 实现 `eval "1+2*3"`（用递归下降解析）
2. **链表**: 实现 `map/filter/reverse`（理解递归和模式匹配）
3. **二叉树**: 实现插入/查找/遍历（理解代数数据类型）

### Level 2: 类型系统（Week 2）
1. **安全计算器**: 类型区分 `Safe` 和 `Unsafe` 操作
2. **状态机**: 用类型表示状态转换（编译期防止无效状态）
3. **简单的类型检查器**: 实现 lambda 演算的类型推导

### Level 3: 实际应用（Week 3-4）
1. **CLI工具**: 文件处理、配置解析、日志记录
2. **Web服务**: 用Servant写REST API（对比C++的Crow/POCO）
3. **并发程序**: 生产者-消费者、线程池（对比C++的线程库）

### Level 4: λ-Py 相关（Week 5-6）
1. **实现Free Monad**: 理解 λ-Py 的核心模式
2. **gRPC服务**: 与Python服务通信
3. **阅读 λ-Py 代码**: 我带你逐行分析

---

## 🔍 调试和排错

### 常见编译错误（C程序员视角）

```haskell
-- 错误1：忘记函数参数
func x y = x + y
result = func 1    -- 错误：缺少参数
result = func 1 2  -- 正确

-- 错误2：类型不匹配
x :: Int
x = "hello"        -- 错误：期望Int，得到String

-- 错误3：变量不可变
x = 5
x = 6              -- 错误：不能重新绑定

-- 错误4：IO操作在纯函数中
pureFunc :: Int -> Int
pureFunc x = do     -- 错误：纯函数不能用do
  print x           -- 错误：IO操作
  return (x + 1)

-- 修正：分离纯函数和IO
pureFunc :: Int -> Int
pureFunc x = x + 1

ioFunc :: Int -> IO ()
ioFunc x = do
  print x
  print (pureFunc x)
```

### 调试工具

```bash
# GHCi（交互式环境，类似Python解释器）
ghci
> :load MyModule.hs
> :type myFunction
> :info MyType
> :set +s  # 显示执行时间

# 编译优化
ghc -O2 -threaded -rtsopts MyProgram.hs
./MyProgram +RTS -N4 -s  # 使用4核，显示统计信息

# 性能分析
ghc -prof -fprof-auto MyProgram.hs
./MyProgram +RTS -p
# 生成 MyProgram.prof 文件
```

---

## 📞 获取帮助

### 社区资源

| 平台 | 用途 | 链接 |
|------|------|------|
| **Haskell Reddit** | 日常问答 | https://www.reddit.com/r/haskell/ |
| **Stack Overflow** | 具体问题 | Tag: haskell |
| **Matrix** | 实时聊天 | #haskell:matrix.org |
| **Discord** | 新手友好 | Haskell Foundation Discord |
| **邮件列表** | 深度讨论 | haskell-cafe |

### 提问技巧

1. **提供最小可复现代码**（和C++一样）
2. **包含完整的错误信息**（不要只给一部分）
3. **说明你期望的行为和实际行为**
4. **提及你的GHC版本**（`ghc --version`）

---

## ✅ 学习检查清单

### Week 1 目标
- [ ] 安装GHCup和Stack
- [ ] 在GHCi中运行基础表达式
- [ ] 写一个递归函数（如阶乘）
- [ ] 定义和使用代数数据类型
- [ ] 理解模式匹配

### Week 2 目标
- [ ] 实现map/filter/reduce
- [ ] 理解Typeclass（Eq, Ord, Show）
- [ ] 使用Maybe处理可能失败的计算
- [ ] 使用Either处理错误
- [ ] 写一个简单IO程序（读写文件）

### Week 3 目标
- [ ] 理解Functor（fmap）
- [ ] 理解Applicative
- [ ] 理解Monad（>>=）
- [ ] 使用do语法
- [ ] 实现一个简单的State Monad

### Week 4 目标
- [ ] 理解Lazy Evaluation
- [ ] 理解严格性（Bang Patterns）
- [ ] 基本并发（forkIO, MVar）
- [ ] 使用STM
- [ ] 性能分析（profiling）

### Week 5-6 目标
- [ ] 阅读 λ-Py 项目的控制平面代码
- [ ] 理解Free Monad架构
- [ ] 修改一个小功能
- [ ] 提交第一个PR

---

## 🚀 立即开始

### 今天就开始（30分钟）

1. **安装Haskell**（10分钟）
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
   ```

2. **运行第一个程序**（10分钟）
   ```haskell
   -- hello.hs
   main = putStrLn "Hello, Haskell!"
   ```
   ```bash
   ghc hello.hs
   ./hello
   ```

3. **在GHCi中实验**（10分钟）
   ```bash
   ghci
   > 1 + 2
   > let x = 5
   > x * 2
   > :t x  -- 查看类型
   > :q    -- 退出
   ```

### 本周任务

- [ ] 完成《Haskell Tutorial for C Programmers》前3章
- [ ] 用Haskell实现快速排序
- [ ] 定义一个二叉树类型并实现遍历
- [ ] 向我展示你的代码，我帮你review

---

## 📚 λ-Py 专用参考资料

当你阅读 λ-Py 代码时需要了解的概念：

| λ-Py 中的概念 | 学习资源 | 优先级 |
|--------------|----------|--------|
| Free Monad | 《Free Monads for Less》| P0 |
| Tagless Final | 《Typed Tagless Final》| P0 |
| STM | 《Beautiful Concurrency》| P0 |
| Servant | Servant官方教程 | P1 |
| gRPC | grpc-haskell文档 | P1 |
| Lens | 《Lens over Tea》| P2 |

---

**准备好开始了吗？有任何问题随时问我！** 🎉
