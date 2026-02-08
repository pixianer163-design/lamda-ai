# Haskell 学习快速入门指南

> **目标**: 30分钟内搭建环境并运行第一个Haskell程序

---

## 🚀 第一步：安装 Haskell（10分钟）

### Linux / macOS

```bash
# 安装 GHCup（Haskell工具链管理器）
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# 按提示操作，安装完成后重启终端或运行：
source ~/.ghcup/env

# 验证安装
ghc --version      # 应该显示 GHC 版本
stack --version    # Stack版本
cabal --version    # Cabal版本
```

### Windows

1. **下载安装程序**: https://www.haskell.org/ghcup/
2. **或使用 PowerShell**:
```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072;Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true
```

### 验证安装

```bash
# 测试GHC（Glasgow Haskell Compiler）
$ ghci
GHCi, version 9.4.7: https://www.haskell.org/ghc/  :? for help
ghci> 1 + 2
3
ghci> let x = 5
ghci> x * 2
10
ghci> :quit
```

✅ **成功标志**: 看到 `3` 和 `10` 的输出

---

## 📁 第二步：创建学习项目（5分钟）

```bash
# 进入学习目录
cd /mnt/d/project/lamda-ai/haskell-learning

# 运行初始化脚本
./init-haskell-project.sh my-haskell-learning

# 进入项目
cd my-haskell-learning

# 查看项目结构
tree  # 或用 ls -R
```

你应该看到：
```
my-haskell-learning/
├── app/
│   └── Main.hs
├── src/
│   ├── Basics.hs
│   ├── Types.hs
│   └── Concurrency.hs
├── test/
│   └── Spec.hs
├── package.yaml
├── stack.yaml
└── README.md
```

---

## 🔨 第三步：构建和运行（10分钟）

```bash
# 第一次构建（会下载依赖，可能需要几分钟）
stack build

# 运行程序
stack exec my-haskell-learning-exe

# 运行测试
stack test

# 进入交互环境
stack ghci
```

### 预期输出

运行 `stack exec my-haskell-learning-exe` 后应该看到：
```
========================================
Haskell Learning Project for C Programmers
========================================

=== Basics ===
add 3 5 = 8
factorial 5 = 120
fibonacci 10 = 55
myLength [1,2,3] = 3
myMap (*2) [1,2,3] = [2,4,6]
quickSort [3,1,4,1,5,9,2,6] = [1,1,2,3,4,5,6,9]

=== Types ===
Point 3 4 = Point {x = 3, y = 4}
Circle area = 78.53982
Rectangle area = 200.0
safeDiv 10 2 = Just 5
safeDiv 10 0 = Nothing
...
```

✅ **成功标志**: 程序正常运行并输出结果

---

## 🎯 第四步：第一次修改（5分钟）

打开 `app/Main.hs` 文件，找到以下代码：

```haskell
main :: IO ()
main = do
    putStrLn "========================================"
    putStrLn "Haskell Learning Project for C Programmers"
    putStrLn "========================================\n"
    
    -- 基础练习
    putStrLn "=== Basics ==="
    putStrLn $ "add 3 5 = " ++ show (add 3 5)
```

在 `-- 基础练习` 下面添加一行：

```haskell
    putStrLn $ "Hello from C programmer!"
    putStrLn $ "add 3 5 = " ++ show (add 3 5)
```

然后重新构建运行：

```bash
stack build
stack exec my-haskell-learning-exe
```

你应该看到你的新消息出现在输出中！

---

## 📚 第五步：开始学习

### 今天的任务（30分钟）

1. **阅读**: `src/Basics.hs` 文件，理解基础语法
2. **修改**: 在 `Main.hs` 中添加你自己的测试代码
3. **实验**: 在 GHCi 中尝试以下命令：

```bash
stack ghci
ghci> :load src/Basics.hs
ghci> add 10 20
30
ghci> factorial 10
3628800
ghci> quickSort [5,2,8,1,9,3]
[1,2,3,5,8,9]
ghci> :type add
add :: Int -> Int -> Int
ghci> :info Point
data Point = Point {x :: Int, y :: Int}
ghci> :quit
```

### 本周计划

| 天数 | 任务 | 目标 |
|------|------|------|
| **Day 1** | 完成安装和第一个程序 | 能运行并修改代码 |
| **Day 2** | 阅读 Basics.hs | 理解函数、递归、列表操作 |
| **Day 3** | 完成《Haskell Tutorial for C Programmers》第1-3章 | 建立概念映射 |
| **Day 4** | 阅读 Types.hs | 理解代数数据类型、Maybe、Either |
| **Day 5** | 实现一个小练习 | 用Haskell写一个之前用C写过的工具 |
| **Day 6-7** | 复习和总结 | 整理学习笔记 |

---

## 🔧 常见问题

### Q: 构建时卡住/很慢？

```bash
# 使用镜像加速（中国用户）
echo 'setup-info: "http://mirrors.tuna.tsinghua.edu.cn/stackage/stack-setup.yaml"' >> ~/.stack/config.yaml
echo 'urls:
  latest-snapshot: http://mirrors.tuna.tsinghua.edu.cn/stackage/snapshots.json' >> ~/.stack/config.yaml

# 或使用阿里云
stack build --resolver lts-21.0
```

### Q: GHCi 提示符不出现？

```bash
# 确保加载了模块
stack ghci
ghci> :load src/Basics.hs
ghci> add 1 2
```

### Q: 修改后没有生效？

```bash
# 必须重新构建
stack build

# 或者直接在GHCi中重载
ghci> :reload
```

### Q: 内存不足？

```bash
# 限制GHC内存使用
stack build --ghc-options="+RTS -M2G -RTS"
```

---

## 📖 下一步

完成快速入门后：

1. **阅读详细学习资料**: 查看 `learning-resources.md`
2. **使用速查表**: 查看 `c-to-haskell-cheatsheet.md`
3. **开始练习**: 修改 `src/Basics.hs` 中的TODO注释
4. **向我提问**: 把代码贴给我，我帮你review

### 本周要掌握的概念

- [ ] 函数定义和调用
- [ ] 类型签名（对比C函数签名）
- [ ] 列表操作（map/filter/fold）
- [ ] 模式匹配（对比C的switch）
- [ ] Maybe类型（对比NULL指针检查）

### 小测验

当你可以不看资料完成以下内容时，说明你已经入门了：

```haskell
-- 1. 写一个函数计算列表平均值（处理空列表）
average :: [Double] -> Maybe Double

-- 2. 写一个函数查找列表最大值（使用递归）
findMax :: Ord a => [a] -> Maybe a

-- 3. 实现 safeIndex（对比C的数组越界）
safeIndex :: [a] -> Int -> Maybe a

-- 4. 写一个函数反转列表（不使用reverse）
myReverse :: [a] -> [a]
```

---

## 💡 学习提示

### 给C程序员的心理建设

1. **不要试图理解一切** - 先会用，再理解原理
2. **编译错误是正常的** - Haskell的错误信息通常比C更详细
3. **忘记指针** - 没有指针算术，没有内存泄漏
4. **拥抱不可变性** - 变量就是常量，用新值代替修改
5. **类型是你的朋友** - 编译器会帮你捕获80%的错误

### 日常练习建议

```bash
# 每天打开终端就执行：
cd /mnt/d/project/lamda-ai/haskell-learning/my-haskell-learning
stack ghci

# 然后在GHCi中：
ghci> :load src/Basics.hs
ghci> -- 测试各种函数
ghci> :quit
```

### 遇到困难时

1. 查看 `c-to-haskell-cheatsheet.md` 找到对应概念
2. 在GHCi中用 `:type` 和 `:info` 查看类型信息
3. 把错误信息贴给我，我帮你分析
4. 记录问题，每周复习一次

---

**准备好了吗？开始你的Haskell之旅吧！** 🚀

有任何问题随时问我。
