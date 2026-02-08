# 🎓 Haskell 学习资料包（C/C++程序员专用）

> **目标**: 帮助有C/C++背景的程序员在4-6周内掌握Haskell，达到能阅读和理解λ-Py项目代码的水平

---

## 📂 资料包内容

```
haskell-learning/
├── README.md                          # 本文件（总览）
├── QUICKSTART.md                      # 快速入门（30分钟上手）
├── learning-resources.md              # 详细学习资源
├── c-to-haskell-cheatsheet.md         # C→Haskell速查表
├── init-haskell-project.sh            # 项目初始化脚本
└── my-haskell-learning/              # 学习项目（运行脚本后生成）
    ├── src/
    │   ├── Basics.hs                 # 基础语法练习
    │   ├── Types.hs                  # 类型系统练习
    │   └── Concurrency.hs            # 并发编程练习
    ├── app/Main.hs                   # 主程序
    ├── test/Spec.hs                  # 测试
    └── README.md                     # 项目说明
```

---

## 🚀 开始学习的3个步骤

### 第一步：阅读快速入门（15分钟）

**文件**: `QUICKSTART.md`

**内容**:
- 安装Haskell环境
- 创建第一个项目
- 运行示例程序
- 修改并重新运行

**目标**: 让程序跑起来，建立信心

---

### 第二步：查阅速查表（持续使用）

**文件**: `c-to-haskell-cheatsheet.md`

**如何使用**:
1. 遇到Haskell代码看不懂 → 查对应C/C++写法
2. 想写某个功能 → 查Haskell怎么实现
3. 对比学习 → 建立概念映射

**核心章节**:
- 基础语法对比
- 数据结构（struct → data）
- 错误处理（NULL → Maybe）
- 内存管理（malloc/free → GC）
- 并发编程（pthread → forkIO/STM）

---

### 第三步：系统学习（4-6周）

**文件**: `learning-resources.md`

**学习路径**:

| 周数 | 重点 | 资料 | 实践 |
|------|------|------|------|
| **Week 1** | 基础语法 | 《Haskell Tutorial for C Programmers》 | 完成 Basics.hs |
| **Week 2** | 类型系统 | 《What I Wish I Knew When Learning Haskell》 | 完成 Types.hs |
| **Week 3** | Monad | 《You Could Have Invented Monads》 | 理解Maybe/IO |
| **Week 4** | 并发 | 《Beautiful Concurrency》 | 完成 Concurrency.hs |
| **Week 5** | 工程实践 | 《Real World Haskell》选读 | 写一个小工具 |
| **Week 6** | λ-Py实战 | 阅读项目代码 | 修改一个小功能 |

---

## 🎯 学习目标检查点

### Week 1 结束时应能：
- [ ] 独立搭建Haskell环境
- [ ] 理解函数定义和类型签名
- [ ] 使用递归处理列表
- [ ] 区分纯函数和IO操作
- [ ] 编写简单的命令行程序

### Week 2 结束时应能：
- [ ] 定义代数数据类型（data）
- [ ] 使用Maybe处理可能失败的计算
- [ ] 使用Either处理错误
- [ ] 理解Typeclass（类似C++ Concept）
- [ ] 实现简单的类型安全封装

### Week 3 结束时应能：
- [ ] 理解Functor/Applicative/Monad
- [ ] 使用do语法编写链式操作
- [ ] 处理IO操作（文件、网络）
- [ ] 使用State Monad管理状态
- [ ] 解释"Monad就是可组合的上下文"

### Week 4 结束时应能：
- [ ] 使用forkIO创建轻量级线程
- [ ] 使用MVar进行线程间通信
- [ ] 使用STM实现原子操作
- [ ] 理解Haskell的并发优势
- [ ] 编写无死锁的并发程序

### Week 5 结束时应能：
- [ ] 使用Stack管理项目
- [ ] 编写HSpec测试
- [ ] 进行性能分析和优化
- [ ] 使用常见库（text, containers, mtl）
- [ ] 理解Lazy Evaluation的影响

### Week 6 结束时应能：
- [ ] 阅读λ-Py项目的Haskell代码
- [ ] 理解Free Monad架构
- [ ] 修改一个小功能并测试
- [ ] 审查AI生成的Haskell代码
- [ ] 向团队讲解Haskell代码

---

## 💡 学习建议

### 对于C程序员

1. **忘记指针和内存管理** - Haskell有GC，专注于逻辑
2. **拥抱不可变性** - 不要试图"修改"变量，创建新值
3. **类型即文档** - 认真阅读类型签名，它们告诉你一切
4. **编译器是助手** - 错误信息详细，跟着它走
5. **从简单开始** - 不要一上来就看Category Theory

### 每日学习流程（建议）

```
第1-20分钟：阅读理论
  ↓ 看学习资料的一章
  
第21-50分钟：动手实践
  ↓ 在GHCi中实验
  ↓ 修改练习代码
  
第51-60分钟：总结和提问
  ↓ 记录不理解的地方
  ↓ 准备问题清单
  ↓ 向我提问
```

### 常见问题解决

| 问题 | 解决方案 |
|------|----------|
| 编译错误看不懂 | 查看速查表的"常见编译错误"章节 |
| 不理解某个概念 | 在learning-resources.md中找到对应资源 |
| 不知道某个函数怎么用 | 在GHCi中用`:type`和`:info`查看 |
| 想对比C和Haskell | 查看速查表对应章节 |
| 代码运行慢 | 查看性能优化章节，理解严格性 |

---

## 📚 重点资源速览

### 必读（优先级排序）

1. **《Haskell Tutorial for C Programmers》**
   - 专门写给C程序员
   - 直接对比概念
   - 2-3小时读完

2. **《What I Wish I Knew When Learning Haskell》**
   - 作者也是C++背景
   - 工程实践导向
   - 避免常见陷阱

3. **《Learn You a Haskell for Great Good!》**
   - 最友好的入门书
   - 插图丰富
   - 社区维护版更新

4. **《You Could Have Invented Monads》**
   - 理解Monad的最佳文章
   - 从直觉出发
   - 30分钟阅读

### 实践项目

1. **计算器** - 基础语法练习
2. **排序算法** - 递归和列表操作
3. **类型安全银行系统** - 错误处理和类型系统
4. **并发下载器** - forkIO和MVar
5. **Web API客户端** - IO和Monad

---

## 🔧 工具链

### 安装
```bash
# 使用 GHCup（推荐）
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

### 编辑器
- **VS Code**: Haskell插件
- **Vim/Neovim**: haskell-language-server
- **Emacs**: haskell-mode

### 常用命令
```bash
stack build          # 构建
stack exec <name>    # 运行
stack test           # 测试
stack ghci           # 交互环境
stack repl           # 同上
```

---

## 🤝 如何获得帮助

### 向我提问的最佳方式

1. **提供代码上下文**
   ```haskell
   -- 把完整代码贴出来
   myFunction :: Int -> Int
   myFunction x = ...
   ```

2. **说明你的C背景**
   ```
   "我在C中会这样写：...
   在Haskell中应该怎么实现？"
   ```

3. **提供错误信息**
   ```
   错误信息：...
   我理解的错误原因是：...
   我的疑问是：...
   ```

4. **说明学习目标**
   ```
   我想实现的功能是：...
   我在λ-Py中看到的代码是：...
   ```

### 我可以帮你

- ✅ 解释Haskell代码
- ✅ 把C代码转成Haskell
- ✅ Review你的代码
- ✅ 解释编译错误
- ✅ 推荐学习资源
- ✅ 解答概念问题

---

## 📊 进度跟踪

### Week 1 检查清单

- [ ] 成功安装GHCup
- [ ] 运行第一个Hello World
- [ ] 完成 `Basics.hs` 的所有练习
- [ ] 能在GHCi中测试函数
- [ ] 理解 `map/filter/fold`

### Week 2 检查清单

- [ ] 定义了3个以上的data类型
- [ ] 使用Maybe处理空值
- [ ] 使用Either处理错误
- [ ] 实现了一个Typeclass实例
- [ ] 对比了Haskell和C的错误处理方式

### Week 3 检查清单

- [ ] 理解了Functor的fmap
- [ ] 用do语法写过IO程序
- [ ] 解释了"什么是Monad"（用自己的话）
- [ ] 使用了Maybe Monad/Either Monad
- [ ] 编写了一个有多个IO操作的程序

### Week 4 检查清单

- [ ] 使用forkIO创建了线程
- [ ] 使用MVar共享状态
- [ ] 使用STM实现原子操作
- [ ] 理解了STM的优势
- [ ] 编写了一个并发程序

### Week 5-6 检查清单

- [ ] 阅读了λ-Py的控制平面代码
- [ ] 理解了Free Monad的概念
- [ ] 修改了一个功能并测试通过
- [ ] 向（虚拟）团队讲解了代码
- [ ] 有信心审查AI生成的Haskell代码

---

## 🎉 成功标准

当你完成这个学习包后，你应该能够：

1. **独立阅读Haskell代码** - 不借助翻译，直接理解
2. **修改业务逻辑** - 在λ-Py项目中添加新功能
3. **审查AI生成代码** - 判断代码质量和安全性
4. **指导初级工程师** - 解答基础Haskell问题
5. **做出架构决策** - 判断何时用Haskell，何时用Python

---

## 🚀 现在开始！

1. 打开 `QUICKSTART.md`
2. 跟着步骤安装环境
3. 运行第一个程序
4. 在GHCi中实验
5. 每天向我汇报进度

**有任何问题随时提问！** 🎓
