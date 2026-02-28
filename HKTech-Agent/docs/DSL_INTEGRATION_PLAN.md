# DSL 集成计划

**目标**: 将 DSL 策略编译系统集成到 HKTech-Agent  
**优先级**: P1  
**预计时间**: 1 小时

---

## 📋 集成方案

### 方案 A: 深度集成（推荐）⭐

将 DSL 编译器作为 HKTech-Agent 的策略生成引擎。

**架构**:
```
HKTech-Agent
├── factory/
│   ├── dsl_compiler.py      # DSL 编译器集成 ⭐
│   ├── strategy_loader.py   # 策略加载器 ⭐
│   └── agent_factory.py     # 现有工厂（修改）
├── dsl_strategies/          # DSL 策略目录 ⭐
│   ├── rsi_mean_reversion.dsl
│   ├── bollinger_breakout.dsl
│   └── ...
└── generated_strategies/    # 生成的 Python 策略 ⭐
    ├── rsi_strategy.py
    └── ...
```

**优势**:
- ✅ 统一管理 DSL 和 Python 策略
- ✅ 支持热加载 DSL 策略
- ✅ 完整的错误诊断
- ✅ 编译缓存优化

---

## 🔧 实施步骤

### Step 1: 复制 DSL 模块

```bash
# 复制 DSL 核心模块
cp -r /root/.openclaw/workspace/dsl_meta_learning/dsl \
      /root/.openclaw/workspace/Lamda-ai/HKTech-Agent/factory/dsl/

# 复制策略模板
cp /root/.openclaw/workspace/dsl_meta_learning/examples/strategies/*.dsl \
   /root/.openclaw/workspace/Lamda-ai/HKTech-Agent/dsl_strategies/
```

### Step 2: 创建集成模块

**文件**: `factory/dsl_compiler.py`

```python
#!/usr/bin/env python3
"""
DSL 编译器集成 - HKTech-Agent 版本
"""

import os
from pathlib import Path
from typing import Optional, Dict

# 导入 DSL 模块
from .dsl.compiler import DSLParser
from .dsl.cache import CompilationCache

class DSLCompiler:
    """
    HKTech-Agent DSL 编译器
    
    功能:
    1. 编译 DSL 策略为 Python
    2. 缓存编译结果
    3. 热加载策略
    4. 错误诊断
    """
    
    def __init__(self, cache_enabled: bool = True):
        self.parser = DSLParser(use_cache=cache_enabled)
        self.cache = CompilationCache() if cache_enabled else None
        self.compiled_strategies: Dict = {}
    
    def compile_strategy(self, dsl_file: str, output_dir: str = None) -> str:
        """编译单个 DSL 策略"""
        pass
    
    def load_strategy(self, strategy_name: str):
        """加载已编译的策略"""
        pass
    
    def get_strategy(self, strategy_name: str):
        """获取策略实例"""
        pass
```

### Step 3: 创建策略加载器

**文件**: `factory/strategy_loader.py`

```python
#!/usr/bin/env python3
"""
策略加载器 - 支持 DSL 和 Python 策略
"""

from typing import Optional, Dict
from .dsl_compiler import DSLCompiler

class StrategyLoader:
    """
    统一策略加载器
    
    支持:
    - DSL 策略（自动编译）
    - Python 策略（直接加载）
    """
    
    def __init__(self):
        self.dsl_compiler = DSLCompiler()
        self.loaded_strategies: Dict = {}
    
    def load(self, strategy_path: str, strategy_type: str = 'auto'):
        """
        加载策略
        
        Args:
            strategy_path: 策略文件路径
            strategy_type: 'dsl', 'python', or 'auto'
        """
        pass
    
    def reload(self, strategy_name: str):
        """热重载策略"""
        pass
```

### Step 4: 修改 Agent Factory

**文件**: `factory/agent_factory.py`

添加 DSL 策略支持：

```python
from .dsl_compiler import DSLCompiler
from .strategy_loader import StrategyLoader

class AgentFactory:
    def __init__(self, config_dir: str = None):
        # ... 现有代码 ...
        
        # 新增：DSL 支持
        self.strategy_loader = StrategyLoader()
        self.dsl_compiler = DSLCompiler()
    
    def create_agent_from_dsl(
        self,
        dsl_file: str,
        agent_id: str,
        name: str = None
    ) -> TradingAgent:
        """
        从 DSL 文件创建 Agent
        
        Args:
            dsl_file: DSL 策略文件
            agent_id: Agent ID
            name: Agent 名称
        
        Returns:
            TradingAgent 实例
        """
        # 1. 编译 DSL
        python_code = self.dsl_compiler.compile_strategy(dsl_file)
        
        # 2. 加载策略
        strategy = self.strategy_loader.load_from_code(python_code)
        
        # 3. 创建 Agent
        agent = TradingAgent(
            agent_id=agent_id,
            name=name or Path(dsl_file).stem,
            strategy=strategy
        )
        
        self.active_agents[agent_id] = agent
        return agent
```

### Step 5: 创建 CLI 工具

**文件**: `scripts/manage_dsl.py`

```python
#!/usr/bin/env python3
"""
DSL 策略管理工具
"""

import argparse
from factory.dsl_compiler import DSLCompiler

def main():
    parser = argparse.ArgumentParser(description='DSL 策略管理')
    
    subparsers = parser.add_subparsers(dest='command')
    
    # 编译命令
    compile_parser = subparsers.add_parser('compile', help='编译 DSL 策略')
    compile_parser.add_argument('dsl_file', help='DSL 文件')
    compile_parser.add_argument('-o', '--output', help='输出目录')
    
    # 列表命令
    list_parser = subparsers.add_parser('list', help='列出所有 DSL 策略')
    
    # 重载命令
    reload_parser = subparsers.add_parser('reload', help='热重载策略')
    reload_parser.add_argument('strategy_name', help='策略名称')
    
    args = parser.parse_args()
    
    # 执行命令
    if args.command == 'compile':
        compiler = DSLCompiler()
        compiler.compile_strategy(args.dsl_file, args.output)
    elif args.command == 'list':
        # 列出策略
        pass
    elif args.command == 'reload':
        # 重载策略
        pass

if __name__ == '__main__':
    main()
```

---

## 📁 目录结构

集成后的目录结构：

```
HKTech-Agent/
├── factory/
│   ├── dsl/                      # DSL 核心模块 ⭐
│   │   ├── compiler.py
│   │   ├── cache.py
│   │   ├── grammar.py
│   │   ├── types.py
│   │   ├── builtins.py
│   │   └── error_reporter.py
│   ├── dsl_compiler.py           # DSL 编译器集成 ⭐
│   ├── strategy_loader.py        # 策略加载器 ⭐
│   └── agent_factory.py          # 修改
├── dsl_strategies/               # DSL 策略目录 ⭐
│   ├── rsi_mean_reversion.dsl
│   ├── bollinger_breakout.dsl
│   ├── macd_trend.dsl
│   └── multi_factor.dsl
├── generated_strategies/         # 生成的 Python 策略 ⭐
│   └── ...
├── scripts/
│   └── manage_dsl.py             # DSL 管理工具 ⭐
└── docs/
    └── DSL_INTEGRATION.md        # 本文档
```

---

## ✅ 验证步骤

### 1. 编译测试

```bash
cd /root/.openclaw/workspace/Lamda-ai/HKTech-Agent

# 编译 DSL 策略
python3 scripts/manage_dsl.py compile dsl_strategies/rsi_mean_reversion.dsl

# 列出策略
python3 scripts/manage_dsl.py list
```

### 2. 加载测试

```python
from factory.agent_factory import AgentFactory

factory = AgentFactory()

# 从 DSL 创建 Agent
agent = factory.create_agent_from_dsl(
    dsl_file='dsl_strategies/rsi_mean_reversion.dsl',
    agent_id='rsi_agent_001',
    name='RSI Mean Reversion'
)

# 验证 Agent
print(f"✅ Agent 创建成功：{agent.name}")
```

### 3. 热重载测试

```python
# 修改 DSL 文件
# ... 编辑 dsl_strategies/rsi_mean_reversion.dsl ...

# 热重载
factory.strategy_loader.reload('rsi_agent_001')
print("✅ 策略热重载成功")
```

---

## 🎯 集成检查清单

- [ ] DSL 模块复制到 HKTech-Agent
- [ ] 创建 factory/dsl_compiler.py
- [ ] 创建 factory/strategy_loader.py
- [ ] 修改 factory/agent_factory.py
- [ ] 创建 dsl_strategies/ 目录
- [ ] 复制 5 个策略模板
- [ ] 创建 scripts/manage_dsl.py
- [ ] 编写集成文档
- [ ] 运行编译测试
- [ ] 运行加载测试
- [ ] 运行热重载测试

---

## 📊 预期效果

### 使用前

```python
# 需要手动编译 DSL
# 需要手动加载 Python 文件
# 无法热重载
```

### 使用后

```python
# 一行代码创建 Agent
agent = factory.create_agent_from_dsl('strategy.dsl', 'agent_001')

# 自动编译、加载、缓存
# 支持热重载
# 完整错误诊断
```

---

**计划状态**: 待执行  
**优先级**: P1  
**预计完成**: 1 小时
