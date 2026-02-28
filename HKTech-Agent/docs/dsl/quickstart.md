# DSL 快速入门指南

**版本**: v1.0  
**最后更新**: 2026-02-28  
**预计时间**: 15 分钟

---

## 🎯 学习目标

完成本教程后，你将能够：
- ✅ 理解 DSL 的基本语法
- ✅ 编写简单的交易策略
- ✅ 编译和运行策略
- ✅ 调试常见问题

---

## 📦 第一步：环境准备

### 检查依赖

```bash
cd /root/.openclaw/workspace/dsl_meta_learning
python3 check_dependencies.py
```

### 安装依赖（如有缺失）

```bash
pip install textx --break-system-packages
```

### 验证安装

```bash
python3 -c "from dsl.compiler import DSLParser; print('✅ DSL 环境就绪')"
```

---

## 📝 第二步：第一个策略

### 创建文件

```bash
mkdir -p my_strategies
cat > my_strategies/hello_world.dsl << 'EOF'
/*
 * 我的第一个 DSL 策略
 * 简单的 RSI 超卖买入策略
 */

strategy HelloRSI
    description: "RSI 超卖买入策略"
{
    // 参数定义
    param rsi_period: period = 14
    param rsi_oversold: int = 30
    
    // 指标定义
    indicator rsi: RSI(period=rsi_period)
    
    // 入场条件
    when "RSI Oversold" {
        trigger: rsi < rsi_oversold
        action: BUY
        size: 0.5
        confidence: 0.8
    }
    
    // 仓位管理
    position {
        max_position = 0.5
        risk_per_trade = 2%
    }
}
EOF
```

### 编译策略

```bash
python3 << 'EOF'
from dsl.compiler import DSLParser

# 创建解析器
parser = DSLParser()

# 读取 DSL 文件
with open('my_strategies/hello_world.dsl') as f:
    dsl_code = f.read()

# 编译为 Python 代码
python_code = parser.compile(dsl_code)

# 保存生成的代码
with open('my_strategies/hello_world_strategy.py', 'w') as f:
    f.write(python_code)

print("✅ 策略编译成功！")
print(f"   生成文件：my_strategies/hello_world_strategy.py")
print(f"   代码行数：{len(python_code.splitlines())} 行")
EOF
```

### 查看生成的代码

```bash
head -30 my_strategies/hello_world_strategy.py
```

---

## 🚀 第三步：使用策略

### 导入策略

```python
from my_strategies.hello_world_strategy import HellorsiStrategy
import pandas as pd
import numpy as np

# 创建策略实例
strategy = HellorsiStrategy()

# 准备测试数据
dates = pd.date_range('2024-01-01', periods=100, freq='D')
np.random.seed(42)
close_prices = 100 + np.cumsum(np.random.randn(100) * 2)

data = pd.DataFrame({
    'open': close_prices * (1 + np.random.randn(100) * 0.01),
    'high': close_prices * (1 + np.abs(np.random.randn(100) * 0.02)),
    'low': close_prices * (1 - np.abs(np.random.randn(100) * 0.02)),
    'close': close_prices,
    'volume': np.random.randint(1000000, 10000000, 100)
}, index=dates)

# 生成交易信号
signal = strategy.generate_signal(data)

if signal:
    print(f"📊 交易信号：{signal.action}")
    print(f"   置信度：{signal.confidence}")
    print(f"   仓位：{signal.position_size}")
else:
    print("⏸️  无交易信号 (HOLD)")
```

---

## 🎨 第四步：修改和优化

### 修改参数

编辑 `my_strategies/hello_world.dsl`:

```textx
// 修改 RSI 周期
param rsi_period: period = 21  // 从 14 改为 21

// 修改超卖阈值
param rsi_oversold: int = 25  // 从 30 改为 25
```

### 添加新指标

```textx
// 添加移动平均线
indicator sma: SMA(period=20)

// 在条件中使用
when "RSI + MA" {
    trigger: rsi < rsi_oversold and close > sma
    action: BUY
    confidence: 0.85  // 提高置信度
}
```

### 添加退出规则

```textx
// 添加止损退出
exit "Stop Loss" {
    close < entry_price * (1 - 0.05)
    type = STOP_LOSS
}

// 添加止盈退出
exit "Take Profit" {
    close > entry_price * (1 + 0.10)
    type = TAKE_PROFIT
}
```

---

## 📚 第五步：学习更多

### 参考示例策略

```bash
# 查看示例策略
ls examples/strategies/

# 学习 RSI 策略
cat examples/strategies/rsi_mean_reversion.dsl

# 学习 MACD 策略
cat examples/strategies/macd_trend.dsl

# 学习多因子策略
cat examples/strategies/multi_factor.dsl
```

### 阅读完整文档

- [语法参考手册](syntax_reference.md) - 完整语法说明
- [策略模板库](../examples/strategies/README.md) - 5 个经典策略
- [错误处理指南](error_handling.md) - 调试技巧

---

## 🔧 常见问题

### Q1: 编译失败怎么办？

**错误示例**:
```
Error: Line 1:1 - 语法错误：Expected 'param' or 'indicator'
```

**解决方法**:
1. 检查语法是否正确（参考语法手册）
2. 检查是否缺少必需字段（如 `position` 块）
3. 查看错误位置的代码片段

### Q2: 如何调试策略？

**方法 1**: 打印生成的 Python 代码
```python
python_code = parser.compile(dsl_code)
print(python_code)  # 查看生成的代码
```

**方法 2**: 使用 CLI 工具
```bash
python3 scripts/dsl_compile.py my_strategy.dsl --stats
```

### Q3: 如何优化策略性能？

1. **调整参数**: 修改指标周期、阈值等
2. **添加过滤条件**: 增加成交量、波动率等过滤
3. **优化仓位**: 调整 max_position 和 risk_per_trade
4. **回测验证**: 使用历史数据验证策略效果

---

## 🎓 进阶学习路径

### 初级（1-2 天）
- ✅ 完成本教程
- ✅ 理解基本语法
- ✅ 编写简单策略

### 中级（3-5 天）
- 📖 学习 5 个示例策略
- 📖 掌握复杂条件表达式
- 📖 理解仓位管理

### 高级（1-2 周）
- 📖 多因子组合策略
- 📖 自定义指标
- 📖 策略优化和回测

---

## 📝 练习任务

### 任务 1: 修改 RSI 策略

将 RSI 超卖策略改为超买策略：
- RSI > 70 时卖出
- 添加 RSI < 30 时平仓

### 任务 2: 创建双均线策略

编写一个双均线交叉策略：
- 快线（10 日）金叉慢线（30 日）时买入
- 快线死叉慢线时卖出
- 添加 3% 追踪止损

### 任务 3: 组合策略

创建一个组合策略：
- 使用 RSI + MACD 两个指标
- 只有两个指标都发出买入信号时才买入
- 添加完整的退出规则

---

## 🎉 恭喜完成！

你已经完成了 DSL 快速入门！

**下一步**:
1. 📖 阅读 [语法参考手册](syntax_reference.md)
2. 💻 尝试修改示例策略
3. 🚀 创建你自己的策略

**遇到问题？**
- 查看 [常见问题](#常见问题)
- 参考 [示例策略](../examples/strategies/)
- 阅读 [错误处理指南](error_handling.md)

---

**教程版本**: v1.0  
**作者**: Alex  
**最后更新**: 2026-02-28
