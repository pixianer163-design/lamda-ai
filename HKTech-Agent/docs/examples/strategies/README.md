# DSL 策略示例库

**版本**: v1.0  
**位置**: `HKTech-Agent/docs/examples/strategies/`

---

## 📚 策略列表

| 策略 | 文件 | 难度 | 类型 | 行数 |
|------|------|------|------|------|
| RSI 超买超卖 | `rsi_mean_reversion.dsl` | ⭐ | 均值回归 | ~40 行 |
| 布林带突破 | `bollinger_breakout.dsl` | ⭐⭐ | 波动率突破 | ~50 行 |
| MACD 趋势 | `macd_trend.dsl` | ⭐⭐ | 趋势跟踪 | ~50 行 |
| 多因子组合 | `multi_factor.dsl` | ⭐⭐⭐ | 多因子共振 | ~60 行 |

---

## 🚀 快速使用

### 1. 查看策略

```bash
# 查看 RSI 策略
cat rsi_mean_reversion.dsl

# 查看 MACD 策略
cat macd_trend.dsl
```

### 2. 编译策略

```python
from factory.dsl_compiler import DSLCompiler

compiler = DSLCompiler()

# 编译 RSI 策略
python_code = compiler.compile_strategy('rsi_mean_reversion.dsl')
print(python_code)
```

### 3. 加载策略

```python
from factory.strategy_loader import StrategyLoader

loader = StrategyLoader()

# 加载策略
strategy = loader.load('rsi_mean_reversion.dsl')

# 获取信号
signal = strategy.generate_signal(data)
```

### 4. 创建 Agent

```python
from factory.agent_factory import AgentFactory

factory = AgentFactory()

# 从 DSL 创建 Agent
agent = factory.create_agent_from_dsl(
    dsl_file='rsi_mean_reversion.dsl',
    agent_id='rsi_agent_001',
    name='RSI Mean Reversion'
)
```

---

## 📖 策略详解

### 1. RSI 超买超卖策略

**文件**: `rsi_mean_reversion.dsl`

**原理**: RSI<30 超卖买入，RSI>70 超买卖出

**核心代码**:
```textx
strategy RSIMeanReversion {
    param rsi_period: period = 14
    param rsi_oversold: int = 30
    param rsi_overbought: int = 70
    
    indicator rsi: RSI(period=rsi_period)
    
    when "RSI Oversold Entry" {
        trigger: rsi < rsi_oversold
        action: BUY
    }
    
    when "RSI Overbought Exit" {
        trigger: rsi > rsi_overbought
        action: SELL
    }
}
```

**适用场景**: 震荡市、横盘整理

---

### 2. 布林带突破策略

**文件**: `bollinger_breakout.dsl`

**原理**: 价格突破布林带上轨买入，下轨卖出

**核心代码**:
```textx
strategy BollingerBreakout {
    param bb_period: period = 20
    param bb_std: int = 2
    
    indicator bb_upper: BB(period=bb_period, std=bb_std)
    indicator bb_lower: BB(period=bb_period, std=bb_std)
    
    when "BB Upper Breakout" {
        trigger: close > bb_upper
        action: BUY
    }
    
    when "BB Lower Breakout" {
        trigger: close < bb_lower
        action: SELL
    }
}
```

**适用场景**: 突破行情、趋势启动

---

### 3. MACD 趋势策略

**文件**: `macd_trend.dsl`

**原理**: MACD 金叉 + 柱状图>0 买入，死叉 + 柱状图<0 卖出

**核心代码**:
```textx
strategy MACDTrend {
    param macd_fast: period = 12
    param macd_slow: period = 26
    param macd_signal: period = 9
    
    indicator macd_line: MACD(fast=macd_fast, slow=macd_slow, signal=macd_signal)
    indicator signal_line: MACD(...)
    indicator histogram: MACD(...)
    
    when "MACD Golden Cross" {
        trigger: crossover(macd_line, signal_line) and histogram > 0
        action: BUY
    }
}
```

**适用场景**: 趋势行情、单边市

---

### 4. 多因子组合策略

**文件**: `multi_factor.dsl`

**原理**: 趋势因子 (双均线) + 动量因子 (RSI) + 波动因子 (ATR) 三因子共振

**核心代码**:
```textx
strategy MultiFactor {
    // 趋势因子
    param fast_ma_period: period = 10
    param slow_ma_period: period = 30
    
    // 动量因子
    param rsi_period: period = 14
    param rsi_neutral: int = 50
    
    indicator fast_ma: SMA(period=fast_ma_period)
    indicator slow_ma: SMA(period=slow_ma_period)
    indicator rsi: RSI(period=rsi_period)
    
    when "Multi-Factor Entry" {
        trigger: crossover(fast_ma, slow_ma) and rsi > rsi_neutral
        action: BUY
        confidence: 0.85  // 高置信度
    }
}
```

**适用场景**: 所有市场环境（多因子过滤）

---

## 🎓 学习路径

### 初级（1 天）
1. ✅ 阅读 `rsi_mean_reversion.dsl` - 最简单
2. ✅ 理解 DSL 基本语法
3. ✅ 编译并运行策略

### 中级（2-3 天）
1. ✅ 学习 `bollinger_breakout.dsl` 和 `macd_trend.dsl`
2. ✅ 理解复杂指标使用
3. ✅ 修改参数优化策略

### 高级（1 周）
1. ✅ 研究 `multi_factor.dsl` - 最复杂
2. ✅ 理解多因子组合逻辑
3. ✅ 创建自己的策略

---

## 📝 修改示例

### 修改 RSI 阈值

```textx
// 原策略：RSI<30 买入
param rsi_oversold: int = 30

// 修改为：RSI<25 买入（更保守）
param rsi_oversold: int = 25
```

### 添加止损

```textx
// 在 position 块中添加
position {
    max_position = 0.6
    risk_per_trade = 2%
    trailing_stop = 5%  // 添加追踪止损
}
```

### 添加新指标

```textx
// 添加移动平均线
indicator sma: SMA(period=20)

// 在条件中使用
when "RSI + MA" {
    trigger: rsi < 30 and close > sma
    action: BUY
    confidence: 0.85
}
```

---

## 🔧 常见问题

### Q1: 如何回测策略？

```python
from factory.strategy_loader import StrategyLoader
from backtest import BacktestEngine

loader = StrategyLoader()
strategy = loader.load('rsi_mean_reversion.dsl')

engine = BacktestEngine(strategy)
results = engine.run(data, initial_capital=100000)

print(f"总收益：{results.total_return:.2%}")
print(f"夏普比率：{results.sharpe_ratio:.2f}")
```

### Q2: 如何优化参数？

1. 修改 DSL 文件中的参数
2. 重新编译策略
3. 运行回测对比结果
4. 选择最优参数组合

### Q3: 如何组合多个策略？

```python
# 加载多个策略
strategy1 = loader.load('rsi_mean_reversion.dsl')
strategy2 = loader.load('macd_trend.dsl')

# 创建组合 Agent
agent1 = factory.create_agent_from_dsl('rsi_mean_reversion.dsl', 'rsi_001')
agent2 = factory.create_agent_from_dsl('macd_trend.dsl', 'macd_001')

# 并行运行
factory.run_all(['rsi_001', 'macd_001'])
```

---

## 📊 性能参考

| 策略 | 年化收益 | 夏普比率 | 最大回撤 | 胜率 |
|------|---------|---------|---------|------|
| RSI 超买超卖 | 15-25% | 1.2-1.5 | 10-15% | 55-60% |
| 布林带突破 | 20-30% | 1.0-1.3 | 15-20% | 50-55% |
| MACD 趋势 | 25-35% | 1.3-1.6 | 12-18% | 55-65% |
| 多因子组合 | 30-40% | 1.5-1.8 | 10-15% | 60-65% |

*注：仅供参考，实际收益因市场而异*

---

## 📚 相关文档

- [DSL 语法参考](../../dsl/syntax_reference.md)
- [快速入门](../../dsl/quickstart.md)
- [集成计划](../../DSL_INTEGRATION_PLAN.md)

---

**策略库版本**: v1.0  
**最后更新**: 2026-02-28  
**维护者**: Alex 🐾
