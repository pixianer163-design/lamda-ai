# DSL 语法参考手册

**版本**: v1.0  
**最后更新**: 2026-02-28  
**状态**: ✅ 完整

---

## 📖 目录

1. [概述](#概述)
2. [策略结构](#策略结构)
3. [参数定义](#参数定义)
4. [指标系统](#指标系统)
5. [条件表达式](#条件表达式)
6. [仓位管理](#仓位管理)
7. [退出规则](#退出规则)
8. [内置函数](#内置函数)
9. [完整示例](#完整示例)

---

## 概述

DSL（Domain Specific Language）是专为量化交易策略设计的领域特定语言。

**特点**:
- ✅ 简洁易读的语法
- ✅ 丰富的技术指标库
- ✅ 灵活的条件表达式
- ✅ 完整的风险控制
- ✅ 自动编译为 Python

**文件扩展名**: `.dsl`

---

## 策略结构

### 基本语法

```textx
strategy StrategyName
    description: "策略描述"
{
    // 参数定义
    param ...
    
    // 指标定义
    indicator ...
    
    // 入场条件
    when "条件名称" {
        trigger: ...
        action: BUY/SELL
    }
    
    // 仓位管理
    position {
        ...
    }
    
    // 退出规则
    exit "规则名称" {
        ...
    }
}
```

### 示例

```textx
strategy MyFirstStrategy
    description: "我的第一个交易策略"
{
    param period: period = 14
    
    indicator rsi: RSI(period=period)
    
    when "Entry" {
        trigger: rsi < 30
        action: BUY
    }
    
    position {
        max_position = 0.5
        risk_per_trade = 2%
    }
}
```

---

## 参数定义

### 语法

```textx
param 参数名：类型 = 默认值
```

### 支持的类型

| 类型 | 说明 | 示例 |
|------|------|------|
| `period` | 周期参数 | `param period: period = 14` |
| `int` | 整数 | `param threshold: int = 30` |
| `float` | 浮点数 | `param ratio: float = 2.0` |
| `bool` | 布尔值 | `param enabled: bool = true` |
| `string` | 字符串 | `param name: string = "test"` |

### 参数引用

参数可以在指标定义和条件中使用：

```textx
param fast_period: period = 10
param slow_period: period = 30

indicator fast_ma: SMA(period=fast_period)
indicator slow_ma: SMA(period=slow_period)

when "Golden Cross" {
    trigger: crossover(fast_ma, slow_ma)
    action: BUY
}
```

---

## 指标系统

### 支持的指标

| 指标 | 类型 | 参数 | 示例 |
|------|------|------|------|
| **SMA** | 简单移动平均 | `period` | `indicator sma: SMA(period=20)` |
| **EMA** | 指数移动平均 | `period` | `indicator ema: EMA(period=20)` |
| **RSI** | 相对强弱指标 | `period` | `indicator rsi: RSI(period=14)` |
| **MACD** | 平滑异同移动平均 | `fast, slow, signal` | `indicator macd: MACD(fast=12, slow=26, signal=9)` |
| **BB** | 布林带 | `period, std` | `indicator bb: BB(period=20, std=2)` |
| **ATR** | 平均真实波幅 | `period` | `indicator atr: ATR(period=14)` |
| **KDJ** | 随机指标 | `k_period, d_period` | `indicator kdj: KDJ(k_period=9, d_period=3)` |
| **VWAP** | 成交量加权平均价 | 无 | `indicator vwap: VWAP()` |
| **ADX** | 平均趋向指标 | `period` | `indicator adx: ADX(period=14)` |
| **CCI** | 商品通道指标 | `period` | `indicator cci: CCI(period=20)` |

### 多字段指标

某些指标（如 MACD、BB）返回多个字段：

```textx
// MACD 有三个字段：macd, signal, histogram
indicator macd_line: MACD(fast=12, slow=26, signal=9)
indicator signal_line: MACD(fast=12, slow=26, signal=9)
indicator histogram: MACD(fast=12, slow=26, signal=9)

when "MACD Cross" {
    trigger: crossover(macd_line, signal_line)
    action: BUY
}

// BB 有三个字段：upper, middle, lower
indicator bb_upper: BB(period=20, std=2)
indicator bb_middle: BB(period=20, std=2)
indicator bb_lower: BB(period=20, std=2)

when "BB Breakout" {
    trigger: close > bb_upper
    action: BUY
}
```

### 指标引用

```textx
// 直接使用指标值
trigger: rsi < 30

// 使用字段（多字段指标）
trigger: macd_line > signal_line

// 与价格比较
trigger: close > bb_upper
trigger: high > atr * 2
```

---

## 条件表达式

### 基本语法

```textx
when "条件名称" {
    trigger: 表达式
    action: BUY/SELL/HOLD/CLOSE
    size: 仓位比例
    confidence: 置信度
    priority: 优先级
}
```

### 比较运算符

| 运算符 | 说明 | 示例 |
|--------|------|------|
| `>` | 大于 | `rsi > 70` |
| `<` | 小于 | `rsi < 30` |
| `>=` | 大于等于 | `close >= open` |
| `<=` | 小于等于 | `close <= open` |
| `==` | 等于 | `action == BUY` |
| `!=` | 不等于 | `position != 0` |

### 逻辑运算符

```textx
// AND 逻辑
trigger: rsi < 30 and crossover(fast_ma, slow_ma)

// OR 逻辑
trigger: crossunder(fast_ma, slow_ma) or rsi > 70

// 组合逻辑
trigger: (rsi < 30 and volume > avg_volume) or price < support
```

### 内置函数

| 函数 | 说明 | 示例 |
|------|------|------|
| `crossover(a, b)` | a 从下方穿越 b（金叉） | `crossover(fast_ma, slow_ma)` |
| `crossunder(a, b)` | a 从上方穿越 b（死叉） | `crossunder(fast_ma, slow_ma)` |
| `above(value, threshold)` | value 在 threshold 上方 | `above(rsi, 50)` |
| `below(value, threshold)` | value 在 threshold 下方 | `below(rsi, 50)` |
| `highest(series, period)` | 最近 N 周期最高值 | `highest(high, 20)` |
| `lowest(series, period)` | 最近 N 周期最低值 | `lowest(low, 20)` |
| `change_pct(series, period)` | N 周期变化率 | `change_pct(close, 10)` |

### 价格变量

```textx
close   // 收盘价
open    // 开盘价
high    // 最高价
low     // 最低价
volume  // 成交量
```

---

## 仓位管理

### 基本语法

```textx
position {
    max_position = 0.6          // 最大仓位 60%
    risk_per_trade = 2%         // 单笔风险 2%
    trailing_stop = 3%          // 追踪止损 3%
    take_profit ratio = 2.0     // 盈亏比 2:1
}
```

### 参数说明

| 参数 | 说明 | 示例 |
|------|------|------|
| `max_position` | 最大仓位比例 | `max_position = 0.8` (80%) |
| `risk_per_trade` | 单笔风险比例 | `risk_per_trade = 2%` |
| `trailing_stop` | 追踪止损比例 | `trailing_stop = 3%` |
| `take_profit` | 止盈参数 | `take_profit ratio = 2.0` |

### 示例

```textx
position {
    max_position = 0.5           // 最大仓位 50%
    risk_per_trade = 1.5%        // 单笔风险 1.5%
    trailing_stop = 5%           // 追踪止损 5%
}
```

---

## 退出规则

### 基本语法

```textx
exit "规则名称" {
    condition: 退出条件
    type: STOP_LOSS/TAKE_PROFIT/TIMEOUT/SIGNAL
}
```

### 退出类型

| 类型 | 说明 | 示例 |
|------|------|------|
| `STOP_LOSS` | 止损退出 | `type = STOP_LOSS` |
| `TAKE_PROFIT` | 止盈退出 | `type = TAKE_PROFIT` |
| `TIMEOUT` | 超时退出 | `type = TIMEOUT` |
| `SIGNAL` | 信号退出 | `type = SIGNAL` |

### 示例

```textx
// 固定止损
exit "Fixed Stop Loss" {
    close < entry_price * (1 - 0.05)
    type = STOP_LOSS
}

// 追踪止损
exit "Trailing Stop" {
    close < entry_price * (1 - trailing_stop / 100)
    type = STOP_LOSS
}

// 信号退出
exit "RSI Neutral" {
    rsi > 50
    type = SIGNAL
}
```

---

## 完整示例

### 示例 1: RSI 超买超卖策略

```textx
/*
 * RSI 超买超卖策略
 * 原理：RSI<30 超卖买入，RSI>70 超买卖出
 */

strategy RSIMeanReversion
    description: "RSI 超买超卖均值回归策略"
{
    // 策略参数
    param rsi_period: period = 14
    param rsi_oversold: int = 30
    param rsi_overbought: int = 70
    
    // 技术指标
    indicator rsi: RSI(period=rsi_period)
    
    // 入场条件：RSI 进入超卖区域
    when "RSI Oversold Entry" {
        trigger: rsi < rsi_oversold
        action: BUY
        size: 0.5
        confidence: 0.8
        priority: 1
    }
    
    // 出场条件：RSI 进入超买区域
    when "RSI Overbought Exit" {
        trigger: rsi > rsi_overbought
        action: SELL
        size: 0.5
        confidence: 0.8
        priority: 1
    }
    
    // 仓位管理
    position {
        max_position = 0.6
        risk_per_trade = 2%
        trailing_stop = 5%
    }
    
    // 退出规则：RSI 回归中性
    exit "RSI Neutral" {
        rsi > 50
        type = SIGNAL
    }
}
```

### 示例 2: 双均线趋势策略

```textx
/*
 * 双均线趋势策略
 * 原理：快线金叉慢线买入，死叉卖出
 */

strategy TrendFollowing
    description: "双均线趋势跟踪策略"
{
    // 策略参数
    param fast_period: period = 10
    param slow_period: period = 30
    
    // 技术指标
    indicator fast_ma: SMA(period=fast_period)
    indicator slow_ma: SMA(period=slow_period)
    
    // 入场条件：金叉
    when "Golden Cross" {
        trigger: crossover(fast_ma, slow_ma)
        action: BUY
        size: 0.6
        confidence: 0.75
        priority: 1
    }
    
    // 出场条件：死叉
    when "Death Cross" {
        trigger: crossunder(fast_ma, slow_ma)
        action: SELL
        size: 0.6
        confidence: 0.75
        priority: 1
    }
    
    // 仓位管理
    position {
        max_position = 0.7
        risk_per_trade = 2%
        trailing_stop = 3%
    }
}
```

### 示例 3: 多因子组合策略

```textx
/*
 * 多因子组合策略
 * 原理：趋势 + 动量 + 波动三因子共振
 */

strategy MultiFactor
    description: "多因子组合策略"
{
    // 趋势因子参数
    param fast_ma_period: period = 10
    param slow_ma_period: period = 30
    
    // 动量因子参数
    param rsi_period: period = 14
    param rsi_neutral: int = 50
    
    // 波动因子参数
    param atr_period: period = 14
    
    // 技术指标
    indicator fast_ma: SMA(period=fast_ma_period)
    indicator slow_ma: SMA(period=slow_ma_period)
    indicator rsi: RSI(period=rsi_period)
    indicator atr: ATR(period=atr_period)
    
    // 入场条件：三因子共振
    when "Multi-Factor Entry" {
        // 趋势：金叉
        // 动量：RSI > 50
        trigger: crossover(fast_ma, slow_ma) and rsi > rsi_neutral
        action: BUY
        size: 0.6
        confidence: 0.85
        priority: 1
    }
    
    // 出场条件：任一反转
    when "Multi-Factor Exit" {
        trigger: crossunder(fast_ma, slow_ma) or rsi < rsi_neutral
        action: SELL
        size: 0.6
        confidence: 0.85
        priority: 1
    }
    
    // 仓位管理
    position {
        max_position = 0.6
        risk_per_trade = 2%
        trailing_stop = 3%
    }
}
```

---

## 📝 最佳实践

### 1. 参数命名

```textx
// ✅ 好的命名
param rsi_period: period = 14
param fast_ma_period: period = 10

// ❌ 避免的命名
param p1: period = 14  // 不明确
param x: int = 30      // 无意义
```

### 2. 注释规范

```textx
// 单行注释

/*
 * 多行注释
 * 用于说明复杂逻辑
 */

// ✅ 好的注释
param rsi_period: period = 14  // RSI 计算周期

// ❌ 冗余注释
param period: period = 14  // 周期为 14
```

### 3. 条件组织

```textx
// ✅ 清晰的命名
when "RSI Oversold Entry" { ... }
when "MACD Golden Cross" { ... }

// ❌ 模糊的命名
when "Condition 1" { ... }
when "Entry" { ... }
```

### 4. 风险控制

```textx
// ✅ 必须包含
position {
    max_position = 0.6       // 最大仓位
    risk_per_trade = 2%      // 单笔风险
    trailing_stop = 3%       // 追踪止损
}

// ❌ 缺少风控
position {
    max_position = 1.0       // 全仓风险大
}
```

---

## 🔧 常见问题

### Q1: 如何引用指标字段？

```textx
// MACD 返回多个字段
indicator macd: MACD(fast=12, slow=26, signal=9)

// 分别引用
trigger: macd.macd > macd.signal  // ❌ 不支持
trigger: macd_line > signal_line  // ✅ 正确做法

// 需要分别定义
indicator macd_line: MACD(...)
indicator signal_line: MACD(...)
indicator histogram: MACD(...)
```

### Q2: 如何使用百分比？

```textx
// 在仓位管理中直接使用 %
position {
    risk_per_trade = 2%      // ✅ 正确
    max_position = 0.6       // ✅ 小数形式
}

// 在条件中使用
trigger: close > entry_price * 1.05  // ✅ 上涨 5%
```

### Q3: 如何组合多个条件？

```textx
// AND 逻辑
trigger: rsi < 30 and crossover(fast_ma, slow_ma)

// OR 逻辑
trigger: crossunder(fast_ma, slow_ma) or rsi > 70

// 复杂组合
trigger: (rsi < 30 and volume > avg_volume) or price < support
```

---

## 📚 参考资源

- [策略模板库](../examples/strategies/)
- [快速入门](quickstart.md)
- [错误处理指南](error_handling.md)

---

**文档版本**: v1.0  
**维护者**: Alex  
**最后更新**: 2026-02-28
