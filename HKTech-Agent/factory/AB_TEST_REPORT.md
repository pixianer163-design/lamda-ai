# Agent A/B测试报告
## 手动创建配置对比

**测试时间**: 2026-02-18  
**测试类型**: 策略参数A/B对比

---

## 📊 测试Agent配置

### Agent A - 基础版 (`abtest_basic_001`)

```yaml
名称: A-基础版
模板: hktech_basic
风险等级: 中等

股票配置:
  - 00700 腾讯控股 (33%)
  - 09988 阿里巴巴 (33%)
  - 03690 美团     (34%)

风控参数:
  - 最大仓位: 40%
  - 止损线: -8%
  - 止盈线: +15%
  - 现金保留: 20%

策略配置:
  - 类型: multi_factor
  - 技术因子: 40%
  - 基本面因子: 30%
  - 情绪因子: 30%

适用场景: 日常投资，平衡收益与风险
```

---

### Agent B - 保守版 (`abtest_conservative_001`)

```yaml
名称: B-保守版
模板: hktech_conservative
风险等级: 低

股票配置:
  - 00700 腾讯控股 (50%)
  - 09988 阿里巴巴 (50%)
  # 只有2只股票

风控参数:
  - 最大仓位: 35% (更保守)
  - 止损线: -5% (更严格)
  - 止盈线: +10% (更严格)
  - 现金保留: 40% (更多现金)

策略配置:
  - 类型: value (价值投资)
  - 技术因子: 20%
  - 基本面因子: 60% (更注重)
  - 情绪因子: 20%

适用场景: 资本保全，稳健收益
```

---

### Agent C - 激进版 (`abtest_aggressive_001`)

```yaml
名称: C-激进版
模板: custom (自定义)
风险等级: 高

股票配置:
  - 00700 腾讯控股 (30%)
  - 09988 阿里巴巴 (30%)
  - 03690 美团     (25%)
  - 01810 小米集团 (15%) # 4只股票

风控参数:
  - 最大仓位: 50% (更激进)
  - 止损线: -12% (更宽松)
  - 止盈线: +25% (更激进)
  - 现金保留: 10% (更少现金)

策略配置:
  - 类型: momentum (动量策略)
  - 技术因子: 60% (更注重)
  - 基本面因子: 20%
  - 情绪因子: 20%

LLM模型: deepseek-reasoner (更强的推理能力)
调度: 09:00, 13:00, 16:30 (更多检查点)

适用场景: 高风险承受能力，追求高收益
```

---

## 📈 配置对比表

| 参数 | Agent A (基础) | Agent B (保守) | Agent C (激进) |
|------|---------------|---------------|---------------|
| **股票数量** | 3只 | 2只 | 4只 |
| **最大仓位** | 40% | 35% | 50% |
| **止损线** | -8% | -5% | -12% |
| **止盈线** | +15% | +10% | +25% |
| **现金保留** | 20% | 40% | 10% |
| **策略类型** | multi_factor | value | momentum |
| **技术因子** | 40% | 20% | 60% |
| **基本面因子** | 30% | 60% | 20% |
| **情绪因子** | 30% | 20% | 20% |
| **LLM模型** | chat | chat | reasoner |
| **调度频率** | 1次/天 | 1次/天 | 3次/天 |

---

## 🎯 预期表现分析

### Agent A (基础版)
- **优势**: 平衡配置，适应多种市场环境
- **劣势**: 没有明显优势，中规中矩
- **适合**: 大多数投资者，不想冒太大风险
- **预期收益**: 年化 8-15%
- **预期回撤**: 最大 15-20%

### Agent B (保守版)
- **优势**: 严格风控，回撤较小，现金流充足
- **劣势**: 上涨时仓位不足，可能错失机会
- **适合**: 风险厌恶型投资者，退休资金
- **预期收益**: 年化 5-10%
- **预期回撤**: 最大 8-12%

### Agent C (激进版)
- **优势**: 高仓位抓住上涨机会，技术敏感
- **劣势**: 回撤大，波动剧烈
- **适合**: 风险承受能力强，追求高收益
- **预期收益**: 年化 15-30% (也可能亏损)
- **预期回撤**: 最大 25-35%

---

## 🚀 使用方式

### 1. 加载Agent

```python
from factory.agent_factory import AgentFactory

factory = AgentFactory()

# 加载A/B测试Agent
agent_a = factory.create_from_config("abtest_basic_001")
agent_b = factory.create_from_config("abtest_conservative_001")
agent_c = factory.create_from_config("abtest_aggressive_001")
```

### 2. 并行运行

```python
import asyncio

async def run_all():
    await asyncio.gather(
        agent_a.run_cycle(),
        agent_b.run_cycle(),
        agent_c.run_cycle()
    )

asyncio.run(run_all())
```

### 3. 对比结果

```python
# 获取统计
stats_a = agent_a.get_stats()
stats_b = agent_b.get_stats()
stats_c = agent_c.get_stats()

# 对比组合价值
print(f"Agent A: ¥{stats_a['portfolio_value']}")
print(f"Agent B: ¥{stats_b['portfolio_value']}")
print(f"Agent C: ¥{stats_c['portfolio_value']}")
```

---

## 📁 配置文件位置

```
/opt/hktech-agent/factory/configs/
├── abtest_basic_001.yaml       # Agent A
├── abtest_conservative_001.yaml # Agent B
└── abtest_aggressive_001.yaml   # Agent C
```

---

## 💡 建议

### 短期（1-2周）
1. 同时运行3个Agent
2. 观察每日决策差异
3. 记录各Agent的买卖点

### 中期（1-3个月）
1. 对比累计收益率
2. 分析最大回撤
3. 评估夏普比率

### 长期（3-6个月）
1. 选择表现最好的策略
2. 根据市场环境动态调整
3. 考虑策略组合（多Agent并行）

---

## ⚠️ 风险提示

1. **历史表现不代表未来**: A/B测试结果仅供参考
2. **市场环境变化**: 不同策略在不同市场周期表现差异大
3. **参数敏感性**: 小幅参数调整可能导致结果大幅变化
4. **建议小额测试**: 实际投资前充分测试

---

**测试状态**: ✅ 配置已创建，可以开始测试
