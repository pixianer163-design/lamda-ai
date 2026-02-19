# HKTech-Agent 完善设计文档

**日期**：2026-02-19
**状态**：已审批，待实施
**方案**：C — 并行双轨

---

## 背景与现状

HKTech-Agent 架构完整，但 AI/ML 核心全是 mock：
- `llm_signal_extractor.py`：纯关键词匹配，注释写明"真实部署时需替换为实际LLM API调用"
- `llm_decision_enhancer.py`：决策合成用 `random.uniform()`，无真实合并逻辑
- `rssm_world_model.py`：没有训练好的模型，预测是启发式规则
- `world_model_integration.py`：`predict_future()` 函数实现截断，不完整

---

## 目标

1. 集成 DeepSeek API 替换全部 mock LLM 调用
2. 训练真实 GRU 世界模型（2018–今，6 只恒科股票）
3. 统一配置管理，消灭硬编码路径
4. 完善错误处理与数据缓存，确保 API 失败时不崩溃
5. 补全 VectorBT 回测逻辑
6. 补充测试覆盖率 ≥ 70%

---

## 方案：并行双轨

### 轨道 1 — 基础设施

涉及文件：`shared/config.py`（新建）、`.env.example`（新建）、`active_src/data_collector.py`、`active_src/vectorbt_integration.py`、`prod/src/llm_enhanced_agent.py`（错误处理）、`tests/`（新增）

涉及文件不与轨道 2 重叠，可并行推进。

### 轨道 2 — AI 核心

涉及文件：`prod/src/llm_signal_extractor.py`、`prod/src/llm_decision_enhancer.py`、`prod/src/rssm_world_model.py`、`prod/src/world_model_integration.py`、`prod/src/train_world_model.py`

---

## 详细设计

### 轨道 2：AI 核心

#### 2.1 DeepSeek LLM 集成

**原则**：就地修改现有文件，不新建独立 Client 类。

**`llm_signal_extractor.py`**：

修改 `_call_llm_api()` 方法，直接调用 DeepSeek：

```python
def _call_llm_api(self, stock_code: str, news_items: list) -> dict:
    import requests, os, json
    api_key = os.environ.get("DEEPSEEK_API_KEY")
    if not api_key:
        return self._keyword_fallback(stock_code, news_items)

    news_text = "\n".join([f"- {n}" for n in news_items[:5]])
    prompt = f"""你是一位专业的港股分析师。请分析以下关于{stock_code}的新闻，返回JSON格式的情感分析结果。

新闻内容：
{news_text}

要求返回格式（只返回JSON，不要其他文字）：
{{"sentiment": 0到1之间的浮点数, "key_factors": ["因素1", "因素2"], "confidence": 0到1之间的浮点数}}

其中sentiment含义：0=极度悲观, 0.5=中性, 1=极度乐观"""

    try:
        resp = requests.post(
            "https://api.deepseek.com/v1/chat/completions",
            headers={"Authorization": f"Bearer {api_key}", "Content-Type": "application/json"},
            json={"model": "deepseek-chat", "messages": [{"role": "user", "content": prompt}],
                  "temperature": 0.1, "max_tokens": 200},
            timeout=30
        )
        result = json.loads(resp.json()["choices"][0]["message"]["content"])
        return result
    except Exception as e:
        logger.warning(f"DeepSeek API 调用失败: {e}，fallback 到关键词匹配")
        return self._keyword_fallback(stock_code, news_items)
```

**`llm_decision_enhancer.py`**：

修改 `_simulate_llm_analysis()` → 真实 DeepSeek 调用，移除 `random.uniform()`：

```python
def _call_deepseek_decision(self, stock_code: str, technical_signal: dict,
                             predicted_return: float, sentiment: float) -> dict:
    prompt = f"""你是一位专业的港股量化交易决策助手。综合以下信息给出交易建议：

股票代码：{stock_code}
技术信号：RSI={technical_signal.get('rsi', 50):.1f}, 趋势={technical_signal.get('trend', '中性')}
世界模型预测5日收益：{predicted_return:.2%}
市场情感得分：{sentiment:.2f}（0=极度悲观，1=极度乐观）

请返回JSON（只返回JSON，不要其他文字）：
{{"action":"BUY/SELL/HOLD","confidence":0到1之间的浮点数,"reasoning":"50字以内的中文理由","risk_level":"LOW/MEDIUM/HIGH"}}"""
    # HTTP 调用 DeepSeek，失败 fallback 到规则决策
```

最终决策合并权重：`final_score = tech×0.4 + world_model×0.3 + llm_sentiment×0.3`

#### 2.2 RSSM 训练 — 简化 GRU 架构

**模型结构**（替换现有 RSSM 复杂实现）：
```
输入: [batch, seq_len=20, features=8]
  features: open, high, low, close, volume, ma5_ratio, rsi, volume_ratio
GRU(input_size=8, hidden_size=64, num_layers=2, dropout=0.2)
Linear(64 → 1)  # 预测第5天收益率
```

**训练数据**：
- 股票：00700、09988、03690、01810、09618、09999（6只恒科股票）
- 时间：2018-01-01 至今（约 1500 交易日 × 6 只 = 9000 样本）
- 划分：80% 训练 / 20% 验证（按时间切分，不随机打乱）

**训练参数**：
- Epochs: 100，早停 patience=10
- 优化器：Adam lr=1e-3
- 损失：MSE
- 归一化：MinMaxScaler 保存到 `data/models/scaler.pkl`
- 模型保存：`data/models/rssm_model.pt`

**`rssm_world_model.py` 推理接口**：
```python
def predict(self, recent_data: pd.DataFrame) -> dict:
    # 返回: {"predicted_return": float, "confidence": float, "regime": "bullish/bearish/neutral"}
    # 模型不存在时 fallback 到技术指标规则（RSI超买超卖 + MA趋势）
```

**`world_model_integration.py`**：补全被截断的 `predict_future()` 方法。

---

### 轨道 1：基础设施

#### 1.1 统一配置管理

**新建 `shared/config.py`**：
```python
from dataclasses import dataclass
import os
from pathlib import Path

@dataclass
class Config:
    deepseek_api_key: str
    feishu_app_id: str
    feishu_app_secret: str
    feishu_chat_id: str
    data_dir: Path
    log_dir: Path

def get_config() -> Config:
    """读取顺序: .env文件 → 环境变量 → 默认值"""
    _load_dotenv()
    base = Path(__file__).parent.parent
    return Config(
        deepseek_api_key=os.environ.get("DEEPSEEK_API_KEY", ""),
        feishu_app_id=os.environ.get("FEISHU_APP_ID", ""),
        feishu_app_secret=os.environ.get("FEISHU_APP_SECRET", ""),
        feishu_chat_id=os.environ.get("FEISHU_CHAT_ID", ""),
        data_dir=Path(os.environ.get("DATA_DIR", str(base / "data"))),
        log_dir=Path(os.environ.get("LOG_DIR", str(base / "prod" / "logs"))),
    )
```

**新建 `HKTech-Agent/.env.example`**（所有必要变量）：
```
DEEPSEEK_API_KEY=your_key_here
FEISHU_APP_ID=your_app_id
FEISHU_APP_SECRET=your_app_secret
FEISHU_CHAT_ID=your_chat_id
DATA_DIR=./data
LOG_DIR=./prod/logs
```

消灭所有文件里的 `/opt/hktech-agent/` 硬编码，改为 `config.data_dir`。

#### 1.2 错误处理 & 数据缓存

**`llm_enhanced_agent.py`** 主流程改造：
```python
# 每个 step 包裹 try/except
try:
    market_data = self._load_market_data(stocks)
except Exception as e:
    logger.error(f"Step 1 数据加载失败: {e}，使用缓存数据")
    market_data = self._load_cached_data(stocks)  # 读磁盘缓存
```

**`data_collector.py`** 磁盘缓存（12h TTL）：
- 成功抓取后写 `data/cache/{stock_code}_{date}.json`
- 失败时读 `data/cache/` 下最新文件
- 缓存过期（>12h）时打印警告但仍可用

#### 1.3 VectorBT 回测补全

**`active_src/vectorbt_integration.py`** 补全 `get_metrics()` 和完整 `run_backtest()`：

```python
def get_metrics(self, portfolio) -> dict:
    return {
        "sharpe": float(portfolio.sharpe_ratio()),
        "max_drawdown": float(portfolio.max_drawdown()),
        "total_return": float(portfolio.total_return()),
        "win_rate": float(portfolio.trades.win_rate()),
    }

def run_backtest(self, stock_code: str, signals: pd.Series,
                 start: str = "2022-01-01") -> dict:
    """signals: pd.Series with values 1(买)/−1(卖)/0(持有)"""
    ...
```

`train_world_model.py` 训练结束后自动调用回测验证模型效果。

#### 1.4 测试覆盖补充

新增测试文件：

| 文件 | 覆盖内容 |
|------|---------|
| `tests/unit/test_llm_integration.py` | mock DeepSeek HTTP，测试正常路径 + fallback |
| `tests/unit/test_rssm_model.py` | 用随机权重模型测试推理接口形状和类型 |
| `tests/unit/test_config.py` | 测试 `.env` 读取、环境变量覆盖、默认值 |
| `tests/integration/test_full_pipeline.py` | 端到端（全 mock 外部依赖），测试 6 步流程 |

---

## 文件变更清单

### 新建
- `shared/config.py`
- `HKTech-Agent/.env.example`
- `tests/unit/test_llm_integration.py`
- `tests/unit/test_rssm_model.py`
- `tests/unit/test_config.py`
- `tests/integration/test_full_pipeline.py`

### 修改（轨道 2）
- `prod/src/llm_signal_extractor.py` — `_call_llm_api()` 真实 DeepSeek 调用
- `prod/src/llm_decision_enhancer.py` — `_simulate_llm_analysis()` → 真实 DeepSeek，去掉 random，补合并权重
- `prod/src/rssm_world_model.py` — 替换 RSSM 为简化 GRU，补 fallback 技术指标
- `prod/src/world_model_integration.py` — 补全 `predict_future()`
- `prod/src/train_world_model.py` — 完整训练脚本，含回测验证

### 修改（轨道 1）
- `active_src/data_collector.py` — 磁盘缓存（12h TTL）
- `active_src/vectorbt_integration.py` — 补全 `get_metrics()` + `run_backtest()`
- `prod/src/llm_enhanced_agent.py` — 每步 try/except + 缓存降级
- `factory/core/config_manager.py` — 改用 `shared/config.py`
- `scripts/start_web_server.py` — 改用 `shared/config.py`

---

## 成功标准

1. `pytest tests/ -v` 全部通过，覆盖率 ≥ 70%
2. `DEEPSEEK_API_KEY` 设置后，运行 `llm_enhanced_agent.py` 能看到真实 LLM 返回的 JSON
3. `python3 train_world_model.py` 能训练完成并保存 `data/models/rssm_model.pt`
4. 训练完后 `python3 -c "from prod.src.rssm_world_model import *; print('model loaded')"` 无报错
5. 无任何 `/opt/hktech-agent/` 硬编码残留
6. 模拟 DeepSeek API 超时场景时，主流程不崩溃，Feishu 报告标注"数据降级"
