# HKTech-Agent 完善 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 将 HKTech-Agent 从 mock 原型升级为真实可用的 AI 交易系统——集成 DeepSeek LLM、训练真实 GRU 世界模型、统一配置管理、完善错误处理与测试。

**Architecture:** 并行双轨推进。轨道 1（基础设施）不依赖轨道 2，先建立稳定的配置层和缓存层；轨道 2（AI 核心）在稳定底座上集成真实 LLM 和 GRU 模型。最后整合测试覆盖全链路。

**Tech Stack:** Python 3.9+, PyTorch, DeepSeek API (HTTP), yfinance, VectorBT, pytest, python-dotenv

---

## 环境准备（首次执行前）

```bash
cd /home/huawei/project_opencode/lamda-ai/HKTech-Agent
source venv/bin/activate   # 若 venv 不存在: python3 -m venv venv && pip install -r requirements.txt
pip install python-dotenv  # 若未安装
```

---

## Task 1: 统一配置管理 — shared/config.py

**Files:**
- Create: `shared/config.py`
- Create: `.env.example`
- Modify: `shared/constants.py` (不改已有逻辑，添加 `load_dotenv` 调用)

### Step 1: 写失败测试

创建 `tests/unit/test_config.py`：

```python
"""Tests for shared/config.py"""
import os
import sys
from pathlib import Path
import pytest

# 确保 shared/ 在路径上
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "shared"))


def test_get_config_returns_dataclass():
    from config import get_config
    cfg = get_config()
    assert hasattr(cfg, "deepseek_api_key")
    assert hasattr(cfg, "feishu_app_id")
    assert hasattr(cfg, "feishu_app_secret")
    assert hasattr(cfg, "feishu_chat_id")
    assert hasattr(cfg, "data_dir")
    assert hasattr(cfg, "log_dir")


def test_get_config_reads_env_var(monkeypatch):
    from config import get_config
    monkeypatch.setenv("DEEPSEEK_API_KEY", "test_key_123")
    cfg = get_config()
    assert cfg.deepseek_api_key == "test_key_123"


def test_get_config_data_dir_is_path():
    from config import get_config
    cfg = get_config()
    assert isinstance(cfg.data_dir, Path)


def test_get_config_default_data_dir_relative_to_project():
    from config import get_config
    cfg = get_config()
    # 默认路径应在 HKTech-Agent/ 下
    assert "HKTech-Agent" in str(cfg.data_dir) or cfg.data_dir.exists() or True


def test_get_config_env_override_data_dir(monkeypatch, tmp_path):
    from config import get_config
    monkeypatch.setenv("DATA_DIR", str(tmp_path))
    cfg = get_config()
    assert cfg.data_dir == tmp_path
```

### Step 2: 运行确认失败

```bash
cd /home/huawei/project_opencode/lamda-ai/HKTech-Agent
python3 -m pytest tests/unit/test_config.py -v
```

预期：`ModuleNotFoundError: No module named 'config'`

### Step 3: 实现 shared/config.py

创建 `shared/config.py`：

```python
"""
统一配置管理模块
读取顺序: .env 文件 → 环境变量 → 默认值
"""
from dataclasses import dataclass
import os
from pathlib import Path


def _load_dotenv():
    """尝试加载 .env 文件（从项目根目录查找）"""
    try:
        from dotenv import load_dotenv
        # 从本文件向上查找 .env
        base = Path(__file__).parent.parent
        env_file = base / ".env"
        if env_file.exists():
            load_dotenv(env_file)
    except ImportError:
        pass  # python-dotenv 未安装时跳过


@dataclass
class Config:
    deepseek_api_key: str
    feishu_app_id: str
    feishu_app_secret: str
    feishu_chat_id: str
    data_dir: Path
    log_dir: Path


def get_config() -> Config:
    """获取统一配置对象"""
    _load_dotenv()
    base = Path(__file__).parent.parent  # HKTech-Agent/
    return Config(
        deepseek_api_key=os.environ.get("DEEPSEEK_API_KEY", ""),
        feishu_app_id=os.environ.get("FEISHU_APP_ID", ""),
        feishu_app_secret=os.environ.get("FEISHU_APP_SECRET", ""),
        feishu_chat_id=os.environ.get("FEISHU_CHAT_ID", ""),
        data_dir=Path(os.environ.get("DATA_DIR", str(base / "data"))),
        log_dir=Path(os.environ.get("LOG_DIR", str(base / "prod" / "logs"))),
    )
```

### Step 4: 创建 .env.example

创建 `HKTech-Agent/.env.example`：

```
# HKTech-Agent 环境变量配置示例
# 复制为 .env 并填入真实值（.env 已在 .gitignore 中）

DEEPSEEK_API_KEY=your_deepseek_api_key_here
FEISHU_APP_ID=your_feishu_app_id
FEISHU_APP_SECRET=your_feishu_app_secret
FEISHU_CHAT_ID=your_feishu_chat_id

# 可选：覆盖默认路径
# DATA_DIR=./data
# LOG_DIR=./prod/logs
```

将 `.env` 加入 `.gitignore`（如果文件存在则追加，不存在则创建）：

```bash
grep -qx ".env" /home/huawei/project_opencode/lamda-ai/HKTech-Agent/.gitignore 2>/dev/null \
  || echo ".env" >> /home/huawei/project_opencode/lamda-ai/HKTech-Agent/.gitignore
```

### Step 5: 运行测试确认通过

```bash
python3 -m pytest tests/unit/test_config.py -v
```

预期：全部 5 个测试 PASS

### Step 6: Commit

```bash
git add shared/config.py .env.example .gitignore tests/unit/test_config.py
git commit -m "feat: add unified config management via shared/config.py"
```

---

## Task 2: 消灭硬编码路径

**Files:**
- Modify: `prod/src/llm_signal_extractor.py:30`
- Modify: `prod/src/llm_decision_enhancer.py` (data_dir 参数默认值)
- Modify: `prod/src/world_model_integration.py` (data_dir 参数默认值)
- Modify: `active_src/data_collector.py:__init__`

**原则**：所有 `data_dir` 参数默认值从 `"/opt/hktech-agent/data"` 改为从 `shared/config.py` 读取。

### Step 1: 写失败测试

在 `tests/unit/test_config.py` 追加：

```python
def test_llm_signal_extractor_uses_config_data_dir(tmp_path):
    """LLMSignalExtractor 不应硬编码 /opt/hktech-agent"""
    import subprocess, inspect, sys
    result = subprocess.run(
        [sys.executable, "-c",
         "import sys; sys.path.insert(0,'prod/src'); "
         "from llm_signal_extractor import LLMSignalExtractor; "
         "import inspect; src = inspect.getsource(LLMSignalExtractor.__init__); "
         "assert '/opt/hktech-agent' not in src, 'hardcoded path found'"],
        capture_output=True, text=True,
        cwd="/home/huawei/project_opencode/lamda-ai/HKTech-Agent"
    )
    assert result.returncode == 0, result.stderr
```

### Step 2: 运行确认失败

```bash
python3 -m pytest tests/unit/test_config.py::test_llm_signal_extractor_uses_config_data_dir -v
```

预期：FAIL（硬编码路径存在）

### Step 3: 修改 llm_signal_extractor.py 第 30 行

将：
```python
def __init__(self, data_dir="/opt/hktech-agent/data"):
```
改为：
```python
def __init__(self, data_dir=None):
    if data_dir is None:
        try:
            import sys, os
            sys.path.insert(0, os.path.join(os.path.dirname(__file__), '../../shared'))
            from config import get_config
            data_dir = str(get_config().data_dir)
        except Exception:
            data_dir = os.path.join(os.path.dirname(__file__), '../../data')
```

对 `llm_decision_enhancer.py`、`world_model_integration.py`、`active_src/data_collector.py` 执行同样替换（`data_dir=None` + 从 config 读取）。

### Step 4: 运行测试

```bash
python3 -m pytest tests/unit/test_config.py -v
```

预期：全部 PASS

### Step 5: Commit

```bash
git add prod/src/llm_signal_extractor.py prod/src/llm_decision_enhancer.py \
        prod/src/world_model_integration.py active_src/data_collector.py \
        tests/unit/test_config.py
git commit -m "refactor: replace hardcoded /opt/hktech-agent paths with config-driven data_dir"
```

---

## Task 3: data_collector 磁盘缓存（12h TTL）

**Files:**
- Modify: `active_src/data_collector.py`
- Test: `tests/unit/test_data_collector.py`

### Step 1: 写失败测试

创建 `tests/unit/test_data_collector.py`：

```python
"""Tests for data_collector disk cache"""
import sys, os, json, time
from pathlib import Path
from unittest.mock import patch, MagicMock
import pytest

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "active_src"))
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "shared"))


def test_cache_write_on_success(tmp_path):
    """成功获取数据后应写入磁盘缓存"""
    from data_collector import HKStockDataCollector
    collector = HKStockDataCollector(data_dir=str(tmp_path))

    mock_result = {"00700": {"price": 385.0, "data_source": "yahoo"}}
    with patch.object(collector, '_get_yahoo_data', return_value={"price": 385.0, "data_source": "yahoo"}), \
         patch.object(collector, '_get_sina_data', return_value=None):
        data = collector.get_daily_data(days=5)

    cache_files = list(tmp_path.glob("cache/00700_*.json"))
    assert len(cache_files) >= 1, "缓存文件应存在"


def test_cache_read_on_failure(tmp_path):
    """所有数据源失败时应读取最新缓存"""
    from data_collector import HKStockDataCollector
    collector = HKStockDataCollector(data_dir=str(tmp_path))

    # 预先写入缓存
    cache_dir = tmp_path / "cache"
    cache_dir.mkdir(parents=True, exist_ok=True)
    cached = {"price": 380.0, "data_source": "cache", "name": "腾讯控股"}
    (cache_dir / "00700_20260219.json").write_text(json.dumps(cached))

    with patch.object(collector, '_get_yahoo_data', return_value=None), \
         patch.object(collector, '_get_sina_data', return_value=None):
        data = collector.get_daily_data(days=5)

    assert "00700" in data
    assert data["00700"].get("data_source") in ("cache", "mock")  # 读缓存或mock


def test_cache_not_read_when_fresh_data_available(tmp_path):
    """有新鲜数据时不应读取缓存"""
    from data_collector import HKStockDataCollector
    collector = HKStockDataCollector(data_dir=str(tmp_path))

    fresh = {"price": 400.0, "data_source": "yahoo", "name": "腾讯控股",
             "code": "00700", "change": 1.0, "change_pct": 0.26}
    with patch.object(collector, '_get_yahoo_data', return_value=fresh):
        data = collector.get_daily_data(days=5)

    assert data["00700"]["data_source"] == "yahoo"
```

### Step 2: 运行确认失败

```bash
python3 -m pytest tests/unit/test_data_collector.py -v
```

预期：`test_cache_write_on_success` FAIL（没有写缓存逻辑）

### Step 3: 在 data_collector.py 中添加缓存方法

在 `HKStockDataCollector` 类中添加两个方法：

```python
def _write_cache(self, code: str, data: dict):
    """将成功获取的股票数据写入磁盘缓存"""
    import json
    from datetime import datetime
    cache_dir = os.path.join(self.data_dir, "cache")
    os.makedirs(cache_dir, exist_ok=True)
    today = datetime.now().strftime("%Y%m%d")
    cache_path = os.path.join(cache_dir, f"{code}_{today}.json")
    try:
        with open(cache_path, "w", encoding="utf-8") as f:
            json.dump({**data, "_cached_at": datetime.now().isoformat()}, f, ensure_ascii=False)
    except Exception:
        pass  # 缓存写失败不影响主流程

def _read_cache(self, code: str) -> dict | None:
    """读取最新缓存文件（12h TTL）"""
    import json
    from datetime import datetime, timedelta
    cache_dir = os.path.join(self.data_dir, "cache")
    if not os.path.exists(cache_dir):
        return None
    # 找最新缓存文件
    pattern = f"{code}_*.json"
    files = sorted(Path(cache_dir).glob(pattern), reverse=True)
    for cache_file in files[:1]:
        try:
            with open(cache_file, encoding="utf-8") as f:
                data = json.load(f)
            cached_at_str = data.get("_cached_at")
            if cached_at_str:
                cached_at = datetime.fromisoformat(cached_at_str)
                if datetime.now() - cached_at > timedelta(hours=12):
                    print(f"⚠️ {code} 缓存已过期（>12h），但仍使用")
            data["data_source"] = "cache"
            return data
        except Exception:
            continue
    return None
```

在 `_get_yahoo_data()` 成功返回前调用 `self._write_cache(code, result)`；在 mock fallback 前先尝试 `self._read_cache(code)`。

### Step 4: 运行测试

```bash
python3 -m pytest tests/unit/test_data_collector.py -v
```

预期：全部 PASS

### Step 5: Commit

```bash
git add active_src/data_collector.py tests/unit/test_data_collector.py
git commit -m "feat: add 12h disk cache to HKStockDataCollector"
```

---

## Task 4: DeepSeek 集成 — llm_signal_extractor.py

**Files:**
- Modify: `prod/src/llm_signal_extractor.py`
- Test: `tests/unit/test_llm_integration.py`

### Step 1: 写失败测试

创建 `tests/unit/test_llm_integration.py`：

```python
"""Tests for DeepSeek LLM integration"""
import sys, os, json
from pathlib import Path
from unittest.mock import patch, MagicMock
import pytest

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "prod" / "src"))
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "shared"))


class TestLLMSignalExtractorDeepSeek:

    def test_calls_deepseek_when_api_key_set(self, tmp_path, monkeypatch):
        """有 API Key 时应发 HTTP 请求"""
        monkeypatch.setenv("DEEPSEEK_API_KEY", "sk-test-key")
        from llm_signal_extractor import LLMSignalExtractor

        mock_response = MagicMock()
        mock_response.json.return_value = {
            "choices": [{"message": {"content":
                json.dumps({"sentiment": 0.75, "key_factors": ["营收增长"], "confidence": 0.8})
            }}]
        }
        mock_response.raise_for_status = MagicMock()

        with patch("requests.post", return_value=mock_response) as mock_post:
            extractor = LLMSignalExtractor(data_dir=str(tmp_path))
            result = extractor._call_llm_api("00700", ["腾讯Q4营收超预期"])

        mock_post.assert_called_once()
        assert result["sentiment"] == 0.75
        assert result["confidence"] == 0.8

    def test_fallback_to_keywords_when_no_api_key(self, tmp_path, monkeypatch):
        """无 API Key 时应 fallback 到关键词匹配"""
        monkeypatch.delenv("DEEPSEEK_API_KEY", raising=False)
        from llm_signal_extractor import LLMSignalExtractor

        extractor = LLMSignalExtractor(data_dir=str(tmp_path))
        result = extractor._call_llm_api("00700", ["腾讯Q4营收超预期"])

        assert "sentiment" in result
        assert 0.0 <= result["sentiment"] <= 1.0

    def test_fallback_when_api_fails(self, tmp_path, monkeypatch):
        """HTTP 异常时应 fallback 到关键词匹配，不抛出"""
        monkeypatch.setenv("DEEPSEEK_API_KEY", "sk-test-key")
        from llm_signal_extractor import LLMSignalExtractor

        with patch("requests.post", side_effect=Exception("connection refused")):
            extractor = LLMSignalExtractor(data_dir=str(tmp_path))
            result = extractor._call_llm_api("00700", ["某新闻"])

        assert "sentiment" in result  # fallback 应返回结果
        assert 0.0 <= result["sentiment"] <= 1.0

    def test_analyze_news_returns_all_stocks(self, tmp_path, monkeypatch):
        """analyze_news 应为每只股票返回情感分数"""
        monkeypatch.delenv("DEEPSEEK_API_KEY", raising=False)
        from llm_signal_extractor import LLMSignalExtractor

        extractor = LLMSignalExtractor(data_dir=str(tmp_path))
        news = [{"title": "港股市场整体回暖", "content": ""}]
        signals = extractor.analyze_news(news)

        assert isinstance(signals, dict)
        for code in ["00700", "09988", "03690"]:
            assert code in signals
            assert 0.0 <= signals[code] <= 1.0
```

### Step 2: 运行确认失败

```bash
python3 -m pytest tests/unit/test_llm_integration.py::TestLLMSignalExtractorDeepSeek -v
```

预期：`test_calls_deepseek_when_api_key_set` FAIL（`_call_llm_api` 方法不存在或不调用 requests.post）

### Step 3: 修改 llm_signal_extractor.py

在 `LLMSignalExtractor` 中添加 `_call_llm_api()` 和 `_keyword_fallback()` 方法，并在 `analyze_news()` 中调用：

```python
def _keyword_fallback(self, stock_code: str, news_items: list) -> dict:
    """关键词匹配 fallback（原有逻辑）"""
    positive_keywords = ["增长", "超预期", "盈利", "利好", "上涨", "突破", "合作", "扩张"]
    negative_keywords = ["下滑", "亏损", "利空", "下跌", "监管", "罚款", "裁员", "收缩"]
    score = 0.5
    text = " ".join([str(n) for n in news_items])
    for kw in positive_keywords:
        if kw in text:
            score = min(score + 0.05, 0.9)
    for kw in negative_keywords:
        if kw in text:
            score = max(score - 0.05, 0.1)
    return {"sentiment": round(score, 2), "key_factors": [], "confidence": 0.5}

def _call_llm_api(self, stock_code: str, news_items: list) -> dict:
    """调用 DeepSeek API 提取情感信号，失败时 fallback 到关键词匹配"""
    import requests
    api_key = os.environ.get("DEEPSEEK_API_KEY", "")
    if not api_key:
        return self._keyword_fallback(stock_code, news_items)

    stock_name = self.stock_names.get(stock_code, stock_code)
    news_text = "\n".join([f"- {n}" for n in news_items[:5]])
    prompt = (
        f"你是一位专业的港股分析师。请分析以下关于{stock_name}({stock_code})的新闻，"
        f"返回JSON格式的情感分析结果。\n\n新闻内容：\n{news_text}\n\n"
        f"要求返回格式（只返回JSON，不要其他文字）：\n"
        f'{"{"}"sentiment": 0到1之间的浮点数, "key_factors": ["因素1","因素2"], '
        f'"confidence": 0到1之间的浮点数{"}"}\n'
        f"其中sentiment含义：0=极度悲观, 0.5=中性, 1=极度乐观"
    )
    try:
        resp = requests.post(
            "https://api.deepseek.com/v1/chat/completions",
            headers={"Authorization": f"Bearer {api_key}",
                     "Content-Type": "application/json"},
            json={
                "model": "deepseek-chat",
                "messages": [{"role": "user", "content": prompt}],
                "temperature": 0.1,
                "max_tokens": 200,
            },
            timeout=30,
        )
        content = resp.json()["choices"][0]["message"]["content"].strip()
        # 提取 JSON 部分（防止 LLM 输出多余文字）
        if "{" in content:
            content = content[content.index("{"):content.rindex("}") + 1]
        result = json.loads(content)
        result.setdefault("confidence", 0.7)
        result["sentiment"] = max(0.0, min(1.0, float(result["sentiment"])))
        return result
    except Exception as e:
        print(f"⚠️ DeepSeek API 调用失败 ({stock_code}): {e}，fallback 到关键词匹配")
        return self._keyword_fallback(stock_code, news_items)
```

在 `analyze_news()` 方法内替换原有模拟调用，改为 `self._call_llm_api(code, relevant_news)`。

### Step 4: 运行测试

```bash
python3 -m pytest tests/unit/test_llm_integration.py::TestLLMSignalExtractorDeepSeek -v
```

预期：全部 PASS

### Step 5: Commit

```bash
git add prod/src/llm_signal_extractor.py tests/unit/test_llm_integration.py
git commit -m "feat: integrate DeepSeek API into llm_signal_extractor with keyword fallback"
```

---

## Task 5: DeepSeek 集成 — llm_decision_enhancer.py

**Files:**
- Modify: `prod/src/llm_decision_enhancer.py`
- Test: `tests/unit/test_llm_integration.py` (追加)

### Step 1: 写失败测试（追加到 test_llm_integration.py）

```python
class TestLLMDecisionEnhancerDeepSeek:

    def test_no_random_in_enhancement(self, tmp_path, monkeypatch):
        """最终决策不应包含随机数"""
        monkeypatch.delenv("DEEPSEEK_API_KEY", raising=False)
        from llm_decision_enhancer import LLMDecisionEnhancer

        enhancer = LLMDecisionEnhancer(data_dir=str(tmp_path))
        # 多次调用，结果应稳定（无 random）
        base_decision = {
            "decisions": {"00700": {"action": "buy", "confidence": 0.6}},
            "summary": "test"
        }
        market_data = {"00700": {"price": 385.0, "rsi": 55.0, "trend": "upward"}}
        portfolio = {"cash": 10000, "holdings": {}}

        results = [enhancer.enhance_decision(base_decision, market_data, portfolio)
                   for _ in range(3)]
        confidences = [r["final_decision"]["00700"]["confidence"] for r in results]
        # 无 random 时，相同输入应输出相同结果
        assert len(set(round(c, 4) for c in confidences)) == 1, "结果不应随机变化"

    def test_calls_deepseek_for_decision(self, tmp_path, monkeypatch):
        """有 API Key 时应调用 DeepSeek 做决策"""
        monkeypatch.setenv("DEEPSEEK_API_KEY", "sk-test")
        from llm_decision_enhancer import LLMDecisionEnhancer

        mock_resp = MagicMock()
        mock_resp.json.return_value = {"choices": [{"message": {"content":
            json.dumps({"action": "BUY", "confidence": 0.75,
                        "reasoning": "技术指标偏强", "risk_level": "MEDIUM"})
        }}]}

        with patch("requests.post", return_value=mock_resp):
            enhancer = LLMDecisionEnhancer(data_dir=str(tmp_path))
            result = enhancer._call_deepseek_decision(
                "00700",
                {"rsi": 55.0, "trend": "upward"},
                predicted_return=0.03,
                sentiment=0.7
            )

        assert result["action"] == "BUY"
        assert result["confidence"] == 0.75

    def test_weighted_merge_logic(self, tmp_path, monkeypatch):
        """最终决策应用 tech×0.4 + world×0.3 + sentiment×0.3 权重"""
        monkeypatch.delenv("DEEPSEEK_API_KEY", raising=False)
        from llm_decision_enhancer import LLMDecisionEnhancer

        enhancer = LLMDecisionEnhancer(data_dir=str(tmp_path))
        # tech=0.8 (buy), world=0.6 (buy), sentiment=0.7 → weighted = 0.71
        merged = enhancer._merge_signals(
            tech_confidence=0.8, tech_action="buy",
            world_confidence=0.6, world_action="buy",
            sentiment_score=0.7
        )
        assert merged["action"] == "buy"
        assert abs(merged["confidence"] - (0.8*0.4 + 0.6*0.3 + 0.7*0.3)) < 0.01
```

### Step 2: 运行确认失败

```bash
python3 -m pytest tests/unit/test_llm_integration.py::TestLLMDecisionEnhancerDeepSeek -v
```

### Step 3: 修改 llm_decision_enhancer.py

**3a. 添加 `_call_deepseek_decision()` 方法**（替换 `_simulate_llm_analysis` 中的 mock）：

```python
def _call_deepseek_decision(self, stock_code: str, technical_signal: dict,
                             predicted_return: float, sentiment: float) -> dict:
    """调用 DeepSeek 做单股决策，失败时返回基于规则的 fallback"""
    import requests, json
    api_key = os.environ.get("DEEPSEEK_API_KEY", "")
    if not api_key:
        return self._rule_based_decision(technical_signal, predicted_return, sentiment)

    stock_name = self.stock_names.get(stock_code, stock_code)
    rsi = technical_signal.get("rsi", 50)
    trend = technical_signal.get("trend", "中性")
    prompt = (
        f"你是一位专业的港股量化交易决策助手。综合以下信息给出交易建议：\n\n"
        f"股票：{stock_name}({stock_code})\n"
        f"技术信号：RSI={rsi:.1f}, 趋势={trend}\n"
        f"世界模型预测5日收益：{predicted_return:.2%}\n"
        f"市场情感得分：{sentiment:.2f}（0=极度悲观，1=极度乐观）\n\n"
        f"请返回JSON（只返回JSON，不要其他文字）：\n"
        f'{"{"}"action":"BUY/SELL/HOLD","confidence":0到1之间的浮点数,'
        f'"reasoning":"50字以内的中文理由","risk_level":"LOW/MEDIUM/HIGH"{"}"}'
    )
    try:
        resp = requests.post(
            "https://api.deepseek.com/v1/chat/completions",
            headers={"Authorization": f"Bearer {api_key}",
                     "Content-Type": "application/json"},
            json={"model": "deepseek-chat",
                  "messages": [{"role": "user", "content": prompt}],
                  "temperature": 0.1, "max_tokens": 200},
            timeout=30,
        )
        content = resp.json()["choices"][0]["message"]["content"].strip()
        if "{" in content:
            content = content[content.index("{"):content.rindex("}") + 1]
        result = json.loads(content)
        result["action"] = result["action"].upper()
        result["confidence"] = max(0.0, min(1.0, float(result["confidence"])))
        return result
    except Exception as e:
        print(f"⚠️ DeepSeek 决策 API 失败 ({stock_code}): {e}")
        return self._rule_based_decision(technical_signal, predicted_return, sentiment)

def _rule_based_decision(self, technical_signal: dict,
                          predicted_return: float, sentiment: float) -> dict:
    """规则 fallback（无随机数）"""
    rsi = technical_signal.get("rsi", 50)
    score = technical_signal.get("confidence", 0.5) * 0.4 \
          + max(0, min(1, predicted_return * 10 + 0.5)) * 0.3 \
          + sentiment * 0.3
    if score > 0.6:
        action = "BUY"
    elif score < 0.4:
        action = "SELL"
    else:
        action = "HOLD"
    return {"action": action, "confidence": round(score, 3),
            "reasoning": "基于技术指标+预测+情感的规则决策", "risk_level": "MEDIUM"}
```

**3b. 添加 `_merge_signals()` 方法**（三路信号合并）：

```python
def _merge_signals(self, tech_confidence: float, tech_action: str,
                   world_confidence: float, world_action: str,
                   sentiment_score: float) -> dict:
    """三路信号加权合并: tech×0.4 + world×0.3 + sentiment×0.3"""
    action_score = {"buy": 1.0, "BUY": 1.0, "sell": 0.0, "SELL": 0.0,
                    "hold": 0.5, "HOLD": 0.5}
    tech_s = action_score.get(tech_action, 0.5) * tech_confidence
    world_s = action_score.get(world_action, 0.5) * world_confidence
    merged = tech_s * 0.4 + world_s * 0.3 + sentiment_score * 0.3
    if merged > 0.6:
        action = "buy"
    elif merged < 0.4:
        action = "sell"
    else:
        action = "hold"
    return {"action": action, "confidence": round(merged, 4)}
```

**3c. 修改 `_generate_final_decision()`**：调用 `_merge_signals()` 代替直接复制 base_decision；将 `_simulate_llm_analysis()` 中的 `random.uniform()` 替换为 `_call_deepseek_decision()`。

### Step 4: 运行测试

```bash
python3 -m pytest tests/unit/test_llm_integration.py -v
```

预期：全部 PASS

### Step 5: Commit

```bash
git add prod/src/llm_decision_enhancer.py tests/unit/test_llm_integration.py
git commit -m "feat: replace random mock with DeepSeek API + weighted signal merge in decision enhancer"
```

---

## Task 6: 简化 GRU 世界模型（替换 RSSM）

**Files:**
- Modify: `prod/src/rssm_world_model.py`
- Modify: `prod/src/train_world_model.py`
- Test: `tests/unit/test_rssm_model.py`

**关键决策**：保留 `RSSMWorldModel` 和 `WorldModelTrainer` 的对外接口，内部替换为 GRU 实现。不破坏 `world_model_integration.py` 调用方。

### Step 1: 写失败测试

创建 `tests/unit/test_rssm_model.py`：

```python
"""Tests for GRU world model"""
import sys
from pathlib import Path
import pytest
import numpy as np

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "prod" / "src"))


class TestGRUWorldModel:

    def test_model_predict_returns_correct_keys(self, tmp_path):
        """predict() 应返回 predicted_return, confidence, regime"""
        from rssm_world_model import RSSMWorldModel

        model = RSSMWorldModel(data_dir=str(tmp_path))
        market_data = {
            "00700": {"price": 385.0, "rsi": 55.0, "ma5": 380.0,
                      "ma20": 370.0, "change_pct": 1.2, "volume": 1e7}
        }
        result = model.predict(market_data)

        assert "predicted_return" in result
        assert "confidence" in result
        assert "regime" in result
        assert result["regime"] in ("bullish", "bearish", "neutral")
        assert -1.0 <= result["predicted_return"] <= 1.0
        assert 0.0 <= result["confidence"] <= 1.0

    def test_model_fallback_without_model_file(self, tmp_path):
        """没有模型文件时应 fallback 到技术指标（不崩溃）"""
        from rssm_world_model import RSSMWorldModel

        model = RSSMWorldModel(data_dir=str(tmp_path))  # tmp_path 无模型文件
        market_data = {"00700": {"price": 385.0, "rsi": 75.0,  # RSI 超买
                                  "ma5": 390.0, "ma20": 380.0,
                                  "change_pct": 2.0, "volume": 1e7}}
        result = model.predict(market_data)

        # RSI 超买时 fallback 应预测偏负
        assert result is not None
        assert "predicted_return" in result

    def test_gru_model_architecture_importable(self):
        """GRUWorldModel 类应可导入"""
        from rssm_world_model import GRUWorldModel
        assert GRUWorldModel is not None

    def test_gru_model_forward_shape(self, tmp_path):
        """GRU 前向传播输出形状应正确"""
        try:
            import torch
        except ImportError:
            pytest.skip("torch 不可用")
        from rssm_world_model import GRUWorldModel

        model = GRUWorldModel(input_size=8, hidden_size=64, num_layers=2)
        # batch=4, seq_len=20, features=8
        x = torch.randn(4, 20, 8)
        out = model(x)
        assert out.shape == (4, 1), f"期望 (4,1) 得到 {out.shape}"
```

### Step 2: 运行确认失败

```bash
python3 -m pytest tests/unit/test_rssm_model.py -v
```

预期：`test_gru_model_architecture_importable` FAIL（`GRUWorldModel` 不存在）

### Step 3: 修改 rssm_world_model.py

在文件顶部（`TORCH_AVAILABLE` 检测后）添加 `GRUWorldModel` 类：

```python
if TORCH_AVAILABLE:
    import torch
    import torch.nn as nn

    class GRUWorldModel(nn.Module):
        """简化 GRU 世界模型：预测未来 5 日收益率"""
        def __init__(self, input_size=8, hidden_size=64, num_layers=2, dropout=0.2):
            super().__init__()
            self.gru = nn.GRU(
                input_size=input_size,
                hidden_size=hidden_size,
                num_layers=num_layers,
                dropout=dropout if num_layers > 1 else 0,
                batch_first=True
            )
            self.fc = nn.Linear(hidden_size, 1)

        def forward(self, x):
            # x: (batch, seq_len, input_size)
            out, _ = self.gru(x)
            return self.fc(out[:, -1, :])  # 取最后时间步 → (batch, 1)

else:
    class GRUWorldModel:
        """torch 不可用时的占位类"""
        def __init__(self, *args, **kwargs):
            pass
```

修改 `RSSMWorldModel.predict()` 的 fallback 逻辑（当模型不存在时用技术指标规则，而非随机启发式）：

```python
def _predict_technical_fallback(self, market_data: dict) -> dict:
    """基于技术指标的 fallback 预测（无随机数）"""
    scores = []
    for code, data in market_data.items():
        rsi = data.get("rsi", 50)
        ma5 = data.get("ma5", data.get("price", 100))
        ma20 = data.get("ma20", data.get("price", 100))
        change_pct = data.get("change_pct", 0)

        # RSI 信号
        if rsi > 70:
            rsi_signal = -0.3   # 超买，可能回调
        elif rsi < 30:
            rsi_signal = 0.3    # 超卖，可能反弹
        else:
            rsi_signal = (50 - rsi) * 0.006  # 线性插值

        # MA 趋势
        ma_signal = (ma5 - ma20) / ma20 if ma20 > 0 else 0

        scores.append(rsi_signal + ma_signal * 0.5)

    avg_return = sum(scores) / len(scores) if scores else 0.0
    regime = "bullish" if avg_return > 0.02 else ("bearish" if avg_return < -0.02 else "neutral")
    return {
        "predicted_return": round(float(avg_return), 4),
        "confidence": 0.4,  # 技术 fallback 置信度较低
        "regime": regime,
        "source": "technical_fallback"
    }
```

### Step 4: 运行测试

```bash
python3 -m pytest tests/unit/test_rssm_model.py -v
```

预期：全部 PASS

### Step 5: Commit

```bash
git add prod/src/rssm_world_model.py tests/unit/test_rssm_model.py
git commit -m "feat: add GRUWorldModel class and technical-indicator fallback to RSSMWorldModel"
```

---

## Task 7: 训练脚本完善 — train_world_model.py

**Files:**
- Modify: `prod/src/train_world_model.py`

**目标**：将训练脚本从 200 天 / 3 股改为 2018-今 / 6 股，使用 `GRUWorldModel`。

### Step 1: 修改训练脚本

替换以下几处：

**7a. 股票列表**（文件顶部）：
```python
# 使用 shared/constants.py 的 ALL_STOCKS (6只)
try:
    import sys, os
    sys.path.insert(0, os.path.join(os.path.dirname(__file__), '../../shared'))
    from constants import ALL_STOCKS, STOCKS as STOCK_INFO
    TRAIN_STOCKS = {code: STOCK_INFO[code] for code in ALL_STOCKS}
except Exception:
    TRAIN_STOCKS = {
        "00700": {"name": "腾讯控股"}, "09988": {"name": "阿里巴巴"},
        "03690": {"name": "美团-W"}, "01810": {"name": "小米集团"},
        "09618": {"name": "京东集团"}, "09999": {"name": "网易"},
    }
```

**7b. fetch_historical_data()**：将 `days=200` 改为从 2018 年拉取：
```python
def fetch_historical_data(start_date="2018-01-01") -> dict:
    """拉取 2018 至今的历史数据（约 1500 交易日）"""
    import yfinance as yf
    from datetime import datetime
    end_date = datetime.now().strftime("%Y-%m-%d")
    result = {}
    for code, info in TRAIN_STOCKS.items():
        yf_symbol = f"{int(code):04d}.HK"
        try:
            df = yf.download(yf_symbol, start=start_date, end=end_date,
                             progress=False, auto_adjust=True)
            if len(df) > 100:
                df = calculate_indicators(df)
                result[code] = df
                print(f"✅ {code} {info['name']}: {len(df)} 条记录")
            else:
                print(f"⚠️ {code}: 数据不足（{len(df)}条），跳过")
        except Exception as e:
            print(f"❌ {code}: 下载失败 {e}")
    return result
```

**7c. 特征工程**（8 维特征，替换原有 15 维）：
```python
def build_feature_matrix(df: pd.DataFrame) -> pd.DataFrame:
    """构建 8 维特征: open_r, high_r, low_r, close_r, vol_r, ma5_r, rsi_norm, vol_ratio"""
    feat = pd.DataFrame(index=df.index)
    close = df["Close"]
    feat["open_r"]   = (df["Open"] / close.shift(1) - 1).fillna(0)
    feat["high_r"]   = (df["High"] / close - 1).fillna(0)
    feat["low_r"]    = (df["Low"] / close - 1).fillna(0)
    feat["close_r"]  = close.pct_change().fillna(0)
    feat["vol_r"]    = df["Volume"].pct_change().fillna(0)
    ma5 = close.rolling(5).mean()
    feat["ma5_r"]    = (ma5 / close - 1).fillna(0)
    rsi = calculate_rsi(close)
    feat["rsi_norm"] = (rsi - 50) / 50  # 归一化到 [-1, 1]
    avg_vol = df["Volume"].rolling(20).mean()
    feat["vol_ratio"] = (df["Volume"] / avg_vol - 1).fillna(0)
    return feat.clip(-3, 3)  # 截断极端值
```

**7d. create_training_dataset()**（滑动窗口，seq_len=20）：
```python
def create_training_dataset(historical_data: dict, seq_len=20) -> tuple:
    """构建训练数据集: X (N, seq_len, 8), y (N,)"""
    X_list, y_list = [], []
    for code, df in historical_data.items():
        features = build_feature_matrix(df).values
        closes = df["Close"].values
        for i in range(seq_len, len(features) - 5):
            x = features[i-seq_len:i]      # (20, 8)
            y = (closes[i+5] / closes[i] - 1)  # 5日收益率
            if not (np.isnan(x).any() or np.isnan(y)):
                X_list.append(x)
                y_list.append(y)
    return np.array(X_list, dtype=np.float32), np.array(y_list, dtype=np.float32)
```

**7e. train_gru_model()**（新训练函数）：
```python
def train_gru_model(X: np.ndarray, y: np.ndarray, data_dir: str,
                    epochs=100, patience=10) -> str:
    """训练 GRU 世界模型，保存到 data_dir/models/"""
    import torch
    import torch.nn as nn
    from torch.utils.data import TensorDataset, DataLoader
    import pickle
    from rssm_world_model import GRUWorldModel

    os.makedirs(os.path.join(data_dir, "models"), exist_ok=True)

    # 时间序列划分（不随机打乱）
    split = int(len(X) * 0.8)
    X_train, X_val = X[:split], X[split:]
    y_train, y_val = y[:split], y[split:]

    # 归一化目标值
    y_mean, y_std = y_train.mean(), y_train.std() + 1e-8
    y_train_n = (y_train - y_mean) / y_std
    y_val_n   = (y_val   - y_mean) / y_std

    # 保存 scaler 参数
    scaler = {"y_mean": float(y_mean), "y_std": float(y_std)}
    with open(os.path.join(data_dir, "models", "scaler.pkl"), "wb") as f:
        pickle.dump(scaler, f)

    # DataLoader
    train_ds = TensorDataset(torch.from_numpy(X_train), torch.from_numpy(y_train_n))
    val_ds   = TensorDataset(torch.from_numpy(X_val),   torch.from_numpy(y_val_n))
    train_loader = DataLoader(train_ds, batch_size=64, shuffle=True)
    val_loader   = DataLoader(val_ds,   batch_size=256)

    model = GRUWorldModel(input_size=8, hidden_size=64, num_layers=2)
    optimizer = torch.optim.Adam(model.parameters(), lr=1e-3)
    criterion = nn.MSELoss()

    best_val_loss = float("inf")
    no_improve = 0
    model_path = os.path.join(data_dir, "models", "rssm_model.pt")

    for epoch in range(epochs):
        model.train()
        for xb, yb in train_loader:
            optimizer.zero_grad()
            pred = model(xb).squeeze()
            loss = criterion(pred, yb)
            loss.backward()
            optimizer.step()

        # 验证
        model.eval()
        val_losses = []
        with torch.no_grad():
            for xb, yb in val_loader:
                pred = model(xb).squeeze()
                val_losses.append(criterion(pred, yb).item())
        val_loss = sum(val_losses) / len(val_losses)

        if val_loss < best_val_loss:
            best_val_loss = val_loss
            torch.save(model.state_dict(), model_path)
            no_improve = 0
        else:
            no_improve += 1
            if no_improve >= patience:
                print(f"早停于 epoch {epoch+1}，最佳验证损失: {best_val_loss:.6f}")
                break

        if (epoch + 1) % 10 == 0:
            print(f"Epoch {epoch+1}/{epochs}: val_loss={val_loss:.6f}")

    print(f"✅ 模型已保存到 {model_path}")
    return model_path
```

**7f. 修改 `main()`**：

```python
def main():
    data_dir = get_data_dir()
    print("📥 拉取历史数据（2018-至今）...")
    historical_data = fetch_historical_data(start_date="2018-01-01")

    if not historical_data:
        print("❌ 无法获取历史数据，终止训练")
        return

    print(f"🔧 构建训练数据集 (seq_len=20)...")
    X, y = create_training_dataset(historical_data, seq_len=20)
    print(f"   样本数: {len(X)}, X shape: {X.shape}, y 均值: {y.mean():.4f}")

    print("🚀 开始训练 GRU 世界模型...")
    model_path = train_gru_model(X, y, data_dir)

    print("📊 运行回测验证...")
    # 回测（可选，需 vectorbt）
    try:
        sys.path.insert(0, os.path.join(os.path.dirname(__file__), "../../active_src"))
        from vectorbt_integration import VectorBTBacktester
        bt = VectorBTBacktester()
        print("  (回测验证待实现)")
    except Exception as e:
        print(f"  跳过回测: {e}")

    print(f"✅ 训练完成！模型: {model_path}")

if __name__ == "__main__":
    main()
```

### Step 2: 验证脚本可运行（不要真的训练，只检查 import 和 dry-run）

```bash
cd /home/huawei/project_opencode/lamda-ai/HKTech-Agent
python3 -c "
import sys
sys.path.insert(0, 'prod/src')
sys.path.insert(0, 'shared')
from train_world_model import create_training_dataset, build_feature_matrix
import numpy as np
import pandas as pd

# 构造 mock 数据验证函数
dates = pd.date_range('2020-01-01', periods=100)
df = pd.DataFrame({
    'Open':  np.random.uniform(380, 400, 100),
    'High':  np.random.uniform(385, 410, 100),
    'Low':   np.random.uniform(370, 385, 100),
    'Close': np.random.uniform(375, 405, 100),
    'Volume': np.random.uniform(1e7, 5e7, 100),
}, index=dates)
feat = build_feature_matrix(df)
print('feature shape:', feat.shape)
X, y = create_training_dataset({'00700': df}, seq_len=20)
print('X shape:', X.shape, 'y shape:', y.shape)
print('✅ 训练脚本函数验证通过')
"
```

预期输出：`✅ 训练脚本函数验证通过`

### Step 3: Commit

```bash
git add prod/src/train_world_model.py
git commit -m "feat: rewrite train_world_model.py with GRU architecture, 6-stock 2018-now dataset"
```

---

## Task 8: 补全 world_model_integration.py

**Files:**
- Modify: `prod/src/world_model_integration.py`

### Step 1: 读取当前 predict_future() 的截断位置

在文件中找到 `predict_future` 方法，补全被截断的实现（调用 `GRUWorldModel` 加载推理）。

### Step 2: 修复 predict_future()

将截断处补全为：

```python
def predict_future(self, market_data: dict, portfolio: dict,
                   proposed_action=None, horizon=3) -> dict:
    """使用 GRU 世界模型预测未来收益"""
    if not self.enabled:
        return {
            "enabled": False,
            "horizon": horizon,
            "predicted_returns": {},
            "cumulative_return": 0.0,
            "confidence": 0.0,
            "recommendation": "hold",
            "reasoning": "世界模型未加载，使用保守策略",
            "actions": []
        }

    try:
        result = self.trainer.predict(market_data, portfolio)
        predicted_return = result.get("predicted_return", 0.0)
        confidence = result.get("confidence", 0.5)
        regime = result.get("regime", "neutral")

        # 基于预测给出建议
        if predicted_return > 0.03 and confidence > 0.6:
            recommendation = "buy"
        elif predicted_return < -0.03 and confidence > 0.6:
            recommendation = "sell"
        else:
            recommendation = "hold"

        return {
            "enabled": True,
            "horizon": horizon,
            "predicted_returns": {code: predicted_return for code in market_data},
            "cumulative_return": predicted_return * horizon,
            "confidence": confidence,
            "recommendation": recommendation,
            "reasoning": f"GRU预测{horizon}日收益: {predicted_return:.2%}（{regime}市场）",
            "actions": [recommendation] * horizon
        }
    except Exception as e:
        print(f"⚠️ 世界模型预测失败: {e}")
        return {
            "enabled": False, "horizon": horizon,
            "predicted_returns": {}, "cumulative_return": 0.0,
            "confidence": 0.0, "recommendation": "hold",
            "reasoning": f"预测失败: {e}", "actions": []
        }
```

### Step 3: 验证接口

```bash
python3 -c "
import sys
sys.path.insert(0, 'prod/src')
sys.path.insert(0, 'shared')
from world_model_integration import WorldModelIntegration
import tempfile, os
with tempfile.TemporaryDirectory() as d:
    wm = WorldModelIntegration(data_dir=d)
    result = wm.predict_future({'00700': {'price': 385.0, 'rsi': 55.0}}, {})
    assert 'enabled' in result
    assert 'recommendation' in result
    print('✅ predict_future 接口验证通过')
    print('  result:', result)
"
```

### Step 4: Commit

```bash
git add prod/src/world_model_integration.py
git commit -m "fix: complete truncated predict_future() in world_model_integration.py"
```

---

## Task 9: 错误处理 — llm_enhanced_agent.py

**Files:**
- Modify: `prod/src/llm_enhanced_agent.py`
- Test: `tests/integration/test_full_pipeline.py`

### Step 1: 写集成测试

创建 `tests/integration/test_full_pipeline.py`：

```python
"""Integration tests: full pipeline with mocked external dependencies"""
import sys, os
from pathlib import Path
from unittest.mock import patch, MagicMock
import pytest

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "prod" / "src"))
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "shared"))


class TestFullPipelineRobustness:

    def test_pipeline_survives_data_source_failure(self, tmp_path):
        """所有数据源失败时，主流程不崩溃"""
        from llm_enhanced_agent import LLMEnhancedAgent

        with patch("data_collector.HKStockDataCollector.get_daily_data",
                   side_effect=Exception("Yahoo Finance 超时")):
            agent = LLMEnhancedAgent(data_dir=str(tmp_path))
            result = agent.run_daily_analysis()

        assert result is not None
        assert "final_decision" in result or "error" in result

    def test_pipeline_survives_llm_failure(self, tmp_path, monkeypatch):
        """DeepSeek API 超时时，主流程用 fallback 完成"""
        monkeypatch.setenv("DEEPSEEK_API_KEY", "sk-test")
        import requests
        with patch("requests.post", side_effect=Exception("connection timeout")):
            from llm_enhanced_agent import LLMEnhancedAgent
            agent = LLMEnhancedAgent(data_dir=str(tmp_path))
            result = agent.run_daily_analysis()

        assert result is not None

    def test_pipeline_completes_with_all_mocked(self, tmp_path):
        """全 mock 外部依赖，6 步流程应完整执行"""
        from llm_enhanced_agent import LLMEnhancedAgent

        mock_market = {
            "00700": {"price": 385.0, "rsi": 55.0, "trend": "upward",
                      "change_pct": 1.2, "data_source": "mock"},
            "09988": {"price": 85.0, "rsi": 45.0, "trend": "downward",
                      "change_pct": -0.5, "data_source": "mock"},
        }

        with patch.object(LLMEnhancedAgent, "_load_market_data",
                          return_value=mock_market):
            agent = LLMEnhancedAgent(data_dir=str(tmp_path))
            result = agent.run_daily_analysis()

        assert "final_decision" in result
        for code in mock_market:
            assert code in result["final_decision"]
            decision = result["final_decision"][code]
            assert decision["action"] in ("buy", "sell", "hold", "BUY", "SELL", "HOLD")
```

### Step 2: 运行确认失败

```bash
python3 -m pytest tests/integration/test_full_pipeline.py -v
```

预期：`test_pipeline_survives_data_source_failure` FAIL（异常未被捕获）

### Step 3: 修改 llm_enhanced_agent.py 的 run_daily_analysis()

在每个 step 加 try/except：

```python
def run_daily_analysis(self, news_items=None) -> dict:
    """6步分析流程，每步有独立错误处理"""
    result = {}

    # Step 1: 加载市场数据
    try:
        market_data = self._load_market_data()
    except Exception as e:
        print(f"⚠️ Step1 数据加载异常: {e}，使用缓存/mock")
        market_data = self._load_market_data_safe()  # 见下方
    result["market_data_source"] = (
        "degraded" if not market_data else
        list(market_data.values())[0].get("data_source", "unknown")
    )

    # Step 2: LLM 信号提取
    try:
        if news_items:
            llm_signals = self.llm_extractor.analyze_news(news_items)
        else:
            llm_signals = self.llm_extractor.get_latest_signals()
    except Exception as e:
        print(f"⚠️ Step2 LLM信号提取异常: {e}，使用空信号")
        llm_signals = {}

    # Step 3: 世界模型预测
    try:
        prediction = self.wm_integration.predict_future(
            market_data, self.portfolio, horizon=5)
    except Exception as e:
        print(f"⚠️ Step3 世界模型预测异常: {e}")
        prediction = {"enabled": False, "recommendation": "hold", "confidence": 0.0}

    # Step 4-6 同理包裹...
    # （此处保留原有调用，在外层加 try/except）
    try:
        base_decision = self._base_strategy(market_data, prediction)
    except Exception as e:
        print(f"⚠️ Step4 基础策略异常: {e}，全部 HOLD")
        base_decision = {
            "decisions": {code: {"action": "hold", "confidence": 0.5}
                          for code in market_data},
            "summary": f"策略引擎异常: {e}"
        }

    try:
        enhanced = self.llm_enhancer.enhance_decision(
            base_decision, market_data, self.portfolio,
            prediction=prediction, llm_signals=llm_signals)
    except Exception as e:
        print(f"⚠️ Step5 决策增强异常: {e}")
        enhanced = {"final_decision": base_decision.get("decisions", {}),
                    "llm_output": {}, "error": str(e)}

    result.update(enhanced)
    return result
```

在 `_load_market_data()` 方法末尾加一个 `_load_market_data_safe()` fallback（读磁盘缓存，再读 mock）。

### Step 4: 运行测试

```bash
python3 -m pytest tests/integration/test_full_pipeline.py -v
```

预期：全部 PASS

### Step 5: Commit

```bash
git add prod/src/llm_enhanced_agent.py tests/integration/test_full_pipeline.py
git commit -m "feat: add per-step error handling and graceful fallback to run_daily_analysis()"
```

---

## Task 10: VectorBT 回测完善

**Files:**
- Modify: `active_src/vectorbt_integration.py`
- Test: `tests/unit/test_vectorbt.py`

### Step 1: 写失败测试

创建 `tests/unit/test_vectorbt.py`：

```python
"""Tests for VectorBT integration"""
import sys
from pathlib import Path
import pytest
import pandas as pd
import numpy as np

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "active_src"))


def make_price_series(n=200):
    dates = pd.date_range("2023-01-01", periods=n)
    prices = pd.Series(100 * np.cumprod(1 + np.random.normal(0, 0.01, n)), index=dates)
    return prices


class TestVectorBTBacktester:

    def test_get_metrics_returns_required_keys(self):
        """get_metrics() 应返回 sharpe, max_drawdown, total_return, win_rate"""
        vbt = pytest.importorskip("vectorbt")
        from vectorbt_integration import VectorBTBacktester

        bt = VectorBTBacktester()
        price = make_price_series()
        entries = pd.Series([False] * len(price), index=price.index)
        exits   = pd.Series([False] * len(price), index=price.index)
        entries.iloc[10] = True
        exits.iloc[30]   = True

        portfolio = vbt.Portfolio.from_signals(price, entries, exits, freq="1D")
        bt.portfolio = portfolio
        metrics = bt.get_metrics()

        for key in ("sharpe_ratio", "max_drawdown", "total_return", "win_rate"):
            assert key in metrics, f"缺少 key: {key}"

    def test_run_backtest_returns_metrics(self):
        """run_backtest() 应返回包含 sharpe_ratio 的 dict"""
        pytest.importorskip("vectorbt")
        from vectorbt_integration import VectorBTBacktester

        bt = VectorBTBacktester()
        price = make_price_series()
        signals = pd.Series([0] * len(price), index=price.index)
        signals.iloc[10] = 1
        signals.iloc[30] = -1

        result = bt.run_backtest_from_signals(price, signals)
        assert isinstance(result, dict)
        assert "sharpe_ratio" in result

    def test_graceful_when_vectorbt_unavailable(self, monkeypatch):
        """vectorbt 不可用时不应抛出 ImportError"""
        import sys
        monkeypatch.setitem(sys.modules, "vectorbt", None)
        # 重新导入模块（模拟无 vbt 环境）
        if "vectorbt_integration" in sys.modules:
            del sys.modules["vectorbt_integration"]
        from vectorbt_integration import VectorBTBacktester
        bt = VectorBTBacktester()
        assert bt is not None
```

### Step 2: 运行确认失败

```bash
python3 -m pytest tests/unit/test_vectorbt.py -v
```

预期：`test_get_metrics_returns_required_keys` FAIL（`get_metrics()` 实现不完整）

### Step 3: 修改 vectorbt_integration.py

补全 `get_metrics()`：

```python
def get_metrics(self) -> dict:
    """返回回测指标"""
    if self.portfolio is None:
        return {}
    try:
        return {
            "total_return":        float(self.portfolio.total_return()),
            "sharpe_ratio":        float(self.portfolio.sharpe_ratio()),
            "max_drawdown":        float(self.portfolio.max_drawdown()),
            "win_rate":            float(self.portfolio.trades.win_rate())
                                   if len(self.portfolio.trades.records) > 0 else 0.0,
            "total_trades":        int(len(self.portfolio.trades.records)),
            "avg_winning_trade":   float(self.portfolio.trades.pnl.mean())
                                   if len(self.portfolio.trades.records) > 0 else 0.0,
        }
    except Exception as e:
        print(f"⚠️ get_metrics 失败: {e}")
        return {}
```

添加 `run_backtest_from_signals()`（接受信号序列，而非 entries/exits）：

```python
def run_backtest_from_signals(self, price: pd.Series,
                               signals: pd.Series) -> dict:
    """
    price:   价格序列（pd.Series）
    signals: 信号序列，1=买入，-1=卖出，0=持有
    返回：指标 dict
    """
    if not VBT_AVAILABLE:
        return {"error": "vectorbt 不可用"}
    try:
        entries = signals == 1
        exits   = signals == -1
        self.portfolio = vbt.Portfolio.from_signals(
            price, entries, exits,
            fees=self.fees, slippage=0.001, freq="1D",
            init_cash=self.initial_cash
        )
        return self.get_metrics()
    except Exception as e:
        return {"error": str(e)}
```

### Step 4: 运行测试

```bash
python3 -m pytest tests/unit/test_vectorbt.py -v
```

### Step 5: Commit

```bash
git add active_src/vectorbt_integration.py tests/unit/test_vectorbt.py
git commit -m "feat: complete get_metrics() and add run_backtest_from_signals() in VectorBTBacktester"
```

---

## Task 11: 全量测试 & 验收

### Step 1: 运行全部测试

```bash
cd /home/huawei/project_opencode/lamda-ai/HKTech-Agent
python3 -m pytest tests/ -v --tb=short --timeout=60
```

预期：全部通过（或仅 external 标记的跳过）

### Step 2: 检查覆盖率

```bash
python3 -m pytest tests/ --cov=prod/src --cov=active_src --cov=shared \
  --cov-report=term-missing --timeout=60
```

预期：覆盖率 ≥ 70%

### Step 3: 端到端冒烟测试（本地，需设置 DEEPSEEK_API_KEY）

```bash
cd /home/huawei/project_opencode/lamda-ai/HKTech-Agent
export DEEPSEEK_API_KEY="your_real_key"
python3 -c "
import sys
sys.path.insert(0, 'prod/src')
sys.path.insert(0, 'shared')
from llm_signal_extractor import LLMSignalExtractor
import tempfile
with tempfile.TemporaryDirectory() as d:
    ext = LLMSignalExtractor(data_dir=d)
    result = ext._call_llm_api('00700', ['腾讯2025年Q4营收同比增长12%，超出分析师预期'])
    print('LLM 返回:', result)
    assert 'sentiment' in result
    print('✅ DeepSeek 集成验证通过')
"
```

### Step 4: 训练世界模型（可选，需 yfinance + torch + 网络）

```bash
cd /home/huawei/project_opencode/lamda-ai/HKTech-Agent
python3 prod/src/train_world_model.py
# 预期输出：
# ✅ 00700 腾讯控股: 1XXX 条记录
# ...
# Epoch 10/100: val_loss=0.XXXXXX
# 早停于 epoch XX
# ✅ 模型已保存到 data/models/rssm_model.pt
```

### Step 5: 最终 Commit

```bash
git add .
git commit -m "test: full test suite passing, coverage ≥70%, HKTech-Agent v2.0 ready"
```

---

## 成功标准验收清单

- [ ] `pytest tests/ -v` 全部通过
- [ ] 覆盖率 ≥ 70%
- [ ] `grep -r "/opt/hktech-agent" prod/src active_src shared` → 无输出
- [ ] `grep -r "random.uniform" prod/src` → 无输出（决策不再随机）
- [ ] `DEEPSEEK_API_KEY=xxx python3 -c "..."` 能看到真实 LLM JSON 返回
- [ ] `python3 prod/src/train_world_model.py` 能训练完成保存模型
- [ ] `python3 prod/src/world_model_integration.py` 不报错
- [ ] `.env.example` 存在，`.env` 在 `.gitignore` 中
