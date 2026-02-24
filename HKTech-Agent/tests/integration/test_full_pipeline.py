"""Integration tests: full pipeline robustness with mocked external dependencies"""
import sys, os
from pathlib import Path
from unittest.mock import patch, MagicMock
import pytest

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "prod" / "src"))
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "shared"))


class TestFullPipelineRobustness:

    def test_pipeline_survives_data_source_failure(self, tmp_path):
        """所有数据源失败时，主流程不崩溃，返回包含 final_decision 或 error 的 dict"""
        if "llm_enhanced_agent" in sys.modules:
            del sys.modules["llm_enhanced_agent"]
        from llm_enhanced_agent import LLMEnhancedAgent

        with patch("data_collector.HKStockDataCollector.get_daily_data",
                   side_effect=Exception("Yahoo Finance 超时")):
            agent = LLMEnhancedAgent(data_dir=str(tmp_path))
            result = agent.run_daily_analysis()

        assert result is not None
        assert isinstance(result, dict)
        assert "final_decision" in result or "error" in result

    def test_pipeline_survives_llm_failure(self, tmp_path, monkeypatch):
        """DeepSeek API 超时时，主流程用 fallback 完成，不崩溃"""
        monkeypatch.setenv("DEEPSEEK_API_KEY", "sk-test")
        if "llm_enhanced_agent" in sys.modules:
            del sys.modules["llm_enhanced_agent"]

        with patch("requests.post", side_effect=Exception("connection timeout")):
            from llm_enhanced_agent import LLMEnhancedAgent
            agent = LLMEnhancedAgent(data_dir=str(tmp_path))
            result = agent.run_daily_analysis()

        assert result is not None
        assert isinstance(result, dict)

    def test_pipeline_completes_with_all_mocked(self, tmp_path):
        """全 mock 外部依赖，流程应完整执行并返回 final_decision"""
        if "llm_enhanced_agent" in sys.modules:
            del sys.modules["llm_enhanced_agent"]
        from llm_enhanced_agent import LLMEnhancedAgent

        mock_market = {
            "00700": {"price": 385.0, "rsi": 55.0, "trend": "upward",
                      "change_pct": 1.2, "data_source": "mock",
                      "ma5": 383.0, "ma20": 378.0, "volume": 1e7},
            "09988": {"price": 85.0, "rsi": 45.0, "trend": "downward",
                      "change_pct": -0.5, "data_source": "mock",
                      "ma5": 86.0, "ma20": 87.0, "volume": 5e6},
        }

        with patch.object(LLMEnhancedAgent, "_load_market_data", return_value=mock_market):
            agent = LLMEnhancedAgent(data_dir=str(tmp_path))
            result = agent.run_daily_analysis()

        assert "final_decision" in result, f"Missing final_decision in {list(result.keys())}"
        for code in mock_market:
            assert code in result["final_decision"], f"Missing {code} in final_decision"
            decision = result["final_decision"][code]
            assert decision.get("action", "").upper() in ("BUY", "SELL", "HOLD"), \
                f"Invalid action: {decision.get('action')}"
