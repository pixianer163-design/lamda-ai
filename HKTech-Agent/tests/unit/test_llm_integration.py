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
        if "llm_signal_extractor" in sys.modules:
            del sys.modules["llm_signal_extractor"]
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
        if "llm_signal_extractor" in sys.modules:
            del sys.modules["llm_signal_extractor"]
        from llm_signal_extractor import LLMSignalExtractor

        extractor = LLMSignalExtractor(data_dir=str(tmp_path))
        result = extractor._call_llm_api("00700", ["腾讯Q4营收超预期"])

        assert "sentiment" in result
        assert 0.0 <= result["sentiment"] <= 1.0

    def test_fallback_when_api_fails(self, tmp_path, monkeypatch):
        """HTTP 异常时应 fallback 到关键词匹配，不抛出"""
        monkeypatch.setenv("DEEPSEEK_API_KEY", "sk-test-key")
        if "llm_signal_extractor" in sys.modules:
            del sys.modules["llm_signal_extractor"]
        from llm_signal_extractor import LLMSignalExtractor

        with patch("requests.post", side_effect=Exception("connection refused")):
            extractor = LLMSignalExtractor(data_dir=str(tmp_path))
            result = extractor._call_llm_api("00700", ["某新闻"])

        assert "sentiment" in result
        assert 0.0 <= result["sentiment"] <= 1.0

    def test_analyze_news_returns_all_stocks(self, tmp_path, monkeypatch):
        """analyze_news 应为每只股票返回情感分数"""
        monkeypatch.delenv("DEEPSEEK_API_KEY", raising=False)
        if "llm_signal_extractor" in sys.modules:
            del sys.modules["llm_signal_extractor"]
        from llm_signal_extractor import LLMSignalExtractor

        extractor = LLMSignalExtractor(data_dir=str(tmp_path))
        news = [{"title": "港股市场整体回暖", "content": ""}]
        signals = extractor.analyze_news(news)

        assert isinstance(signals, dict)
        for code in ["00700", "09988", "03690"]:
            assert code in signals
            assert 0.0 <= signals[code] <= 1.0
