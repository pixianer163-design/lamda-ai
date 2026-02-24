"""Tests for GRU world model"""
import sys
from pathlib import Path
import pytest
import numpy as np

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "prod" / "src"))


class TestGRUWorldModel:

    def test_gru_model_architecture_importable(self):
        """GRUWorldModel 类应可导入"""
        from rssm_world_model import GRUWorldModel
        assert GRUWorldModel is not None

    def test_gru_model_forward_shape(self):
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

    def test_model_predict_returns_correct_keys(self, tmp_path):
        """RSSMWorldModel.predict() 应返回 predicted_return, confidence, regime"""
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

        model = RSSMWorldModel(data_dir=str(tmp_path))
        market_data = {"00700": {"price": 385.0, "rsi": 75.0,
                                  "ma5": 390.0, "ma20": 380.0,
                                  "change_pct": 2.0, "volume": 1e7}}
        result = model.predict(market_data)

        assert result is not None
        assert "predicted_return" in result
        # RSI 超买时 fallback 不应返回强烈买入信号
        assert result["predicted_return"] < 0.1

    def test_technical_fallback_deterministic(self, tmp_path):
        """技术指标 fallback 应是确定性的（无随机数）"""
        from rssm_world_model import RSSMWorldModel

        model = RSSMWorldModel(data_dir=str(tmp_path))
        market_data = {"00700": {"price": 385.0, "rsi": 40.0,
                                  "ma5": 380.0, "ma20": 375.0,
                                  "change_pct": 0.5, "volume": 1e7}}
        results = [model.predict(market_data) for _ in range(3)]
        returns = [r["predicted_return"] for r in results]
        assert len(set(returns)) == 1, f"Fallback should be deterministic: {returns}"
