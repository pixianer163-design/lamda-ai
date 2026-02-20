"""
Tests requiring torch to be available.
Covers the GRU training loop and GRU-based prediction path.
These tests are skipped if torch is not installed.
"""
import sys
import os
import pytest
import numpy as np
from pathlib import Path

project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))
sys.path.insert(0, str(project_root / "prod" / "src"))

torch = pytest.importorskip("torch")


class TestGRUWorldModelTorch:
    """Tests for GRUWorldModel requiring torch."""

    def test_gru_forward_shape(self):
        """GRUWorldModel.forward() should return (batch, 1)."""
        from rssm_world_model import GRUWorldModel
        model = GRUWorldModel(input_size=8, hidden_size=64, num_layers=2)
        x = torch.zeros(4, 20, 8)  # batch=4, seq=20, features=8
        out = model(x)
        assert out.shape == (4, 1), f"Expected (4,1), got {out.shape}"

    def test_gru_forward_values_finite(self):
        """GRUWorldModel output should be finite (no NaN/inf)."""
        from rssm_world_model import GRUWorldModel
        model = GRUWorldModel(input_size=8, hidden_size=64, num_layers=2)
        x = torch.randn(2, 20, 8)
        out = model(x)
        assert torch.isfinite(out).all()

    def test_gru_world_model_loaded_via_rssm(self, tmp_path):
        """RSSMWorldModel should load GRU model from models/rssm_model.pt."""
        from rssm_world_model import GRUWorldModel, RSSMWorldModel

        # Save a tiny model to the expected path
        models_dir = tmp_path / "models"
        models_dir.mkdir()
        model = GRUWorldModel(input_size=8, hidden_size=64, num_layers=2)
        torch.save(model.state_dict(), str(models_dir / "rssm_model.pt"))

        # RSSMWorldModel should pick it up
        rssm = RSSMWorldModel(data_dir=str(tmp_path))
        assert rssm.gru_model is not None

        market_data = {"00700": {"price": 385.0, "rsi": 50.0,
                                  "ma5": 382.0, "ma20": 375.0, "change_pct": 1.5}}
        result = rssm.predict(market_data)
        assert result["source"] == "gru_model"
        assert "predicted_return" in result
        assert "regime" in result

    def test_predict_with_gru_regime_values(self, tmp_path):
        """_predict_with_gru should return valid regime strings."""
        from rssm_world_model import GRUWorldModel, RSSMWorldModel
        models_dir = tmp_path / "models"
        models_dir.mkdir()
        model = GRUWorldModel(input_size=8, hidden_size=64, num_layers=2)
        torch.save(model.state_dict(), str(models_dir / "rssm_model.pt"))

        rssm = RSSMWorldModel(data_dir=str(tmp_path))
        result = rssm.predict({"00700": {"price": 300.0}})
        assert result["regime"] in ("bullish", "bearish", "neutral")
        assert -1.0 <= result["predicted_return"] <= 1.0
        assert result["confidence"] == 0.7


class TestTrainGRUModelTorch:
    """Tests for train_gru_model() requiring torch."""

    def test_train_gru_model_saves_file(self, tmp_path):
        """train_gru_model() should save rssm_model.pt to models_dir."""
        from train_world_model import train_gru_model

        # Small dataset: 50 samples, seq_len=20, 8 features
        X = np.random.randn(50, 20, 8).astype(np.float32)
        y = np.random.randn(50).astype(np.float32)

        result = train_gru_model(X, y, str(tmp_path), epochs=2, patience=10)
        assert result != "", "Expected non-empty model path"
        assert os.path.exists(result), f"Model file not found: {result}"
        assert result.endswith("rssm_model.pt")

    def test_train_gru_model_creates_scaler(self, tmp_path):
        """train_gru_model() should save scaler.pkl alongside model."""
        from train_world_model import train_gru_model
        import pickle

        X = np.random.randn(50, 20, 8).astype(np.float32)
        y = np.random.randn(50).astype(np.float32)

        train_gru_model(X, y, str(tmp_path), epochs=2, patience=10)
        scaler_path = os.path.join(str(tmp_path), "models", "scaler.pkl")
        assert os.path.exists(scaler_path)
        with open(scaler_path, "rb") as f:
            scaler = pickle.load(f)
        assert "y_mean" in scaler
        assert "y_std" in scaler

    def test_train_gru_model_early_stopping(self, tmp_path):
        """train_gru_model() should respect patience and stop early."""
        from train_world_model import train_gru_model

        X = np.random.randn(30, 20, 8).astype(np.float32)
        y = np.random.randn(30).astype(np.float32)

        # patience=1 → should stop after 1 non-improving epoch
        result = train_gru_model(X, y, str(tmp_path), epochs=20, patience=1)
        assert result != ""
