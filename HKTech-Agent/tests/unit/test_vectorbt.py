"""Tests for VectorBT integration"""
import sys
from pathlib import Path
import pytest
import pandas as pd
import numpy as np

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "active_src"))


def make_price_series(n=200):
    np.random.seed(42)
    dates = pd.date_range("2023-01-01", periods=n, freq="B")
    prices = pd.Series(
        100 * np.cumprod(1 + np.random.normal(0, 0.01, n)),
        index=dates, name="Close"
    )
    return prices


class TestVectorBTBacktester:

    def test_get_metrics_returns_required_keys(self):
        """get_metrics() 应返回 sharpe_ratio, max_drawdown, total_return, win_rate"""
        vbt = pytest.importorskip("vectorbt")
        from vectorbt_integration import VectorBTBacktester

        bt = VectorBTBacktester()
        price = make_price_series()
        entries = pd.Series(False, index=price.index)
        exits   = pd.Series(False, index=price.index)
        entries.iloc[10] = True
        exits.iloc[30]   = True

        bt.portfolio = vbt.Portfolio.from_signals(price, entries, exits, freq="1D")
        metrics = bt.get_metrics()

        for key in ("total_return", "sharpe_ratio", "max_drawdown", "win_rate",
                    "total_trades", "avg_winning_trade"):
            assert key in metrics, f"缺少 key: {key}"

    def test_run_backtest_returns_metrics(self):
        """run_backtest_from_signals() 应返回含 sharpe_ratio 的 dict"""
        pytest.importorskip("vectorbt")
        from vectorbt_integration import VectorBTBacktester

        bt = VectorBTBacktester()
        price = make_price_series()
        signals = pd.Series(0, index=price.index)
        signals.iloc[10] = 1   # buy
        signals.iloc[30] = -1  # sell

        result = bt.run_backtest_from_signals(price, signals)
        assert isinstance(result, dict)
        assert "sharpe_ratio" in result

    def test_graceful_when_vectorbt_unavailable(self, monkeypatch):
        """vectorbt 不可用时不应抛出 ImportError"""
        import sys
        # Simulate missing vectorbt
        monkeypatch.setitem(sys.modules, "vectorbt", None)
        if "vectorbt_integration" in sys.modules:
            del sys.modules["vectorbt_integration"]
        from vectorbt_integration import VectorBTBacktester
        bt = VectorBTBacktester()
        assert bt is not None

    def test_get_metrics_returns_empty_when_no_portfolio(self):
        """没有 portfolio 时 get_metrics() 应返回空 dict"""
        from vectorbt_integration import VectorBTBacktester
        bt = VectorBTBacktester()
        assert bt.get_metrics() == {}
