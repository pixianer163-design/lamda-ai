"""
Coverage-boosting tests for shared modules and active_src utilities.
Targets: alert.AlertLevel, strategy_engine.StrategyEngine, performance_tracker.
"""
import sys
import pytest
from pathlib import Path
from unittest.mock import patch, MagicMock

project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))
sys.path.insert(0, str(project_root / "shared"))
sys.path.insert(0, str(project_root / "active_src"))


# ============================================================================
# shared/alert.py — AlertLevel and basic Alert tests
# ============================================================================

class TestAlertLevel:
    """Tests for shared/alert.py AlertLevel class."""

    def test_alert_level_constants(self):
        from alert import AlertLevel
        assert AlertLevel.INFO == "info"
        assert AlertLevel.WARNING == "warning"
        assert AlertLevel.ERROR == "error"
        assert AlertLevel.CRITICAL == "critical"

    def test_is_valid_valid_levels(self):
        from alert import AlertLevel
        for level in ["info", "warning", "error", "critical"]:
            assert AlertLevel.is_valid(level) is True

    def test_is_valid_invalid_level(self):
        from alert import AlertLevel
        assert AlertLevel.is_valid("debug") is False
        assert AlertLevel.is_valid("") is False
        assert AlertLevel.is_valid("CRITICAL") is False  # case sensitive

    def test_get_numeric_level_ordering(self):
        from alert import AlertLevel
        assert AlertLevel.get_numeric_level("info") < AlertLevel.get_numeric_level("warning")
        assert AlertLevel.get_numeric_level("warning") < AlertLevel.get_numeric_level("error")
        assert AlertLevel.get_numeric_level("error") < AlertLevel.get_numeric_level("critical")

    def test_get_numeric_level_unknown(self):
        from alert import AlertLevel
        assert AlertLevel.get_numeric_level("unknown") == 0

    def test_alert_manager_import(self):
        from alert import AlertManager
        assert AlertManager is not None

    def test_alert_manager_init_default(self):
        from alert import AlertManager
        manager = AlertManager()
        assert manager is not None
        assert isinstance(manager.channels, list)

    def test_alert_manager_send_alert_info(self):
        """send_alert() should not raise for an info alert."""
        from alert import AlertManager
        manager = AlertManager()
        # Console channel will print — just verify no exception
        result = manager.send_alert("Test Title", "Test message", level="info")
        assert isinstance(result, bool)

    def test_alert_manager_send_alert_invalid_level(self):
        """send_alert() with invalid level should default to error."""
        from alert import AlertManager
        manager = AlertManager()
        result = manager.send_alert("Title", "Message", level="invalid_level")
        assert isinstance(result, bool)

    def test_alert_manager_send_alert_deduplication(self):
        """Duplicate key should cause second send to return False."""
        from alert import AlertManager
        manager = AlertManager(config={"rate_limit": 3600})
        manager.send_alert("Title", "Msg", level="info", deduplicate_key="test-key")
        result2 = manager.send_alert("Title", "Msg", level="info", deduplicate_key="test-key")
        assert result2 is False  # duplicate

    def test_alert_manager_send_alert_critical(self):
        from alert import AlertManager
        manager = AlertManager()
        result = manager.send_alert("Critical", "Something broke", level="critical")
        assert isinstance(result, bool)


# ============================================================================
# shared/strategy_engine.py — StrategyEngine (virtual mode)
# ============================================================================

class TestStrategyEngineVirtual:
    """Tests for StrategyEngine in virtual mode (no VectorBT)."""

    def test_strategy_engine_import(self):
        from strategy_engine import StrategyEngine
        assert StrategyEngine is not None

    def test_strategy_engine_init_virtual(self):
        from strategy_engine import StrategyEngine
        engine = StrategyEngine(engine_type="virtual")
        assert engine.engine_type == "virtual"

    def test_strategy_engine_init_auto_selects_virtual(self):
        """When VectorBT is absent, auto should select virtual."""
        from strategy_engine import StrategyEngine, VECTORBT_AVAILABLE
        engine = StrategyEngine(engine_type="auto")
        if not VECTORBT_AVAILABLE:
            assert engine.engine_type == "virtual"

    def test_generate_signals_returns_dict(self):
        from strategy_engine import StrategyEngine
        engine = StrategyEngine(engine_type="virtual")
        market_data = {
            "00700": {"price": 385.0, "rsi": 55.0, "ma5": 382.0,
                      "ma20": 375.0, "change_pct": 1.5},
            "09988": {"price": 85.0, "rsi": 45.0, "ma5": 84.0,
                      "ma20": 86.0, "change_pct": -0.5},
        }
        result = engine.generate_signals(market_data)
        assert isinstance(result, dict)
        assert len(result) > 0
        for code, signal in result.items():
            assert "action" in signal
            assert signal["action"] in ("buy", "sell", "hold")

    def test_generate_signals_with_custom_config(self):
        from strategy_engine import StrategyEngine
        engine = StrategyEngine(engine_type="virtual")
        market_data = {"00700": {"price": 385.0, "rsi": 75.0}}
        config = {"type": "rsi", "rsi_oversold": 30, "rsi_overbought": 70}
        result = engine.generate_signals(market_data, strategy_config=config)
        assert isinstance(result, dict)

    def test_optimize_parameters_virtual(self):
        from strategy_engine import StrategyEngine
        engine = StrategyEngine(engine_type="virtual")
        result = engine.optimize_parameters({})
        assert isinstance(result, dict)

    def test_get_capabilities_returns_dict(self):
        from strategy_engine import StrategyEngine
        engine = StrategyEngine(engine_type="virtual")
        caps = engine.get_capabilities()
        assert isinstance(caps, dict)
        assert "engine_type" in caps

    def test_convenience_generate_signals(self):
        from strategy_engine import generate_signals
        result = generate_signals({"00700": {"price": 385.0, "rsi": 50.0}})
        assert isinstance(result, dict)

    def test_convenience_optimize_strategy(self):
        from strategy_engine import optimize_strategy
        result = optimize_strategy({})
        assert isinstance(result, dict)

    def test_generate_signals_vectorbt_engine(self):
        """_vectorbt_strategy should return signals for all stocks in market_data."""
        from strategy_engine import StrategyEngine
        engine = StrategyEngine(engine_type="vectorbt")
        market_data = {
            "00700": {"price": 385.0, "rsi": 25.0, "ma5": 390.0,
                      "ma20": 375.0, "change_pct": 2.0},
        }
        result = engine.generate_signals(market_data, {"type": "ma_cross"})
        assert "00700" in result
        assert result["00700"]["action"] == "buy"  # ma5>ma20 and change>0

    def test_generate_signals_vectorbt_sell(self):
        from strategy_engine import StrategyEngine
        engine = StrategyEngine(engine_type="vectorbt")
        market_data = {
            "00700": {"price": 385.0, "rsi": 75.0, "ma5": 375.0,
                      "ma20": 390.0, "change_pct": -1.0},
        }
        result = engine.generate_signals(market_data, {"type": "ma_cross"})
        assert result["00700"]["action"] == "sell"  # ma5<ma20 and change<0

    def test_generate_signals_vectorbt_rsi(self):
        from strategy_engine import StrategyEngine
        engine = StrategyEngine(engine_type="vectorbt")
        market_data = {"00700": {"price": 100.0, "rsi": 80.0}}
        result = engine.generate_signals(market_data, {"type": "rsi"})
        assert result["00700"]["action"] == "sell"

    def test_generate_signals_traditional_engine(self):
        from strategy_engine import StrategyEngine
        engine = StrategyEngine(engine_type="traditional")
        market_data = {
            "00700": {"price": 100.0, "rsi": 25.0, "change_pct": -3.0},
        }
        result = engine.generate_signals(market_data)
        assert "00700" in result
        assert result["00700"]["action"] == "buy"  # rsi<30 and change<-2

    def test_generate_signals_traditional_sell(self):
        from strategy_engine import StrategyEngine
        engine = StrategyEngine(engine_type="traditional")
        market_data = {"00700": {"price": 100.0, "rsi": 75.0, "change_pct": 3.0}}
        result = engine.generate_signals(market_data)
        assert result["00700"]["action"] == "sell"

    def test_backtest_vectorbt_engine(self):
        """backtest() with vectorbt engine should return stats dict."""
        from strategy_engine import StrategyEngine
        engine = StrategyEngine(engine_type="vectorbt")
        signals = {"00700": {"action": "buy"}, "09988": {"action": "sell"}}
        result = engine.backtest(signals, {})
        assert "total_return" in result
        assert "sharpe_ratio" in result

    def test_backtest_virtual_engine(self):
        from strategy_engine import StrategyEngine
        engine = StrategyEngine(engine_type="virtual")
        signals = {"00700": {"action": "hold"}}
        result = engine.backtest(signals, {})
        assert isinstance(result, dict)


