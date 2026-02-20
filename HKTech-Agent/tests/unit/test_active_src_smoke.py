"""
Smoke tests for active_src modules (feishu_sender, daily_report_sender).
These files were pre-existing but need basic coverage.
"""
import sys
import importlib
import importlib.util
import json
import pytest
from pathlib import Path
from unittest.mock import patch, mock_open, MagicMock

project_root = Path(__file__).parent.parent.parent
# active_src must come BEFORE prod/src to avoid shadowing by same-named modules
sys.path.insert(0, str(project_root / "prod" / "src"))
sys.path.insert(0, str(project_root / "active_src"))


def _load_active_src_module(name: str):
    """Load a module directly from active_src, bypassing sys.path ambiguity."""
    spec = importlib.util.spec_from_file_location(
        f"active_src.{name}",
        str(project_root / "active_src" / f"{name}.py")
    )
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


# ============================================================================
# active_src/feishu_sender.py
# ============================================================================

FAKE_CONFIG = json.dumps({
    "app_id": "test_app",
    "app_secret": "test_secret",
    "chat_id": "test_chat",
    "webhook_url": "https://example.com/hook"
})


class TestFeishuSender:

    def _make_sender(self):
        with patch("builtins.open", mock_open(read_data=FAKE_CONFIG)):
            from feishu_sender import FeishuSender
            if "feishu_sender" in sys.modules:
                del sys.modules["feishu_sender"]
            from feishu_sender import FeishuSender
            return FeishuSender.__new__(FeishuSender), FeishuSender

    def test_import_succeeds(self):
        with patch("builtins.open", mock_open(read_data=FAKE_CONFIG)):
            if "feishu_sender" in sys.modules:
                del sys.modules["feishu_sender"]
            from feishu_sender import FeishuSender
            assert FeishuSender is not None

    def test_init_loads_config(self):
        with patch("builtins.open", mock_open(read_data=FAKE_CONFIG)):
            if "feishu_sender" in sys.modules:
                del sys.modules["feishu_sender"]
            from feishu_sender import FeishuSender
            sender = FeishuSender(config_path="/fake/path.json")
            assert sender.app_id == "test_app"
            assert sender.webhook_url == "https://example.com/hook"

    def test_get_token_returns_none_without_credentials(self):
        with patch("builtins.open", mock_open(read_data=json.dumps({}))):
            if "feishu_sender" in sys.modules:
                del sys.modules["feishu_sender"]
            from feishu_sender import FeishuSender
            sender = FeishuSender(config_path="/fake/path.json")
            token = sender._get_token()
            assert token is None

    def test_get_token_returns_none_on_network_error(self):
        with patch("builtins.open", mock_open(read_data=FAKE_CONFIG)):
            if "feishu_sender" in sys.modules:
                del sys.modules["feishu_sender"]
            from feishu_sender import FeishuSender
            with patch("requests.post", side_effect=Exception("network error")):
                sender = FeishuSender(config_path="/fake/path.json")
                token = sender._get_token()
            assert token is None

    def test_send_by_webhook_returns_false_without_url(self):
        with patch("builtins.open", mock_open(read_data=json.dumps({}))):
            if "feishu_sender" in sys.modules:
                del sys.modules["feishu_sender"]
            from feishu_sender import FeishuSender
            sender = FeishuSender(config_path="/fake/path.json")
            result = sender.send_by_webhook("test message")
            assert result is False

    def test_send_by_api_returns_false_without_token(self):
        with patch("builtins.open", mock_open(read_data=json.dumps({}))):
            if "feishu_sender" in sys.modules:
                del sys.modules["feishu_sender"]
            from feishu_sender import FeishuSender
            sender = FeishuSender(config_path="/fake/path.json")
            result = sender.send_by_api("test message")
            assert result is False


# ============================================================================
# active_src/daily_report_sender.py (FeishuCardSender variant)
# ============================================================================

class TestActiveSrcDailyReport:
    """Tests specifically targeting active_src/daily_report_sender.py."""

    def _make_sender(self, tmp_path):
        """Load FeishuCardSender directly from active_src to avoid import shadowing."""
        config_file = tmp_path / "feishu_config.json"
        config_file.write_text(FAKE_CONFIG)
        mod = _load_active_src_module("daily_report_sender")
        return mod.FeishuCardSender(config_path=str(config_file))

    def test_import_succeeds(self):
        mod = _load_active_src_module("daily_report_sender")
        assert hasattr(mod, "FeishuCardSender")

    def test_card_sender_init_with_real_tmp_config(self, tmp_path):
        """Init with a real config file in tmp_path."""
        sender = self._make_sender(tmp_path)
        assert sender.app_id == "test_app"
        assert sender.chat_id == "test_chat"

    def test_card_sender_get_token_no_credentials(self, tmp_path):
        mod = _load_active_src_module("daily_report_sender")
        cfg = tmp_path / "cfg.json"
        cfg.write_text("{}")
        sender = mod.FeishuCardSender(config_path=str(cfg))
        assert sender._get_token() is None

    def test_card_sender_get_token_network_error(self, tmp_path):
        sender = self._make_sender(tmp_path)
        with patch("requests.post", side_effect=Exception("network error")):
            token = sender._get_token()
        assert token is None

    def test_card_sender_send_card_no_token(self, tmp_path):
        mod = _load_active_src_module("daily_report_sender")
        cfg = tmp_path / "cfg.json"
        cfg.write_text("{}")
        sender = mod.FeishuCardSender(config_path=str(cfg))
        result = sender._send_card({"test": "data"})
        assert result is False

    def test_send_daily_report_card_builds_card(self, tmp_path):
        """send_daily_report_card should call _send_card with a structured card dict."""
        sender = self._make_sender(tmp_path)
        captured = {}

        def mock_send(card):
            captured["card"] = card
            return True

        sender._send_card = mock_send

        portfolio = {"cash": 5000, "holdings": {"00700": {"shares": 10, "cost_basis": 4000}}}
        market_data = {"00700": {"price": 385.0, "change_pct": 1.5}}
        result = sender.send_daily_report_card(42, portfolio, market_data)

        assert result is True
        assert "card" in captured
        assert "elements" in captured["card"]
        assert captured["card"]["header"]["title"]["content"].startswith("🌅")

    def test_send_daily_report_card_empty_portfolio(self, tmp_path):
        """send_daily_report_card with empty portfolio should not raise."""
        sender = self._make_sender(tmp_path)
        sender._send_card = lambda c: False
        result = sender.send_daily_report_card(1, {}, {})
        assert result is False  # returns _send_card result

    def test_send_daily_report_card_with_trades(self, tmp_path):
        """send_daily_report_card with trades list should not raise."""
        sender = self._make_sender(tmp_path)
        sender._send_card = lambda c: True
        trades = [{"action": "buy", "code": "00700", "shares": 5}]
        result = sender.send_daily_report_card(5, {}, {}, trades=trades)
        assert result is True
