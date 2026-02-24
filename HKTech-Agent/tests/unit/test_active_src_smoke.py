"""
Smoke tests for active_src modules (feishu_sender).
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


