"""Tests for shared/config.py"""
import os
from pathlib import Path
import pytest


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
    # default data_dir should be HKTech-Agent/data
    assert cfg.data_dir.name == "data"
    assert "HKTech-Agent" in str(cfg.data_dir)


def test_get_config_env_override_data_dir(monkeypatch, tmp_path):
    from config import get_config
    monkeypatch.setenv("DATA_DIR", str(tmp_path))
    cfg = get_config()
    assert cfg.data_dir == tmp_path


def test_no_hardcoded_opt_paths_in_source():
    """No source files should contain /opt/hktech-agent hardcoded paths"""
    import subprocess
    result = subprocess.run(
        ["grep", "-r", "/opt/hktech-agent",
         "prod/src/", "active_src/", "shared/",
         "--include=*.py", "-l"],
        capture_output=True, text=True,
        cwd="/home/huawei/project_opencode/lamda-ai/HKTech-Agent"
    )
    files_with_hardcoded = result.stdout.strip()
    assert files_with_hardcoded == "", f"Files with hardcoded /opt/hktech-agent:\n{files_with_hardcoded}"
