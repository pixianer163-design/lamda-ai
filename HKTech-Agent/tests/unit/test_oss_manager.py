"""
Tests for OSSManager CSV credential loading.
Does NOT require oss2 or network access.
"""
import sys
import csv
import pytest
import tempfile
import os
from pathlib import Path
from unittest.mock import patch, MagicMock

project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root / "active_src"))


def _make_csv(tmp_path, key_id="TESTID123", key_secret="TESTSECRET456"):
    """Helper: write a valid AccessKey.csv to tmp_path."""
    csv_file = tmp_path / "AccessKey.csv"
    with open(csv_file, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=["AccessKey ID", "AccessKey Secret"])
        writer.writeheader()
        writer.writerow({"AccessKey ID": key_id, "AccessKey Secret": key_secret})
    return str(csv_file)


def _make_mock_oss2():
    """Create a fully-mocked oss2 module."""
    mock_oss2 = MagicMock()
    mock_oss2.Auth.return_value = MagicMock()
    mock_bucket = MagicMock()
    mock_oss2.Bucket.return_value = mock_bucket
    mock_bucket.get_bucket_info.return_value = MagicMock()
    return mock_oss2


class TestOSSManagerCSV:
    """OSSManager._load_from_csv() tests — no oss2 / network needed."""

    def setup_method(self):
        """Remove cached oss_manager module before each test."""
        if "oss_manager" in sys.modules:
            del sys.modules["oss_manager"]

    def test_load_from_csv_sets_credentials(self, tmp_path):
        csv_path = _make_csv(tmp_path)
        mock_oss2 = _make_mock_oss2()
        with patch.dict(sys.modules, {"oss2": mock_oss2}):
            from oss_manager import OSSManager
            mgr = OSSManager(csv_path=csv_path)

        assert mgr.access_key_id == "TESTID123"
        assert mgr.access_key_secret == "TESTSECRET456"

    def test_load_from_csv_missing_file_warns(self, tmp_path):
        mock_oss2 = _make_mock_oss2()
        with patch.dict(sys.modules, {"oss2": mock_oss2}):
            # Set env vars so validation passes even without CSV
            with patch.dict(os.environ, {
                "ALIYUN_ACCESS_KEY_ID": "ENV_ID",
                "ALIYUN_ACCESS_KEY_SECRET": "ENV_SECRET",
            }):
                from oss_manager import OSSManager
                # should not raise
                mgr = OSSManager(csv_path=str(tmp_path / "nonexistent.csv"))
        assert mgr.access_key_id == "ENV_ID"  # fell back to env var

    def test_load_from_csv_takes_priority_over_config_file(self, tmp_path):
        """CSV credentials take priority over .conf file."""
        csv_path = _make_csv(tmp_path, key_id="CSV_ID", key_secret="CSV_SECRET")
        conf_path = tmp_path / "oss.conf"
        conf_path.write_text("access_key_id = CONF_ID\naccess_key_secret = CONF_SECRET\n"
                              "oss_endpoint = oss-cn-beijing.aliyuncs.com\n"
                              "oss_bucket = hktech-agent-models\n")

        mock_oss2 = _make_mock_oss2()
        with patch.dict(sys.modules, {"oss2": mock_oss2}):
            from oss_manager import OSSManager
            mgr = OSSManager(config_path=str(conf_path), csv_path=csv_path)

        assert mgr.access_key_id == "CSV_ID"
