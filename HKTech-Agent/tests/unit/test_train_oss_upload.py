"""
Unit tests for _upload_to_oss_after_training() in train_world_model.py.
Verifies graceful skip when oss2 / OSSManager is unavailable.
"""
import sys
import os
import pytest
from pathlib import Path
from unittest.mock import patch, MagicMock

project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root / "prod" / "src"))
sys.path.insert(0, str(project_root / "shared"))


class TestUploadToOssAfterTraining:

    def setup_method(self):
        # clear cached module between tests
        for mod in list(sys.modules.keys()):
            if "train_world_model" in mod:
                del sys.modules[mod]

    def _get_fn(self):
        import train_world_model
        return train_world_model._upload_to_oss_after_training

    def test_skips_gracefully_when_oss2_not_installed(self, tmp_path, capsys):
        """When oss2 is missing, function prints warning and returns without raising."""
        fn = self._get_fn()
        # Simulate oss2 import failure inside OSSManager
        with patch.dict(sys.modules, {"oss2": None}):
            fn(str(tmp_path), str(tmp_path / "models" / "rssm_model.pt"))
        out = capsys.readouterr().out
        # Should print either "oss2 未安装" or "OSS初始化失败" — not raise
        assert "跳过" in out or "失败" in out

    def test_skips_gracefully_when_oss_manager_import_fails(self, tmp_path, capsys):
        """When oss_manager module is not importable, prints warning and returns."""
        fn = self._get_fn()
        original_import = __builtins__.__import__ if hasattr(__builtins__, '__import__') else __import__

        def fake_import(name, *args, **kwargs):
            if name == "oss_manager":
                raise ImportError("mocked missing oss_manager")
            return original_import(name, *args, **kwargs)

        with patch("builtins.__import__", side_effect=fake_import):
            fn(str(tmp_path), str(tmp_path / "rssm_model.pt"))
        out = capsys.readouterr().out
        assert "跳过" in out

    def test_uploads_model_and_data_when_oss_available(self, tmp_path, capsys):
        """When OSSManager is available, uploads rssm_model.pt, scaler.pkl, and training_episodes.json."""
        # Create fake files
        models_dir = tmp_path / "models"
        models_dir.mkdir()
        (models_dir / "rssm_model.pt").write_bytes(b"fake model")
        (models_dir / "scaler.pkl").write_bytes(b"fake scaler")
        (tmp_path / "training_episodes.json").write_text('{"episodes": []}')

        mock_oss = MagicMock()
        mock_oss_class = MagicMock(return_value=mock_oss)

        fn = self._get_fn()
        with patch("oss_manager.OSSManager", mock_oss_class, create=True):
            # patch the import inside the function
            mock_module = MagicMock()
            mock_module.OSSManager = mock_oss_class
            with patch.dict(sys.modules, {"oss_manager": mock_module}):
                fn(str(tmp_path), str(models_dir / "rssm_model.pt"))

        # Should have called upload_model twice and upload_training_data once
        assert mock_oss.upload_model.call_count == 2
        assert mock_oss.upload_training_data.call_count == 1
        out = capsys.readouterr().out
        assert "完成" in out
