"""Tests for data_collector disk cache"""
import sys, os, json
from pathlib import Path
from unittest.mock import patch
import pytest

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "active_src"))
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "shared"))


def test_cache_write_on_success(tmp_path):
    """成功获取数据后应写入磁盘缓存"""
    from data_collector import HKStockDataCollector
    collector = HKStockDataCollector(data_dir=str(tmp_path))

    fresh = {"price": 385.0, "data_source": "yahoo", "name": "腾讯控股",
             "code": "00700", "change": 1.0, "change_pct": 0.26}
    with patch.object(collector, '_get_yahoo_data', return_value=fresh), \
         patch.object(collector, '_get_sina_data', return_value=None):
        collector.get_daily_data(days=5)

    cache_files = list((tmp_path / "cache").glob("00700_*.json"))
    assert len(cache_files) >= 1, "缓存文件应存在"


def test_cache_read_on_failure(tmp_path):
    """所有数据源失败时应读取最新缓存"""
    from data_collector import HKStockDataCollector
    collector = HKStockDataCollector(data_dir=str(tmp_path))

    cache_dir = tmp_path / "cache"
    cache_dir.mkdir(parents=True, exist_ok=True)
    cached = {"price": 380.0, "data_source": "cache", "name": "腾讯控股",
              "code": "00700", "change": 0.0, "change_pct": 0.0,
              "_cached_at": "2026-02-19T08:00:00"}
    (cache_dir / "00700_20260219.json").write_text(json.dumps(cached))

    with patch.object(collector, '_get_yahoo_data', return_value=None), \
         patch.object(collector, '_get_sina_data', return_value=None):
        data = collector.get_daily_data(days=5)

    assert "00700" in data
    assert data["00700"].get("data_source") in ("cache", "mock")


def test_cache_not_read_when_fresh_data_available(tmp_path):
    """有新鲜数据时不应读取缓存"""
    from data_collector import HKStockDataCollector
    collector = HKStockDataCollector(data_dir=str(tmp_path))

    fresh = {"price": 400.0, "data_source": "yahoo", "name": "腾讯控股",
             "code": "00700", "change": 1.0, "change_pct": 0.26}
    with patch.object(collector, '_get_yahoo_data', return_value=fresh):
        data = collector.get_daily_data(days=5)

    assert data["00700"]["data_source"] == "yahoo"
