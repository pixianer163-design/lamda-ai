#!/usr/bin/env python3
"""
pytest 配置文件

提供共享的fixture、配置和测试工具。
"""

import sys
import os
import pytest
from pathlib import Path

# 添加项目根目录到Python路径
project_root = Path(__file__).parent.parent
sys.path.insert(0, str(project_root))
sys.path.insert(0, str(project_root / 'shared'))
sys.path.insert(0, str(project_root / 'prod/src'))
sys.path.insert(0, str(project_root / 'active_src'))

# 测试配置
TEST_DATA_DIR = project_root / 'tests' / 'fixtures'
TEST_DATA_DIR.mkdir(exist_ok=True)

# 环境变量设置（测试环境）
os.environ['AGENT_ENV'] = 'test'
os.environ['AGENT_LOG_DIR'] = str(project_root / 'tests' / 'logs')
os.environ['AGENT_DATA_DIR'] = str(TEST_DATA_DIR)


@pytest.fixture(scope="session")
def test_data_dir():
    """测试数据目录fixture"""
    return TEST_DATA_DIR


@pytest.fixture(scope="session")
def mock_market_data():
    """模拟市场数据fixture"""
    return {
        "00700": {
            "price": 385.0,
            "rsi": 65,
            "ma5": 382.0,
            "ma20": 375.0,
            "change_pct": 1.5,
            "volume": 1000000,
            "data_source": "mock"
        },
        "09988": {
            "price": 85.0,
            "rsi": 45,
            "ma5": 84.0,
            "ma20": 86.0,
            "change_pct": -0.5,
            "volume": 800000,
            "data_source": "mock"
        },
        "03690": {
            "price": 130.0,
            "rsi": 70,
            "ma5": 128.0,
            "ma20": 125.0,
            "change_pct": 2.0,
            "volume": 600000,
            "data_source": "mock"
        }
    }


@pytest.fixture(scope="session")
def mock_portfolio():
    """模拟投资组合fixture"""
    return {
        "cash": 100000.0,
        "holdings": {
            "00700": {"quantity": 100, "avg_price": 380.0},
            "09988": {"quantity": 200, "avg_price": 86.0}
        },
        "total_value": 138000.0
    }


@pytest.fixture(scope="session")
def mock_news_items():
    """模拟新闻数据fixture"""
    return [
        {
            "title": "腾讯Q4财报超预期，游戏收入增长20%",
            "content": "腾讯发布2024年Q4财报，游戏业务收入同比增长20%，微信月活突破13亿。",
            "source": "财联社"
        },
        {
            "title": "阿里巴巴云计算业务增速放缓",
            "content": "阿里Q4云业务增速降至10%，低于市场预期。",
            "source": "华尔街见闻"
        }
    ]


@pytest.fixture(scope="session")
def mock_llm_signals():
    """模拟LLM信号fixture"""
    return {
        "00700_sentiment": 0.75,
        "09988_sentiment": 0.45,
        "03690_sentiment": 0.65,
        "market_sentiment": 0.62,
        "timestamp": "2026-02-19T12:00:00"
    }


@pytest.fixture
def clean_test_env():
    """清理测试环境fixture"""
    # 保存原始环境
    original_env = dict(os.environ)
    
    # 设置测试环境变量
    os.environ['AGENT_ENV'] = 'test'
    os.environ['AGENT_LOG_DIR'] = str(project_root / 'tests' / 'logs')
    os.environ['AGENT_DATA_DIR'] = str(TEST_DATA_DIR)
    
    yield
    
    # 恢复原始环境
    os.environ.clear()
    os.environ.update(original_env)


@pytest.fixture
def temp_log_dir(tmp_path):
    """临时日志目录fixture"""
    log_dir = tmp_path / "logs"
    log_dir.mkdir()
    os.environ['AGENT_LOG_DIR'] = str(log_dir)
    return log_dir


# 自定义断言函数
def assert_dict_structure(data, expected_keys, optional_keys=None):
    """断言字典包含预期的键"""
    optional_keys = optional_keys or []
    
    for key in expected_keys:
        assert key in data, f"缺少必需键: {key}"
    
    for key in data:
        if key not in expected_keys and key not in optional_keys:
            pytest.fail(f"意外的键: {key}")


def assert_decision_valid(decision):
    """断言决策字典有效"""
    required_keys = ["action", "confidence"]
    optional_keys = ["reason", "engine", "strategy"]
    
    assert_dict_structure(decision, required_keys, optional_keys)
    assert decision["action"] in ["buy", "sell", "hold"]
    assert 0 <= decision["confidence"] <= 1


# 测试标记装饰器
def integration_test(func):
    """集成测试标记"""
    return pytest.mark.integration(func)


def unit_test(func):
    """单元测试标记"""
    return pytest.mark.unit(func)


def slow_test(func):
    """慢测试标记"""
    return pytest.mark.slow(func)


# 测试配置类
class TestConfig:
    """测试配置"""
    
    # 测试超时（秒）
    TIMEOUT = 30
    
    # 是否跳过需要外部依赖的测试
    SKIP_EXTERNAL = os.environ.get('SKIP_EXTERNAL_TESTS', '1') == '1'
    
    # 测试数据目录
    DATA_DIR = TEST_DATA_DIR