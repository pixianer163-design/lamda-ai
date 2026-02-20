#!/usr/bin/env python3
"""
共享模块单元测试

测试 constants, config_loader, strategy_engine, logger, health_check 等共享模块。
"""

import pytest
import sys
import os
import json
from pathlib import Path


class TestConstants:
    """测试 constants 模块"""
    
    def test_constants_import(self):
        """测试 constants 模块导入"""
        try:
            import constants
            assert constants is not None
        except ImportError:
            pytest.skip("constants 模块不可用")
    
    def test_default_stocks(self):
        """测试默认股票列表"""
        try:
            import constants
            stocks = constants.DEFAULT_STOCKS
            assert isinstance(stocks, list)
            assert len(stocks) >= 1
            assert all(isinstance(code, str) for code in stocks)
        except ImportError:
            pytest.skip("constants 模块不可用")
    
    def test_stock_names(self):
        """测试股票名称映射"""
        try:
            import constants
            stock_names = constants.STOCK_NAMES
            assert isinstance(stock_names, dict)
            
            # 检查核心股票
            for code in ["00700", "09988", "03690"]:
                if code in stock_names:
                    assert isinstance(stock_names[code], str)
        except ImportError:
            pytest.skip("constants 模块不可用")
    
    def test_get_yf_symbol(self):
        """测试 Yahoo Finance 代码转换"""
        try:
            import constants
            symbol = constants.get_yf_symbol("00700")
            assert isinstance(symbol, str)
            assert len(symbol) > 0
        except ImportError:
            pytest.skip("constants 模块不可用")


class TestConfigLoader:
    """测试 config_loader 模块"""
    
    def test_config_loader_import(self):
        """测试 config_loader 模块导入"""
        try:
            import config_loader
            assert config_loader is not None
        except ImportError:
            pytest.skip("config_loader 模块不可用")
    
    def test_config_loader_init(self):
        """测试配置加载器初始化"""
        try:
            from config_loader import ConfigLoader
            loader = ConfigLoader()
            assert loader is not None
            assert hasattr(loader, 'load_config')
        except ImportError:
            pytest.skip("config_loader 模块不可用")
    
    def test_load_default_config(self):
        """测试加载默认配置"""
        try:
            from config_loader import ConfigLoader
            loader = ConfigLoader()
            config = loader.load_config()
            
            assert isinstance(config, dict)
            assert "agent" in config
            assert "trading" in config
            assert "stocks" in config
        except ImportError:
            pytest.skip("config_loader 模块不可用")


class TestLogger:
    """测试 logger 模块"""
    
    def test_logger_import(self):
        """测试 logger 模块导入"""
        try:
            import logger
            assert logger is not None
        except ImportError:
            pytest.skip("logger 模块不可用")
    
    def test_get_logger(self):
        """测试获取日志器实例"""
        try:
            from logger import get_logger
            logger_instance = get_logger()
            assert logger_instance is not None
            assert hasattr(logger_instance, 'info')
            assert hasattr(logger_instance, 'error')
        except ImportError:
            pytest.skip("logger 模块不可用")
    
    def test_log_functions(self):
        """测试日志函数"""
        try:
            from logger import info, warning, error
            # 这些函数应该存在
            assert callable(info)
            assert callable(warning)
            assert callable(error)
        except ImportError:
            pytest.skip("logger 模块不可用")
    
    def test_log_performance(self, temp_log_dir):
        """测试性能日志"""
        try:
            from logger import get_logger
            logger = get_logger()
            
            # 配置日志到临时目录
            logger.configure(log_level="INFO", log_to_file=True, log_dir=str(temp_log_dir))
            
            # 记录性能日志
            logger.log_performance("test_operation", 0.123, {"detail": "test"})
            
            # 检查日志文件是否创建
            log_file = temp_log_dir / "hktech_agent.log"
            if log_file.exists():
                content = log_file.read_text()
                assert "test_operation" in content or "性能指标" in content
        except ImportError:
            pytest.skip("logger 模块不可用")


class TestStrategyEngine:
    """测试 strategy_engine 模块"""
    
    def test_strategy_engine_import(self):
        """测试 strategy_engine 模块导入"""
        try:
            import strategy_engine
            assert strategy_engine is not None
        except ImportError:
            pytest.skip("strategy_engine 模块不可用")
    
    def test_strategy_engine_init(self):
        """测试策略引擎初始化"""
        try:
            from strategy_engine import StrategyEngine
            engine = StrategyEngine(engine_type="virtual")
            assert engine is not None
            assert hasattr(engine, 'generate_signals')
            assert hasattr(engine, 'optimize_parameters')
            assert hasattr(engine, 'backtest')
        except ImportError:
            pytest.skip("strategy_engine 模块不可用")
    
    def test_generate_signals(self, mock_market_data):
        """测试生成交易信号"""
        try:
            from strategy_engine import StrategyEngine
            engine = StrategyEngine(engine_type="virtual")
            signals = engine.generate_signals(mock_market_data)
            
            assert isinstance(signals, dict)
            
            # 检查信号格式
            for code, signal in signals.items():
                assert "action" in signal
                assert signal["action"] in ["buy", "sell", "hold"]
                assert "confidence" in signal
                assert 0 <= signal["confidence"] <= 1
                assert "reason" in signal
        except ImportError:
            pytest.skip("strategy_engine 模块不可用")
    
    def test_get_capabilities(self):
        """测试获取引擎能力"""
        try:
            from strategy_engine import StrategyEngine
            engine = StrategyEngine(engine_type="virtual")
            capabilities = engine.get_capabilities()
            
            assert isinstance(capabilities, dict)
            assert "engine_type" in capabilities
            assert "optimization_supported" in capabilities
            assert "backtest_supported" in capabilities
        except ImportError:
            pytest.skip("strategy_engine 模块不可用")


class TestHealthCheck:
    """测试 health_check 模块"""
    
    def test_health_check_import(self):
        """测试 health_check 模块导入"""
        try:
            import health_check
            assert health_check is not None
        except ImportError:
            pytest.skip("health_check 模块不可用")
    
    def test_health_check_init(self):
        """测试健康检查器初始化"""
        try:
            from health_check import HealthCheck
            checker = HealthCheck()
            assert checker is not None
            assert hasattr(checker, 'run_all_checks')
            assert hasattr(checker, 'generate_report')
        except ImportError:
            pytest.skip("health_check 模块不可用")
    
    def test_run_health_check(self):
        """测试运行健康检查"""
        try:
            from health_check import run_health_check
            report = run_health_check(format="text")
            assert isinstance(report, str)
            assert len(report) > 0
        except ImportError:
            pytest.skip("health_check 模块不可用")
    
    def test_check_python_environment(self):
        """测试Python环境检查"""
        try:
            from health_check import HealthCheck
            checker = HealthCheck()
            result = checker.check_python_environment()
            
            assert isinstance(result, dict)
            assert "status" in result
            assert result["status"] in ["healthy", "degraded", "unhealthy"]
            assert "python_version" in result
        except ImportError:
            pytest.skip("health_check 模块不可用")


class TestIntegration:
    """集成测试 - 模块间协作"""
    
    def test_constants_with_config_loader(self):
        """测试 constants 与 config_loader 的集成"""
        try:
            import constants
            from config_loader import ConfigLoader
            
            loader = ConfigLoader()
            config = loader.load_config()
            
            # 检查配置中是否使用了正确的股票代码
            stocks_config = config.get("stocks", [])
            assert isinstance(stocks_config, list)
            
            # 如果配置中有股票，检查格式
            if stocks_config:
                stock = stocks_config[0]
                assert "code" in stock
                assert isinstance(stock["code"], str)
        except ImportError:
            pytest.skip("所需模块不可用")
    
    def test_logger_with_strategy_engine(self, mock_market_data, temp_log_dir):
        """测试 logger 与 strategy_engine 的集成"""
        try:
            from logger import get_logger
            from strategy_engine import StrategyEngine
            
            # 配置日志
            logger = get_logger()
            logger.configure(log_level="INFO", log_to_file=True, log_dir=str(temp_log_dir))
            
            # 创建策略引擎
            engine = StrategyEngine(engine_type="virtual")
            
            # 生成信号（应该记录日志）
            signals = engine.generate_signals(mock_market_data)
            assert len(signals) > 0
            
            # 检查日志文件
            log_file = temp_log_dir / "hktech_agent.log"
            if log_file.exists():
                content = log_file.read_text()
                # 日志应该包含策略引擎的相关信息
                assert len(content) > 0
        except ImportError:
            pytest.skip("所需模块不可用")


if __name__ == "__main__":
    # 命令行直接运行测试
    pytest.main([__file__, "-v"])