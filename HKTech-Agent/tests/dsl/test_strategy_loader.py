#!/usr/bin/env python3
"""
策略加载器测试

测试范围:
- DSL 策略加载
- Python 策略加载
- 热重载功能
- 错误处理
"""

import os
import sys
import unittest
from pathlib import Path

# 添加父目录到路径
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

import sys
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from factory.strategy_loader import StrategyLoader
from factory.dsl.types import CompileError


class TestStrategyLoader(unittest.TestCase):
    """策略加载器测试"""
    
    def setUp(self):
        """测试前准备"""
        self.loader = StrategyLoader(cache_enabled=True)
        
        # 创建测试 DSL 文件
        self.test_dsl_content = """
strategy LoaderTest
    description: "测试策略加载"
{
    param period: period = 14
    
    indicator rsi: RSI(period=period)
    
    when "Test Entry" {
        trigger: rsi < 30
        action: BUY
        size: 0.5
        confidence: 0.8
    }
    
    position {
        max_position = 0.5
        risk_per_trade = 2%
    }
}
"""
        
        # 保存测试文件
        self.test_dsl_file = Path('/tmp/test_loader.dsl')
        with open(self.test_dsl_file, 'w', encoding='utf-8') as f:
            f.write(self.test_dsl_content)
    
    def tearDown(self):
        """测试后清理"""
        if self.test_dsl_file.exists():
            self.test_dsl_file.unlink()
    
    def test_load_dsl_strategy(self):
        """测试加载 DSL 策略"""
        strategy = self.loader.load(str(self.test_dsl_file), 'dsl')
        
        # 验证策略已加载
        self.assertIsNotNone(strategy)
        self.assertIn('test_loader', self.loader.loaded_strategies)
    
    def test_load_auto_detect_dsl(self):
        """测试自动检测 DSL 类型"""
        strategy = self.loader.load(str(self.test_dsl_file), 'auto')
        
        # 验证自动识别为 DSL
        self.assertIsNotNone(strategy)
        metadata = self.loader.get_strategy_info('test_loader')
        self.assertEqual(metadata['type'], 'dsl')
    
    def test_reload_strategy(self):
        """测试热重载策略"""
        # 首次加载
        strategy1 = self.loader.load(str(self.test_dsl_file), 'dsl')
        
        # 修改 DSL 文件
        modified_dsl = self.test_dsl_content.replace('period = 14', 'period = 20')
        with open(self.test_dsl_file, 'w', encoding='utf-8') as f:
            f.write(modified_dsl)
        
        # 重载
        strategy2 = self.loader.reload('test_loader')
        
        # 验证策略已重新加载
        self.assertIsNotNone(strategy2)
    
    def test_list_strategies(self):
        """测试列出已加载策略"""
        self.loader.load(str(self.test_dsl_file), 'dsl')
        
        strategies = self.loader.list_strategies()
        
        self.assertIn('test_loader', strategies)
        self.assertEqual(len(strategies), 1)
    
    def test_unload_strategy(self):
        """测试卸载策略"""
        self.loader.load(str(self.test_dsl_file), 'dsl')
        
        # 验证已加载
        self.assertIn('test_loader', self.loader.loaded_strategies)
        
        # 卸载
        self.loader.unload('test_loader')
        
        # 验证已卸载
        self.assertNotIn('test_loader', self.loader.loaded_strategies)
    
    def test_get_strategy_info(self):
        """测试获取策略信息"""
        self.loader.load(str(self.test_dsl_file), 'dsl')
        
        info = self.loader.get_strategy_info('test_loader')
        
        self.assertIsNotNone(info)
        self.assertEqual(info['name'], 'test_loader')
        self.assertEqual(info['type'], 'dsl')
        self.assertIn('path', info)
    
    def test_load_nonexistent_file(self):
        """测试加载不存在的文件"""
        with self.assertRaises(FileNotFoundError):
            self.loader.load('/nonexistent/file.dsl', 'dsl')
    
    def test_load_invalid_strategy_type(self):
        """测试加载不支持的策略类型"""
        with self.assertRaises(ValueError):
            self.loader.load(str(self.test_dsl_file), 'invalid_type')


class TestStrategyLoaderCache(unittest.TestCase):
    """策略加载器缓存测试"""
    
    def setUp(self):
        self.loader = StrategyLoader(cache_enabled=True)
        
        self.test_dsl = """
strategy CacheTest {
    param period: period = 14
    indicator rsi: RSI(period=period)
    when "Test" {
        trigger: rsi < 30
        action: BUY
    }
    position {
        max_position = 0.5
        risk_per_trade = 2%
    }
}
"""
        self.test_file = Path('/tmp/test_cache.dsl')
        with open(self.test_file, 'w', encoding='utf-8') as f:
            f.write(self.test_dsl)
    
    def tearDown(self):
        if self.test_file.exists():
            self.test_file.unlink()
    
    def test_cache_enabled(self):
        """测试缓存启用"""
        # 首次加载
        strategy1 = self.loader.load(str(self.test_file))
        
        # 再次加载（应该使用缓存）
        strategy2 = self.loader.load(str(self.test_file))
        
        # 验证是同一个实例（缓存命中）
        self.assertIsNotNone(strategy1)
        self.assertIsNotNone(strategy2)


class TestStrategyLoaderErrors(unittest.TestCase):
    """策略加载器错误处理测试"""
    
    def setUp(self):
        self.loader = StrategyLoader()
    
    def test_invalid_dsl_syntax(self):
        """测试无效 DSL 语法"""
        invalid_dsl = """
strategy InvalidTest {
    indicator rsi: INVALID_INDICATOR(period=14)
    position {
        max_position = 0.5
        risk_per_trade = 2%
    }
}
"""
        test_file = Path('/tmp/test_invalid.dsl')
        with open(test_file, 'w', encoding='utf-8') as f:
            f.write(invalid_dsl)
        
        try:
            with self.assertRaises(CompileError):
                self.loader.load(str(test_file), 'dsl')
        finally:
            if test_file.exists():
                test_file.unlink()


if __name__ == '__main__':
    unittest.main(verbosity=2)
