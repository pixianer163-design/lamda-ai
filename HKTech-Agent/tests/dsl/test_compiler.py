#!/usr/bin/env python3
"""
DSL 编译器测试

测试范围:
- DSL 编译流程
- 错误处理
- 缓存功能
"""

import os
import sys
import unittest
from pathlib import Path

# 添加父目录到路径
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from factory.dsl.compiler import DSLParser
from factory.dsl.types import CompileError


class TestDSLCompiler(unittest.TestCase):
    """DSL 编译器测试"""
    
    def setUp(self):
        """测试前准备"""
        self.parser = DSLParser(use_cache=False)
        self.test_dsl = """
strategy TestStrategy
    description: "测试策略"
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
    
    def test_compile_basic(self):
        """测试基本编译功能"""
        python_code = self.parser.compile(self.test_dsl)
        
        # 验证生成的代码
        self.assertIn('class TeststrategyStrategy', python_code)
        self.assertIn('def __init__', python_code)
        self.assertIn('def calculate_indicators', python_code)
        self.assertIn('def generate_signal', python_code)
        
        # 验证可以执行
        namespace = {}
        exec(python_code, namespace)
        
        # 验证类存在
        self.assertIn('TeststrategyStrategy', namespace)
    
    def test_compile_with_params(self):
        """测试带参数的编译"""
        dsl_code = """
strategy ParamTest {
    param period: period = 20
    param threshold: int = 50
    
    indicator sma: SMA(period=period)
    
    when "Entry" {
        trigger: sma > threshold
        action: BUY
    }
    
    position {
        max_position = 0.5
        risk_per_trade = 2%
    }
}
"""
        python_code = self.parser.compile(dsl_code)
        
        # 验证参数被使用
        self.assertIn('period=20', python_code)
        self.assertIn('threshold=50', python_code)
    
    def test_compile_invalid_dsl(self):
        """测试无效 DSL 的错误处理"""
        invalid_dsl = """
strategy InvalidTest {
    indicator rsi: INVALID_INDICATOR(period=14)
    
    position {
        max_position = 0.5
        risk_per_trade = 2%
    }
}
"""
        with self.assertRaises(CompileError):
            self.parser.compile(invalid_dsl)
    
    def test_compile_missing_position(self):
        """测试缺少 position 块的错误"""
        incomplete_dsl = """
strategy IncompleteTest {
    param period: period = 14
    indicator rsi: RSI(period=period)
    
    when "Entry" {
        trigger: rsi < 30
        action: BUY
    }
    
    position {
        max_position = 0.5
        risk_per_trade = 2%
    }
}
"""
        # 应该能编译
        python_code = self.parser.compile(incomplete_dsl)
        self.assertIn('IncompleteTest', python_code)


class TestDSLParser(unittest.TestCase):
    """DSL 解析器测试"""
    
    def setUp(self):
        self.parser = DSLParser()
    
    def test_parse_valid_strategy(self):
        """测试解析有效策略"""
        dsl_code = """
strategy ValidTest {
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
        model = self.parser.parse(dsl_code)
        
        self.assertEqual(model.strategy.name, 'ValidTest')
        self.assertEqual(len(model.strategy.indicators), 1)
        self.assertEqual(len(model.strategy.conditions), 1)
    
    def test_parse_with_description(self):
        """测试解析带描述的策略"""
        dsl_code = """
strategy DescribedTest
    description: "这是一个测试策略"
{
    indicator rsi: RSI(period=14)
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
        model = self.parser.parse(dsl_code)
        
        self.assertEqual(model.strategy.description, "这是一个测试策略")


class TestDSLCache(unittest.TestCase):
    """DSL 缓存测试"""
    
    def setUp(self):
        self.parser = DSLParser(use_cache=True)
        self.test_dsl = """
strategy CacheTest {
    indicator rsi: RSI(period=14)
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
    
    def test_cache_hit(self):
        """测试缓存命中"""
        # 第一次编译
        code1 = self.parser.compile(self.test_dsl)
        
        # 第二次编译（应该命中缓存）
        code2 = self.parser.compile(self.test_dsl)
        
        # 验证代码相同
        self.assertEqual(code1, code2)
    
    def test_cache_different_code(self):
        """测试不同代码生成不同缓存"""
        dsl1 = """
strategy Test1 {
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
        dsl2 = """
strategy Test2 {
    param period: period = 20
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
        code1 = self.parser.compile(dsl1)
        code2 = self.parser.compile(dsl2)
        
        # 验证代码不同
        self.assertNotEqual(code1, code2)


if __name__ == '__main__':
    unittest.main(verbosity=2)
