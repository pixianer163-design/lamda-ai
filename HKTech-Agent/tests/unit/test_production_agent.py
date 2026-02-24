#!/usr/bin/env python3
"""
生产模块单元测试

测试 LLMEnhancedAgent 等生产模块。
"""

import pytest
import sys
import os
import json
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock

# 添加项目根目录到Python路径
project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))
sys.path.insert(0, str(project_root / 'shared'))
sys.path.insert(0, str(project_root / 'prod/src'))
sys.path.insert(0, str(project_root / 'active_src'))

# 测试配置
TEST_DATA_DIR = project_root / 'tests' / 'fixtures'
TEST_DATA_DIR.mkdir(exist_ok=True)


class TestLLMEnhancedAgent:
    """测试 LLMEnhancedAgent"""
    
    def test_agent_import(self):
        """测试 agent 模块导入"""
        try:
            from llm_enhanced_agent import LLMEnhancedAgent
            assert LLMEnhancedAgent is not None
        except ImportError as e:
            pytest.skip(f"LLMEnhancedAgent 模块不可用: {e}")
    
    def test_agent_init_with_mocks(self, temp_log_dir):
        """测试 agent 初始化 (使用模拟依赖)"""
        # 模拟依赖模块
        with patch('llm_enhanced_agent.WorldModelIntegration') as mock_wm, \
             patch('llm_enhanced_agent.LLMSignalExtractor') as mock_extractor, \
             patch('llm_enhanced_agent.LLMDecisionEnhancer') as mock_enhancer:
            
            # 设置模拟对象
            mock_wm_instance = Mock()
            mock_wm_instance.enabled = False
            mock_wm.return_value = mock_wm_instance
            
            mock_extractor_instance = Mock()
            mock_extractor.return_value = mock_extractor_instance
            
            mock_enhancer_instance = Mock()
            mock_enhancer.return_value = mock_enhancer_instance
            
            # 导入并创建agent
            from llm_enhanced_agent import LLMEnhancedAgent
            
            # 使用临时数据目录
            test_data_dir = TEST_DATA_DIR / "test_agent"
            test_data_dir.mkdir(exist_ok=True)
            
            agent = LLMEnhancedAgent(data_dir=str(test_data_dir))
            
            # 验证agent属性
            assert agent.data_dir == str(test_data_dir)
            assert agent.wm_integration == mock_wm_instance
            assert agent.llm_extractor == mock_extractor_instance
            assert agent.llm_enhancer == mock_enhancer_instance
            
            # 验证投资组合加载
            assert "cash" in agent.portfolio
            assert "holdings" in agent.portfolio
            assert "total_value" in agent.portfolio
    
    def test_load_portfolio_existing_file(self, temp_log_dir):
        """测试加载已存在的投资组合文件"""
        from llm_enhanced_agent import LLMEnhancedAgent
        
        # 创建测试数据目录
        test_data_dir = TEST_DATA_DIR / "test_portfolio"
        test_data_dir.mkdir(exist_ok=True)
        
        # 创建投资组合文件
        portfolio_file = test_data_dir / "portfolio.json"
        test_portfolio = {
            "cash": 50000.0,
            "holdings": {
                "00700": {"quantity": 50, "avg_price": 400.0}
            },
            "total_value": 70000.0
        }
        
        with open(portfolio_file, 'w') as f:
            json.dump(test_portfolio, f)
        
        # 创建agent并测试
        agent = LLMEnhancedAgent(data_dir=str(test_data_dir))
        
        # 验证投资组合被正确加载
        assert agent.portfolio["cash"] == 50000.0
        assert "00700" in agent.portfolio["holdings"]
        assert agent.portfolio["holdings"]["00700"]["quantity"] == 50
        assert agent.portfolio["total_value"] == 70000.0
    
    def test_load_portfolio_default(self, temp_log_dir):
        """测试加载默认投资组合（当文件不存在时）"""
        from llm_enhanced_agent import LLMEnhancedAgent
        
        # 使用不存在的目录确保文件不存在
        test_data_dir = TEST_DATA_DIR / "nonexistent_dir"
        if test_data_dir.exists():
            import shutil
            shutil.rmtree(test_data_dir)
        
        # 创建agent（应该使用默认投资组合）
        agent = LLMEnhancedAgent(data_dir=str(test_data_dir))
        
        # 验证默认投资组合
        assert agent.portfolio["cash"] == 19000
        assert agent.portfolio["holdings"] == {}
        assert agent.portfolio["total_value"] == 19000
    
    def test_fallback_strategy(self, mock_market_data):
        """测试传统回退策略"""
        from llm_enhanced_agent import LLMEnhancedAgent
        
        # 创建agent
        test_data_dir = TEST_DATA_DIR / "test_fallback"
        test_data_dir.mkdir(exist_ok=True)
        agent = LLMEnhancedAgent(data_dir=str(test_data_dir))
        
        # 测试回退策略
        decisions = agent._fallback_strategy(mock_market_data)
        
        # 验证决策格式
        assert isinstance(decisions, dict)
        assert len(decisions) > 0
        
        for code, decision in decisions.items():
            assert "action" in decision
            assert decision["action"] in ["buy", "sell", "hold"]
            assert "confidence" in decision
            assert 0 <= decision["confidence"] <= 1
    
    def test_base_strategy_without_prediction(self, mock_market_data):
        """测试基础策略（无预测）"""
        from llm_enhanced_agent import LLMEnhancedAgent
        
        # 创建agent并模拟策略引擎
        test_data_dir = TEST_DATA_DIR / "test_base_strategy"
        test_data_dir.mkdir(exist_ok=True)
        
        with patch('llm_enhanced_agent.get_strategy_engine') as mock_get_engine:
            # 模拟策略引擎
            mock_engine = Mock()
            mock_engine.generate_signals.return_value = {
                "00700": {"action": "buy", "confidence": 0.7, "reason": "测试"},
                "09988": {"action": "hold", "confidence": 0.5, "reason": "测试"},
                "03690": {"action": "sell", "confidence": 0.6, "reason": "测试"}
            }
            mock_get_engine.return_value = mock_engine
            
            # 创建agent
            agent = LLMEnhancedAgent(data_dir=str(test_data_dir))
            agent.strategy_engine = mock_engine
            
            # 测试基础策略
            decisions = agent._base_strategy(mock_market_data, None)
            
            # 验证决策
            assert isinstance(decisions, dict)
            assert len(decisions) == 3
            
            for code, decision in decisions.items():
                assert "action" in decision
                assert decision["action"] in ["buy", "sell", "hold"]
                assert "confidence" in decision
                assert 0 <= decision["confidence"] <= 1
    
    def test_agent_with_mocked_components(self, mock_market_data, mock_portfolio):
        """测试agent与模拟组件的集成"""
        from llm_enhanced_agent import LLMEnhancedAgent
        
        # 创建测试数据目录
        test_data_dir = TEST_DATA_DIR / "test_integration"
        test_data_dir.mkdir(exist_ok=True)
        
        # 模拟所有依赖
        with patch('llm_enhanced_agent.WorldModelIntegration') as mock_wm, \
             patch('llm_enhanced_agent.LLMSignalExtractor') as mock_extractor, \
             patch('llm_enhanced_agent.LLMDecisionEnhancer') as mock_enhancer, \
             patch('llm_enhanced_agent.get_strategy_engine') as mock_get_engine:
            
            # 设置模拟对象
            mock_wm_instance = Mock()
            mock_wm_instance.enabled = False
            mock_wm.return_value = mock_wm_instance
            
            mock_extractor_instance = Mock()
            mock_extractor_instance.get_latest_signals.return_value = {
                "00700_sentiment": 0.7,
                "09988_sentiment": 0.5,
                "03690_sentiment": 0.6,
                "market_sentiment": 0.6
            }
            mock_extractor.return_value = mock_extractor_instance
            
            mock_enhancer_instance = Mock()
            mock_enhancer_instance.enhance_decision.return_value = {
                "llm_output": {
                    "analysis": "测试分析",
                    "recommendations": []
                },
                "final_decision": {
                    "00700": {"action": "hold", "confidence": 0.5, "reason": "测试"},
                    "09988": {"action": "hold", "confidence": 0.5, "reason": "测试"},
                    "03690": {"action": "hold", "confidence": 0.5, "reason": "测试"}
                }
            }
            mock_enhancer.return_value = mock_enhancer_instance
            
            mock_engine = Mock()
            mock_engine.generate_signals.return_value = {
                "00700": {"action": "buy", "confidence": 0.7, "reason": "测试"},
                "09988": {"action": "hold", "confidence": 0.5, "reason": "测试"},
                "03690": {"action": "sell", "confidence": 0.6, "reason": "测试"}
            }
            mock_get_engine.return_value = mock_engine
            
            # 创建agent
            agent = LLMEnhancedAgent(data_dir=str(test_data_dir))
            agent.portfolio = mock_portfolio  # 使用模拟投资组合
            
            # 模拟_load_market_data返回模拟数据
            agent._load_market_data = Mock(return_value=mock_market_data)
            
            # 运行每日分析
            result = agent.run_daily_analysis()
            
            # 验证结果
            assert isinstance(result, dict)
            assert "llm_output" in result
            assert "final_decision" in result
            assert len(result["final_decision"]) == 3
            
            # 验证方法被调用
            mock_extractor_instance.get_latest_signals.assert_called_once()
            mock_enhancer_instance.enhance_decision.assert_called_once()


if __name__ == "__main__":
    # 命令行直接运行测试
    pytest.main([__file__, "-v"])