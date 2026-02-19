#!/usr/bin/env python3
"""
生产模块单元测试

测试 LLMSignalExtractor, LLMDecisionEnhancer, RSSMWorldModel 等模块。
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


class TestLLMSignalExtractor:
    """测试 LLMSignalExtractor"""
    
    def test_module_import(self):
        """测试模块导入"""
        try:
            from llm_signal_extractor import LLMSignalExtractor
            assert LLMSignalExtractor is not None
        except ImportError as e:
            pytest.skip(f"LLMSignalExtractor 模块不可用: {e}")
    
    def test_extractor_init(self, temp_log_dir):
        """测试提取器初始化"""
        with patch('llm_signal_extractor.constants') as mock_constants:
            mock_constants.DEFAULT_STOCKS = ["00700", "09988"]
            mock_constants.STOCK_NAMES = {"00700": "腾讯", "09988": "阿里"}
            
            from llm_signal_extractor import LLMSignalExtractor
            
            extractor = LLMSignalExtractor(data_dir=str(temp_log_dir))
            assert extractor.data_dir == str(temp_log_dir)
            assert extractor.stocks == ["00700", "09988"]
            assert extractor.stock_names == {"00700": "腾讯", "09988": "阿里"}
            assert extractor.signals_file.endswith("llm_signals.json")
    
    def test_extract_signals_mock(self, temp_log_dir):
        """测试信号提取（模拟模式）"""
        with patch('llm_signal_extractor.constants') as mock_constants:
            mock_constants.DEFAULT_STOCKS = ["00700"]
            mock_constants.STOCK_NAMES = {"00700": "腾讯"}
            
            from llm_signal_extractor import LLMSignalExtractor
            
            extractor = LLMSignalExtractor(data_dir=str(temp_log_dir))
            
            # 模拟新闻数据
            mock_news = [
                {"title": "测试新闻", "content": "内容", "source": "测试源"}
            ]
            
            # 调用提取方法
            signals = extractor.extract_signals(mock_news)
            
            # 验证返回结构
            assert isinstance(signals, dict)
            assert "00700" in signals
            
            # 验证情感值在合理范围内
            sentiment = signals["00700"]
            assert 0 <= sentiment <= 1
    
    def test_save_and_load_signals(self, temp_log_dir):
        """测试信号保存和加载"""
        with patch('llm_signal_extractor.constants') as mock_constants:
            mock_constants.DEFAULT_STOCKS = ["00700"]
            
            from llm_signal_extractor import LLMSignalExtractor
            
            extractor = LLMSignalExtractor(data_dir=str(temp_log_dir))
            
            # 创建测试信号
            test_signals = {
                "00700_sentiment": 0.75,
                "market_sentiment": 0.62
            }
            
            # 保存信号
            extractor.save_signals(test_signals)
            
            # 验证文件存在
            assert os.path.exists(extractor.signals_file)
            
            # 加载信号
            loaded = extractor.load_signals()
            assert loaded["00700_sentiment"] == 0.75
            assert loaded["market_sentiment"] == 0.62
            assert "timestamp" in loaded
            assert isinstance(loaded["timestamp"], float)


class TestLLMDecisionEnhancer:
    """测试 LLMDecisionEnhancer"""
    
    def test_module_import(self):
        """测试模块导入"""
        try:
            from llm_decision_enhancer import LLMDecisionEnhancer
            assert LLMDecisionEnhancer is not None
        except ImportError as e:
            pytest.skip(f"LLMDecisionEnhancer 模块不可用: {e}")
    
    def test_enhancer_init(self, temp_log_dir):
        """测试增强器初始化"""
        with patch('llm_decision_enhancer.constants') as mock_constants:
            mock_constants.DEFAULT_STOCKS = ["00700", "09988"]
            
            from llm_decision_enhancer import LLMDecisionEnhancer
            
            enhancer = LLMDecisionEnhancer(data_dir=str(temp_log_dir))
            assert enhancer.data_dir == str(temp_log_dir)
            assert enhancer.stocks == ["00700", "09988"]
    
    def test_enhance_decisions_mock(self, temp_log_dir):
        """测试决策增强（模拟模式）"""
        with patch('llm_decision_enhancer.constants') as mock_constants:
            mock_constants.DEFAULT_STOCKS = ["00700", "09988"]
            
            from llm_decision_enhancer import LLMDecisionEnhancer
            
            enhancer = LLMDecisionEnhancer(data_dir=str(temp_log_dir))
            
            # 模拟基础决策
            base_decisions = {
                "00700": {"action": "buy", "confidence": 0.7, "reason": "技术指标"},
                "09988": {"action": "hold", "confidence": 0.5, "reason": "观望"}
            }
            
            # 模拟市场数据
            market_data = {
                "00700": {"price": 385, "rsi": 65, "change_pct": 1.5},
                "09988": {"price": 85, "rsi": 45, "change_pct": -0.8}
            }
            
            # 模拟投资组合
            portfolio = {
                "cash": 10000,
                "holdings": {"00700": {"shares": 10, "avg_price": 380}},
                "total_value": 13850
            }
            
            # 模拟LLM信号
            llm_signals = {
                "00700_sentiment": 0.8,
                "09988_sentiment": 0.4,
                "market_sentiment": 0.65
            }
            
            # 调用增强方法
            result = enhancer.enhance_decision(
                base_decisions, market_data, portfolio, 
                prediction=None, llm_signals=llm_signals
            )
            
            # 验证返回结构
            assert isinstance(result, dict)
            assert "final_decision" in result
            assert "llm_output" in result
            
            enhanced = result["final_decision"]
            assert "00700" in enhanced
            assert "09988" in enhanced
            
            # 验证增强后的决策包含必要字段
            for stock, decision in enhanced.items():
                assert "action" in decision
                assert decision["action"] in ["buy", "sell", "hold"]
                assert "confidence" in decision
                assert 0 <= decision["confidence"] <= 1
                assert "reason" in decision
                assert "enhanced" in decision
                assert decision["enhanced"] is True
    
    def test_calculate_enhanced_confidence(self):
        """测试增强置信度计算"""
        with patch('llm_decision_enhancer.constants'):
            from llm_decision_enhancer import LLMDecisionEnhancer
            
            enhancer = LLMDecisionEnhancer()
            
            # 测试不同情况下的置信度增强
            test_cases = [
                (0.7, 0.8, 0.75),   # 基础置信度0.7，情感0.8，期望增强
                (0.3, 0.2, 0.25),   # 基础置信度0.3，情感0.2，期望降低
                (0.5, 0.5, 0.5),    # 相等
            ]
            
            for base_conf, sentiment, expected in test_cases:
                enhanced = enhancer._calculate_enhanced_confidence(base_conf, sentiment)
                assert abs(enhanced - expected) < 0.01


class TestRSSMWorldModel:
    """测试 RSSMWorldModel"""
    
    def test_module_import(self):
        """测试模块导入"""
        try:
            from rssm_world_model import RSSMWorldModel
            assert RSSMWorldModel is not None
        except ImportError as e:
            pytest.skip(f"RSSMWorldModel 模块不可用: {e}")
    
    def test_world_model_init(self, temp_log_dir):
        """测试世界模型初始化"""
        with patch('rssm_world_model.constants') as mock_constants:
            mock_constants.DEFAULT_STOCKS = ["00700", "09988"]
            
            from rssm_world_model import RSSMWorldModel
            
            model = RSSMWorldModel(data_dir=str(temp_log_dir))
            assert model.data_dir == str(temp_log_dir)
            assert model.stocks == ["00700", "09988"]
            assert model.enabled is True  # 默认启用
    
    def test_predict_mock(self, temp_log_dir):
        """测试预测（模拟模式）"""
        with patch('rssm_world_model.constants') as mock_constants:
            mock_constants.DEFAULT_STOCKS = ["00700"]
            
            from rssm_world_model import RSSMWorldModel
            
            model = RSSMWorldModel(data_dir=str(temp_log_dir))
            
            # 模拟市场数据
            market_data = {
                "00700": {
                    "price": 385.0,
                    "rsi": 65,
                    "ma5": 382.0,
                    "ma20": 375.0,
                    "change_pct": 1.5
                }
            }
            
            # 模拟历史数据
            historical_data = {
                "00700": [380.0, 382.0, 381.0, 383.0, 385.0]
            }
            
            # 调用预测方法
            predictions = model.predict(market_data, historical_data)
            
            # 验证返回结构
            assert isinstance(predictions, dict)
            assert "00700" in predictions
            
            prediction = predictions["00700"]
            assert "predicted_price" in prediction
            assert "predicted_change_pct" in prediction
            assert "confidence" in prediction
            assert "horizon_days" in prediction
            
            # 验证预测值合理性
            assert 0 <= prediction["confidence"] <= 1
            assert prediction["horizon_days"] in [1, 3, 7]
    
    def test_world_model_disabled(self, temp_log_dir):
        """测试禁用状态"""
        with patch('rssm_world_model.constants') as mock_constants:
            mock_constants.DEFAULT_STOCKS = ["00700"]
            
            from rssm_world_model import RSSMWorldModel
            
            model = RSSMWorldModel(data_dir=str(temp_log_dir))
            model.enabled = False
            
            # 当禁用时，预测应返回空字典
            predictions = model.predict({}, {})
            assert predictions == {}


class TestWorldModelIntegration:
    """测试 WorldModelIntegration"""
    
    def test_module_import(self):
        """测试模块导入"""
        try:
            from world_model_integration import WorldModelIntegration
            assert WorldModelIntegration is not None
        except ImportError as e:
            pytest.skip(f"WorldModelIntegration 模块不可用: {e}")
    
    def test_integration_init(self, temp_log_dir):
        """测试集成模块初始化"""
        with patch('world_model_integration.constants') as mock_constants:
            mock_constants.DEFAULT_STOCKS = ["00700", "09988"]
            
            from world_model_integration import WorldModelIntegration
            
            integration = WorldModelIntegration(data_dir=str(temp_log_dir))
            assert integration.data_dir == str(temp_log_dir)
            assert integration.enabled is True
    
    def test_integration_with_mocked_world_model(self, temp_log_dir):
        """测试集成（使用模拟世界模型）"""
        with patch('world_model_integration.constants') as mock_constants, \
             patch('world_model_integration.WorldModelTrainer') as mock_world_model:
            
            mock_constants.DEFAULT_STOCKS = ["00700"]
            
            from world_model_integration import WorldModelIntegration
            
            # 设置模拟世界模型
            mock_instance = Mock()
            mock_instance.enabled = True
            mock_instance.imagine_future.return_value = {
                "trajectory": [
                    {"predicted_reward": 0.01, "action": [0.0, 0.0, 0.0]},
                    {"predicted_reward": 0.02, "action": [0.0, 0.0, 0.0]},
                    {"predicted_reward": 0.015, "action": [0.0, 0.0, 0.0]}
                ],
                "cumulative_reward": 0.045
            }
            mock_world_model.return_value = mock_instance
            
            integration = WorldModelIntegration(data_dir=str(temp_log_dir))
            
            # 模拟市场数据
            market_data = {
                "00700": {"price": 385.0, "rsi": 65}
            }
            
            # 模拟投资组合
            portfolio = {
                "cash": 10000,
                "holdings": {},
                "total_value": 10000
            }
            
            # 调用集成预测
            result = integration.predict_future(market_data, portfolio, horizon=3)
            
            # 验证结果
            assert result['enabled'] is True
            assert result['horizon'] == 3
            assert result['cumulative_return'] == 0.045
            assert 'confidence' in result
            assert 'recommendation' in result
            
            # 验证世界模型被调用
            mock_instance.imagine_future.assert_called_once()


@pytest.mark.unit
def test_all_production_modules():
    """单元测试标记"""
    pass


if __name__ == '__main__':
    pytest.main([__file__, '-v'])