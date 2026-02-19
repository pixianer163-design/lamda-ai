#!/usr/bin/env python3
"""
世界模型集成模块
将RSSM世界模型集成到恒生科技Agent
"""

import sys

import json
import os
from datetime import datetime
try:
    import numpy as np
    NUMPY_AVAILABLE = True
except ImportError:
    print("⚠️  numpy未安装，使用替代方案")
    NUMPY_AVAILABLE = False
    # 创建虚拟np模块
    class MockNumpy:
        @staticmethod
        def array(data, dtype=None):
            return data
        @staticmethod  
        def float32():
            return float
    np = MockNumpy()

# 导入共享常量
SHARED_CONSTANTS_AVAILABLE = False
constants = None  # 默认值
try:
    import constants
    SHARED_CONSTANTS_AVAILABLE = True
except ImportError:
    print("⚠️ 共享常量模块不可用，使用本地定义")

# 导入世界模型
try:
    from rssm_world_model import WorldModelTrainer
    WORLD_MODEL_AVAILABLE = True
except ImportError as e:
    print(f"⚠️  世界模型未安装: {e}")
    WORLD_MODEL_AVAILABLE = False


class WorldModelIntegration:
    """
    世界模型集成类
    为Agent提供预测和决策支持
    """
    
    def __init__(self, data_dir=None):
        import os
        if data_dir is None:
            # 默认使用项目相对路径
            current_dir = os.path.dirname(os.path.abspath(__file__))
            data_dir = os.path.join(current_dir, '../../data')
            print(f"📂 世界模型数据目录: {data_dir}")
        
        self.data_dir = data_dir
        self.enabled = WORLD_MODEL_AVAILABLE
        
        if self.enabled:
            try:
                self.trainer = WorldModelTrainer(data_dir=self.data_dir, device="cpu")
                print(f"📂 训练器模型路径: {self.trainer.model_path}")
                print(f"📂 路径存在: {os.path.exists(self.trainer.model_path)}")
                self.loaded = self.trainer.load()
                if self.loaded:
                    print("✅ 世界模型集成: 已加载")
                else:
                    print("⚠️  世界模型集成: 未找到训练好的模型")
                    self.enabled = False
            except Exception as e:
                print(f"❌ 世界模型加载失败: {e}")
                self.enabled = False
        else:
            self.loaded = False
    
    def prepare_observation(self, market_data: dict, portfolio: dict) -> list:
        """
        准备观测向量 (15维)
        
        market_data: {'00700': {'price': 385, 'ma5': 382, 'ma20': 375, 'rsi': 65, 'change_pct': 1.5}, ...}
        portfolio: {'cash': 19000, 'holdings': {...}}
        """
        obs = []
        
        # 使用共享常量或本地定义
        if SHARED_CONSTANTS_AVAILABLE and constants is not None:
            stock_codes = constants.DEFAULT_STOCKS
        else:
            stock_codes = ['00700', '09988', '03690']
        
        for code in stock_codes:
            if code in market_data:
                data = market_data[code]
                obs.extend([
                    data.get('price', 0) / 500,  # 价格归一化
                    data.get('ma5', data.get('price', 0)) / 500,
                    data.get('ma20', data.get('price', 0)) / 500,
                    data.get('rsi', 50) / 100,
                    data.get('change_pct', 0) / 10
                ])
            else:
                # 缺失数据用默认值
                obs.extend([0, 0, 0, 0.5, 0])
        
        return obs
    
    def predict_future(self, market_data: dict, portfolio: dict, 
                       proposed_action: list = None, horizon: int = 3) -> dict:
        """
        预测未来走势
        
        返回:
            {
                'enabled': True/False,
                'horizon': 预测天数,
                'predicted_returns': [day1_return, day2_return, ...],
                'cumulative_return': 累计收益,
                'confidence': 置信度,
                'recommendation': '买入'/'卖出'/'持有',
                'reasoning': '解释'
            }
        """
        if not self.enabled or not self.loaded:
            return {
                'enabled': False,
                'message': '世界模型未启用'
            }
        
        # 准备观测
        obs = self.prepare_observation(market_data, portfolio)
        
        # 默认动作: 维持当前仓位
        if proposed_action is None:
            proposed_action = [0.0, 0.0, 0.0]
        
        try:
            # 想象未来
            prediction = self.trainer.imagine_future(obs, proposed_action, horizon=horizon)
            
            # 解析结果
            returns = [step['predicted_reward'] for step in prediction['trajectory']]
            cumulative = prediction['cumulative_reward']
            
            # 生成建议
            if cumulative > 0.01:
                recommendation = '加仓'
                reasoning = f'模型预测未来{horizon}天累计收益{cumulative:.2f}%，趋势向好'
            elif cumulative < -0.01:
                recommendation = '减仓'
                reasoning = f'模型预测未来{horizon}天累计收益{cumulative:.2f}%，建议避险'
            else:
                recommendation = '持有'
                reasoning = f'模型预测未来{horizon}天收益{cumulative:.2f}%，趋势不明朗'
            
            # 简单置信度（基于预测一致性）
            if len(returns) > 1:
                # 手动计算标准差和平均值（避免numpy依赖）
                mean_val = sum(returns) / len(returns)
                variance = sum((x - mean_val) ** 2 for x in returns) / len(returns)
                std_val = variance ** 0.5
                consistency = 1 - abs(std_val / (abs(mean_val) + 0.001))
                confidence = max(0.3, min(0.9, consistency))
            else:
                confidence = 0.5
            
            return {
                'enabled': True,
                'horizon': horizon,
                'predicted_returns': returns,
                'cumulative_return': cumulative,
                'confidence': round(confidence, 2),
                'recommendation': recommendation,
                'reasoning': reasoning,
                'actions': [step['action'] for step in prediction['trajectory']]
            }
            
        except Exception as e:
            return {
                'enabled': True,
                'error': str(e),
                'message': '预测过程中出错'
            }
    
    def enhance_decision_prompt(self, base_prompt: str, market_data: dict, 
                                portfolio: dict) -> str:
        """
        增强决策Prompt，加入世界模型预测
        """
        prediction = self.predict_future(market_data, portfolio)
        
        if not prediction.get('enabled'):
            # 世界模型未启用，返回原始Prompt
            return base_prompt
        
        # 构建世界模型分析
        world_model_section = f"""

【世界模型预测】（基于RSSM神经网络）
预测天数: {prediction['horizon']}天
累计预期收益: {prediction['cumulative_return']:.2f}%
模型置信度: {prediction['confidence']:.0%}

逐日预测收益:
"""
        for i, ret in enumerate(prediction['predicted_returns']):
            world_model_section += f"  Day {i+1}: {ret:+.4f}\n"
        
        world_model_section += f"""
模型建议: {prediction['recommendation']}
理由: {prediction['reasoning']}

"""
        
        # 插入到Prompt中
        enhanced_prompt = base_prompt + world_model_section
        
        return enhanced_prompt
    
    def get_daily_report(self, market_data: dict, portfolio: dict) -> str:
        """
        生成每日世界模型报告
        """
        prediction = self.predict_future(market_data, portfolio, horizon=5)
        
        if not prediction.get('enabled'):
            return "🤖 世界模型: 未启用"
        
        report = f"""🤖 世界模型预测 (RSSM)

📊 未来{prediction['horizon']}天展望
累计预期收益: {prediction['cumulative_return']:+.2f}%
模型置信度: {prediction['confidence']:.0%}

📈 逐日预测:
"""
        for i, ret in enumerate(prediction['predicted_returns']):
            emoji = "📈" if ret > 0 else "📉" if ret < 0 else "➡️"
            report += f"  {emoji} Day {i+1}: {ret:+.4f}\n"
        
        report += f"""
🎯 模型建议: {prediction['recommendation']}
💡 {prediction['reasoning']}
"""
        return report


def test_integration():
    """测试集成"""
    print("="*50)
    print("🧪 测试世界模型集成")
    print("="*50)
    
    # 创建集成实例
    wm = WorldModelIntegration()
    
    if not wm.enabled:
        print("⚠️  世界模型未启用，跳过测试")
        return
    
    # 模拟市场数据
    market_data = {
        "00700": {"price": 385, "ma5": 382, "ma20": 375, "rsi": 65, "change_pct": 1.5},
        "09988": {"price": 85, "ma5": 84, "ma20": 86, "rsi": 45, "change_pct": -0.5},
        "03690": {"price": 130, "ma5": 128, "ma20": 125, "rsi": 70, "change_pct": 2.0}
    }
    
    portfolio = {
        "cash": 19000,
        "holdings": {
            "00700": {"shares": 48},
            "09988": {"shares": 213},
            "03690": {"shares": 141}
        }
    }
    
    # 测试预测
    print("\n🔮 预测未来...")
    prediction = wm.predict_future(market_data, portfolio, horizon=3)
    
    print(f"\n预测结果:")
    print(f"  累计收益: {prediction['cumulative_return']:.4f}")
    print(f"  置信度: {prediction['confidence']}")
    print(f"  建议: {prediction['recommendation']}")
    print(f"  理由: {prediction['reasoning']}")
    
    # 测试日报
    print("\n📄 日报格式:")
    report = wm.get_daily_report(market_data, portfolio)
    print(report)
    
    print("\n✅ 集成测试完成!")


if __name__ == "__main__":
    test_integration()
