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

# 导入统一常量
from shared.base import get_constants
constants = get_constants()
print(f"✅ 统一常量模块: 可用={constants.available}")

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
        
        # 使用统一常量
        stock_codes = constants.DEFAULT_STOCKS
        
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
                       proposed_action=None, horizon: int = 3) -> dict:
        """使用世界模型预测未来收益"""
        disabled_result = {
            "enabled": False,
            "horizon": horizon,
            "predicted_returns": {},
            "cumulative_return": 0.0,
            "confidence": 0.0,
            "recommendation": "hold",
            "reasoning": "世界模型未加载，使用保守策略",
            "actions": [],
        }

        if not self.enabled:
            return disabled_result

        try:
            result = self.trainer.predict(market_data, portfolio)
            predicted_return = float(result.get("predicted_return", 0.0))
            confidence = float(result.get("confidence", 0.4))
            regime = result.get("regime", "neutral")

            if predicted_return > 0.03 and confidence > 0.6:
                recommendation = "buy"
            elif predicted_return < -0.03 and confidence > 0.6:
                recommendation = "sell"
            else:
                recommendation = "hold"

            return {
                "enabled": True,
                "horizon": horizon,
                "predicted_returns": {code: predicted_return for code in market_data},
                "cumulative_return": round(predicted_return * horizon, 4),
                "confidence": round(confidence, 4),
                "recommendation": recommendation,
                "reasoning": f"GRU预测{horizon}日收益: {predicted_return:.2%}（{regime}市场）",
                "actions": [recommendation] * horizon,
            }
        except Exception as e:
            print(f"⚠️ 世界模型预测失败: {e}")
            disabled_result["reasoning"] = f"预测失败: {e}"
            return disabled_result

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

    print("\n✅ 集成测试完成!")


if __name__ == "__main__":
    test_integration()
