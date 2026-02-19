#!/usr/bin/env python3
"""
RSSM世界模型 - 支持真实模式与虚拟模式
根据torch可用性自动选择实现
"""

import json
import os
import sys
from datetime import datetime
from typing import Dict, List, Any, Optional

# 导入共享常量
SHARED_CONSTANTS_AVAILABLE = False
constants = None  # 默认值
try:
    import constants
    SHARED_CONSTANTS_AVAILABLE = True
except ImportError:
    print("⚠️ 共享常量模块不可用，使用本地定义")

# ============================================================================
# 动态选择实现：优先使用真实PyTorch模型，否则回退到虚拟实现
# ============================================================================

TORCH_AVAILABLE = False
try:
    import torch
    import torch.nn as nn
    import torch.nn.functional as F
    import numpy as np
    TORCH_AVAILABLE = True
    print("✅ PyTorch可用，启用真实世界模型")
except ImportError:
    print("⚠️ PyTorch不可用，使用虚拟世界模型")
    # 设置占位符，虚拟实现不依赖这些模块
    torch = None
    nn = None
    F = None
    np = None

# ── GRU World Model (replaces complex RSSM) ──────────────────────────────────
if TORCH_AVAILABLE:
    import torch.nn as nn

    class GRUWorldModel(nn.Module):
        """简化 GRU 世界模型：预测未来 5 日收益率"""
        def __init__(self, input_size=8, hidden_size=64, num_layers=2, dropout=0.2):
            super().__init__()
            self.gru = nn.GRU(
                input_size=input_size,
                hidden_size=hidden_size,
                num_layers=num_layers,
                dropout=dropout if num_layers > 1 else 0.0,
                batch_first=True
            )
            self.fc = nn.Linear(hidden_size, 1)

        def forward(self, x):
            # x: (batch, seq_len, input_size) → (batch, 1)
            out, _ = self.gru(x)
            return self.fc(out[:, -1, :])
else:
    class GRUWorldModel:
        """torch 不可用时的占位类"""
        def __init__(self, *args, **kwargs):
            pass

# 根据TORCH_AVAILABLE选择导入真实或虚拟实现
if TORCH_AVAILABLE:
    # 尝试导入真实实现
    try:
        from rssm_world_model_real import RSSM, ActorCritic, WorldModelTrainer
        print("✅ 成功导入真实RSSM世界模型")
        REAL_MODEL_LOADED = True
    except ImportError as e:
        print(f"⚠️ 导入真实模型失败: {e}，使用虚拟实现")
        REAL_MODEL_LOADED = False
else:
    REAL_MODEL_LOADED = False

# 如果真实模型未加载，定义虚拟实现
if not REAL_MODEL_LOADED:
    # ============================================================================
    # 虚拟RSSM类
    # ============================================================================
    class RSSM:
        """虚拟RSSM类"""
        
        def __init__(self, obs_dim=15, action_dim=3, hidden_dim=64, latent_dim=32, latent_classes=32):
            self.obs_dim = obs_dim
            self.action_dim = action_dim
            self.hidden_dim = hidden_dim
            self.latent_dim = latent_dim
            self.latent_classes = latent_classes
            self.latent_flat_dim = latent_dim * latent_classes
        
        def train(self, mode=True):
            return self
        
        def eval(self):
            return self

    # ============================================================================
    # 虚拟ActorCritic类
    # ============================================================================
    class ActorCritic:
        """虚拟ActorCritic类"""
        
        def __init__(self, hidden_dim=64, latent_flat_dim=1024, action_dim=3):
            pass

    # ============================================================================
    # 虚拟WorldModelTrainer类
    # ============================================================================
    class WorldModelTrainer:
        """
        虚拟世界模型训练器
        提供与真实WorldModelTrainer相同的接口，但返回模拟数据
        """
        
        def __init__(self, data_dir=None, device="cpu"):
            if data_dir is None:
                try:
                    import sys as _sys, os as _os
                    _sys.path.insert(0, _os.path.join(_os.path.dirname(_os.path.abspath(__file__)), '../../shared'))
                    from config import get_config
                    data_dir = str(get_config().data_dir)
                except Exception:
                    import os as _os
                    data_dir = _os.path.join(_os.path.dirname(_os.path.abspath(__file__)), '../../data')
            self.data_dir = data_dir
            self.device = device
            self.rssm = RSSM()
            self.actor_critic = ActorCritic()
            self.model_path = f"{data_dir}/rssm_model.pt"
        
        def prepare_data(self, market_data: Dict, portfolio: Dict) -> list:
            """准备观测向量"""
            obs_list = []
            
            # 使用共享常量或本地定义
            if SHARED_CONSTANTS_AVAILABLE and constants is not None:
                stock_codes = constants.DEFAULT_STOCKS
            else:
                stock_codes = ["00700", "09988", "03690"]
            
            for code in stock_codes:
                if code in market_data:
                    data = market_data[code]
                    obs_list.extend([
                        data.get('price', 0) / 500,
                        data.get('ma5', 0) / 500,
                        data.get('ma20', 0) / 500,
                        data.get('rsi', 50) / 100,
                        data.get('change_pct', 0) / 10
                    ])
                else:
                    obs_list.extend([0, 0, 0, 0.5, 0])
            
            return obs_list
        
        def train_world_model(self, episodes: List[Dict], epochs=50):
            """虚拟训练方法"""
            print("⚠️  世界模型训练 (虚拟模式): torch不可用，使用模拟训练")
            return [0.1] * epochs  # 返回模拟损失
        
        def imagine_future(self, initial_obs: list, initial_action: list, horizon=5) -> Dict:
            """虚拟未来预测 (返回与原始模型相同的结构)"""
            # 简单的启发式预测: 基于初始观测中的RSI和价格趋势
            if len(initial_obs) >= 15:
                rsi_indices = [3, 8, 13]
                change_indices = [4, 9, 14]
                avg_rsi = sum(initial_obs[i] * 100 for i in rsi_indices) / 3
                avg_change = sum(initial_obs[i] * 10 for i in change_indices) / 3
                
                base_return = 0.0
                
                if avg_rsi > 60:
                    base_return -= (avg_rsi - 60) * 0.0005
                elif avg_rsi < 40:
                    base_return += (40 - avg_rsi) * 0.0005
                
                base_return += avg_change * 0.3
                
                import random
                random_return = random.uniform(-0.001, 0.001)
                base_return += random_return
            else:
                base_return = 0.001
            
            trajectory = []
            cumulative_reward = 0.0
            
            for step in range(horizon):
                step_return = base_return * (1.0 - step / (horizon * 1.5))
                
                step_random = random.uniform(-0.0005, 0.0005) * (horizon - step) / horizon
                step_return += step_random
                
                trajectory.append({
                    'step': step,
                    'predicted_reward': step_return,
                    'action': [0.0, 0.0, 0.0]
                })
                
                cumulative_reward += step_return
            
            return {
                'horizon': horizon,
                'trajectory': trajectory,
                'cumulative_reward': cumulative_reward
            }
        
        def load(self) -> bool:
            """虚拟加载方法"""
            print("⚠️  世界模型加载: 虚拟模式 (torch不可用)")
            return True
        
        def save(self):
            """虚拟保存方法"""
            print("⚠️  世界模型保存: 虚拟模式 (无操作)")
        
        def predict(self, market_data: Dict, portfolio: Dict) -> Dict:
            """虚拟预测方法"""
            return {
                'enabled': False,
                'message': '世界模型虚拟模式 (torch不可用)',
                'predicted_return': 0.0,
                'confidence': 0.0
            }

# ============================================================================
# 高层世界模型包装器 (兼容测试)
# ============================================================================
class RSSMWorldModel:
    """
    高层世界模型包装器
    提供统一接口，内部使用真实或虚拟模型

    predict() 返回格式 (Task 6 接口):
        {
            "predicted_return": float,   # 预期收益率 [-1, 1]
            "confidence": float,         # 置信度 [0, 1]
            "regime": str,               # "bullish" | "bearish" | "neutral"
            "source": str,               # 数据来源标识
        }

    当 self.enabled = False 时返回 {}（向后兼容禁用逻辑）。
    """
    
    def __init__(self, data_dir=None):
        if data_dir is None:
            try:
                import sys as _sys, os as _os
                _sys.path.insert(0, _os.path.join(_os.path.dirname(_os.path.abspath(__file__)), '../../shared'))
                from config import get_config
                data_dir = str(get_config().data_dir)
            except Exception:
                import os as _os
                data_dir = _os.path.join(_os.path.dirname(_os.path.abspath(__file__)), '../../data')
        self.data_dir = data_dir
        self.enabled = True  # 始终启用（虚拟或真实）
        
        # 使用共享常量或本地定义
        if SHARED_CONSTANTS_AVAILABLE and constants is not None:
            self.stocks = constants.DEFAULT_STOCKS
        else:
            self.stocks = ["00700", "09988", "03690"]
        
        # 检查是否有真实的 GRU 模型文件
        self.gru_model_path = os.path.join(data_dir, "gru_world_model.pt")
        self.gru_model = None
        self._try_load_gru_model()
        
        # 内部使用WorldModelTrainer（保留向后兼容）
        self.trainer = WorldModelTrainer(data_dir=data_dir)
        self.trainer.load()
    
    def _try_load_gru_model(self):
        """尝试加载 GRU 模型文件"""
        if TORCH_AVAILABLE and os.path.exists(self.gru_model_path):
            try:
                self.gru_model = GRUWorldModel()
                self.gru_model.load_state_dict(torch.load(self.gru_model_path, map_location="cpu"))
                self.gru_model.eval()
                print(f"✅ GRU 世界模型已加载: {self.gru_model_path}")
            except Exception as e:
                print(f"⚠️ GRU 模型加载失败: {e}，将使用技术指标 fallback")
                self.gru_model = None
        else:
            self.gru_model = None
    
    def predict(self, market_data: Dict, historical_data: Dict = None) -> Dict:
        """
        预测市场走势。

        当 self.enabled = False 时返回 {}（向后兼容）。

        如果 GRU 模型文件存在且 torch 可用，使用真实 GRU 模型；
        否则 fallback 到技术指标规则（确定性，无随机数）。

        返回格式:
            {
                "predicted_return": float,   # 预期收益率 [-1, 1]
                "confidence": float,         # 置信度 [0, 1]
                "regime": str,               # "bullish" | "bearish" | "neutral"
                "source": str,               # 数据来源标识
            }
        """
        if not self.enabled:
            return {}

        if not market_data:
            return {}

        if self.gru_model is not None and TORCH_AVAILABLE:
            return self._predict_with_gru(market_data)
        else:
            return self._predict_technical_fallback(market_data)

    def _predict_technical_fallback(self, market_data: dict) -> dict:
        """基于技术指标的 fallback 预测（无随机数）"""
        scores = []
        for code, data in market_data.items():
            rsi = data.get("rsi", 50)
            ma5 = data.get("ma5", data.get("price", 100))
            ma20 = data.get("ma20", data.get("price", 100))

            if rsi > 70:
                rsi_signal = -0.03
            elif rsi < 30:
                rsi_signal = 0.03
            else:
                rsi_signal = (50 - rsi) * 0.0006

            ma_signal = (ma5 - ma20) / ma20 if ma20 > 0 else 0.0
            scores.append(rsi_signal + ma_signal * 0.5)

        avg_return = sum(scores) / len(scores) if scores else 0.0
        regime = "bullish" if avg_return > 0.02 else ("bearish" if avg_return < -0.02 else "neutral")
        return {
            "predicted_return": round(float(avg_return), 4),
            "confidence": 0.4,
            "regime": regime,
            "source": "technical_fallback"
        }

    def _predict_with_gru(self, market_data: dict) -> dict:
        """使用已加载的 GRU 模型进行预测"""
        features = []
        for code, data in market_data.items():
            features.extend([
                data.get("price", 0) / 500.0,
                data.get("ma5", 0) / 500.0,
                data.get("ma20", 0) / 500.0,
                data.get("rsi", 50) / 100.0,
                data.get("change_pct", 0) / 10.0,
                data.get("volume", 0) / 1e8,
                0.0,
                0.0,
            ])
            break  # 只使用第一个股票

        features = features[:8] + [0.0] * max(0, 8 - len(features))

        x = torch.tensor(features, dtype=torch.float32).unsqueeze(0).unsqueeze(0)  # (1,1,8)
        with torch.no_grad():
            raw = self.gru_model(x).item()

        predicted_return = max(-1.0, min(1.0, raw))
        regime = "bullish" if predicted_return > 0.02 else ("bearish" if predicted_return < -0.02 else "neutral")
        return {
            "predicted_return": round(float(predicted_return), 4),
            "confidence": 0.7,
            "regime": regime,
            "source": "gru_model"
        }
    
    def _predict_virtual(self, market_data: Dict, historical_data: Dict = None) -> Dict:
        """虚拟预测逻辑（保留向后兼容，委托给 _predict_technical_fallback）"""
        return self._predict_technical_fallback(market_data)
    
    def _predict_with_real_model(self, market_data: Dict, historical_data: Dict = None) -> Dict:
        """真实模型预测逻辑 (待实现)"""
        print("🧠 真实世界模型预测 (待完全集成)")
        return self._predict_technical_fallback(market_data)
    
    def identify_scenarios(self, market_data: Dict) -> List[Dict]:
        """识别市场情景"""
        return [
            {
                "name": "平稳市场",
                "probability": 0.6,
                "description": "市场波动率较低，趋势不明显"
            },
            {
                "name": "技术性反弹",
                "probability": 0.3,
                "description": "RSI超卖后可能出现反弹"
            },
            {
                "name": "回调风险",
                "probability": 0.1,
                "description": "RSI超买后可能出现回调"
            }
        ]

# ============================================================================
# 测试函数
# ============================================================================
def test_world_model():
    """测试世界模型"""
    print("🧪 测试世界模型...")
    
    model = RSSMWorldModel()
    
    market_data = {
        "00700": {"price": 385.0, "rsi": 65, "change_pct": 1.5},
        "09988": {"price": 85.0, "rsi": 45, "change_pct": -0.8},
        "03690": {"price": 130.0, "rsi": 70, "change_pct": 2.1}
    }
    
    result = model.predict(market_data)
    print(f"📊 预测结果: {result}")
    
    scenarios = model.identify_scenarios(market_data)
    print(f"🔮 市场情景:")
    for scenario in scenarios:
        print(f"  {scenario['name']}: {scenario['probability']*100}% - {scenario['description']}")
    
    print("✅ 测试完成")


if __name__ == "__main__":
    test_world_model()
