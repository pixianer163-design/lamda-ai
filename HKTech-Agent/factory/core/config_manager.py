#!/usr/bin/env python3
"""
Agent Factory - 配置管理器
管理Agent配置的加载、验证和合并
"""

import os
import sys
import json
import yaml
from typing import Dict, Any, Optional, List
from dataclasses import dataclass, asdict

project_root = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
shared_path = os.path.join(project_root, "shared")
factory_path = os.path.join(project_root, "factory")

for path in [shared_path, factory_path]:
    if path not in sys.path:
        sys.path.insert(0, path)

try:
    from shared import constants
    SHARED_CONSTANTS_AVAILABLE = True
except ImportError:
    SHARED_CONSTANTS_AVAILABLE = False
    print("⚠️ 共享常量模块不可用，使用本地定义")
    constants = None

try:
    from factory.templates import get_template, AgentTemplate
except ImportError:
    from templates.agent_templates import get_template, AgentTemplate


class ConfigManager:
    """
    配置管理器
    
    职责：
    1. 从模板生成基础配置
    2. 加载YAML配置文件
    3. 合并配置（模板 < 文件 < 运行时覆盖）
    4. 验证配置完整性
    """
    
    def __init__(self, config_dir: Optional[str] = None):
        if config_dir is None:
            possible_paths = [
                os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(__file__))), "factory", "configs"),
                os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__)))), "factory", "configs"),
            ]
            for path in possible_paths:
                if os.path.exists(path):
                    config_dir = path
                    break
            if config_dir is None:
                config_dir = possible_paths[0]
        
        self.config_dir = config_dir
        os.makedirs(config_dir, exist_ok=True)
    
    def create_config(
        self,
        agent_id: str,
        template_name: str,
        overrides: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        创建Agent配置
        
        Args:
            agent_id: Agent唯一标识
            template_name: 模板名称
            overrides: 配置覆盖项
            
        Returns:
            完整配置字典
        """
        template = get_template(template_name)
        
        if template is None:
            raise ValueError(f"模板不存在: {template_name}")
        
        # 2. 转换为字典
        config = {
            "agent_id": agent_id,
            "name": f"{template.name}-{agent_id[-3:]}",
            "template": template_name,
            "market": template.market,
            "stocks": self._build_stocks_config(template.default_stocks, overrides),
            "risk_management": template.risk_config.copy(),
            "strategy": {
                "type": template.strategy_type,
                **template.strategy_config
            },
            "llm": {
                "model": template.llm_model
            },
            "schedule": template.schedule,
            "created_at": None  # 创建时填充
        }
        
        # 3. 应用覆盖
        if overrides:
            config = self._deep_merge(config, overrides)
        
        # 4. 验证
        self._validate_config(config)
        
        return config
    
    def load_config(self, agent_id: str) -> Optional[Dict[str, Any]]:
        """从文件加载配置"""
        config_file = os.path.join(self.config_dir, f"{agent_id}.yaml")
        
        if not os.path.exists(config_file):
            return None
        
        with open(config_file, 'r', encoding='utf-8') as f:
            return yaml.safe_load(f)
    
    def save_config(self, agent_id: str, config: Dict[str, Any]):
        """保存配置到文件"""
        config_file = os.path.join(self.config_dir, f"{agent_id}.yaml")
        
        with open(config_file, 'w', encoding='utf-8') as f:
            yaml.dump(config, f, allow_unicode=True, default_flow_style=False)
        
        print(f"💾 配置已保存: {config_file}")
    
    def _build_stocks_config(
        self,
        stock_codes: list,
        overrides: Optional[Dict[str, Any]] = None
    ) -> list:
        """构建股票配置"""
        if SHARED_CONSTANTS_AVAILABLE and constants is not None:
            stocks = []
            for code in stock_codes:
                stock_info = constants.STOCKS.get(code, {})
                
                stock_config = {
                    "code": code,
                    "name": stock_info.get("name", code),
                    "sector": stock_info.get("sector", "未知"),
                    "weight": 1.0 / len(stock_codes),  # 默认等权
                    "stop_loss": 0.08,
                    "take_profit": 0.15
                }
                stocks.append(stock_config)
        else:
            # 本地回退定义
            stock_info = {
                "00700": {"name": "腾讯控股", "sector": "互联网"},
                "09988": {"name": "阿里巴巴", "sector": "电商"},
                "03690": {"name": "美团-W", "sector": "本地生活"},
                "01810": {"name": "小米集团-W", "sector": "硬件"}
            }
            
            stocks = []
            for code in stock_codes:
                info = stock_info.get(code, {"name": code, "sector": "未知"})
                
                stock_config = {
                    "code": code,
                    "name": info["name"],
                    "sector": info["sector"],
                    "weight": 1.0 / len(stock_codes),  # 默认等权
                    "stop_loss": 0.08,
                    "take_profit": 0.15
                }
                stocks.append(stock_config)
        
        # 应用覆盖
        if overrides and "stocks" in overrides:
            for override_stock in overrides["stocks"]:
                for stock in stocks:
                    if stock["code"] == override_stock.get("code"):
                        stock.update(override_stock)
        
        return stocks
    
    def _deep_merge(self, base: Dict, override: Dict) -> Dict:
        """深度合并字典"""
        result = base.copy()
        
        for key, value in override.items():
            if key in result and isinstance(result[key], dict) and isinstance(value, dict):
                result[key] = self._deep_merge(result[key], value)
            else:
                result[key] = value
        
        return result
    
    def _validate_config(self, config: Dict) -> bool:
        """验证配置完整性"""
        required_fields = ["agent_id", "name", "market", "stocks", "risk_management"]
        
        for field in required_fields:
            if field not in config:
                raise ValueError(f"配置缺少必需字段: {field}")
        
        if not config["stocks"]:
            raise ValueError("股票列表不能为空")
        
        return True
    
    def list_configs(self) -> list:
        """列出所有配置"""
        configs = []
        for filename in os.listdir(self.config_dir):
            if filename.endswith('.yaml'):
                agent_id = filename[:-5]
                config = self.load_config(agent_id)
                if config:
                    configs.append({
                        "agent_id": agent_id,
                        "name": config.get("name"),
                        "template": config.get("template")
                    })
        return configs


# YAML配置示例
CONFIG_EXAMPLE = """
# Agent配置示例
agent_id: "hktech_001"
name: "恒生基础-001"
template: "hktech_basic"

market: "HK"

stocks:
  - code: "00700"
    name: "腾讯控股"
    weight: 0.4
    stop_loss: 0.08
    take_profit: 0.15
  
  - code: "09988"
    name: "阿里巴巴"
    weight: 0.35
    stop_loss: 0.08
    take_profit: 0.15

  - code: "03690"
    name: "美团"
    weight: 0.25
    stop_loss: 0.08
    take_profit: 0.15

risk_management:
  position_control:
    max_single_stock_weight: 0.40
    max_total_position: 0.80
    min_cash_ratio: 0.20
  
  stop_loss_take_profit:
    stop_loss_pct: -0.08
    take_profit_pct: 0.15

strategy:
  type: "multi_factor"
  factors:
    technical: 0.4
    fundamental: 0.3
    sentiment: 0.3

llm:
  model: "deepseek-chat"

schedule:
  pre_market: "09:00"
  midday: "12:30"
  post_market: "16:30"

notification:
  channel: "feishu"
  chat_id: "oc_d5f6f6f591bc129e4ae9037b0acdd3a5"
"""


if __name__ == "__main__":
    # 测试
    print("="*60)
    print("⚙️  ConfigManager 测试")
    print("="*60)
    
    manager = ConfigManager()
    
    # 测试创建配置
    print("\n1️⃣ 创建配置（基础模板）")
    config = manager.create_config(
        agent_id="test_basic",
        template_name="hktech_basic"
    )
    print(f"   Agent: {config['name']}")
    print(f"   股票: {len(config['stocks'])} 只")
    print(f"   风控: {config['risk_management']}")
    
    # 测试自定义覆盖
    print("\n2️⃣ 创建配置（自定义覆盖）")
    config_custom = manager.create_config(
        agent_id="test_custom",
        template_name="hktech_basic",
        overrides={
            "name": "我的自定义Agent",
            "stocks": [
                {"code": "00700", "weight": 0.6},
                {"code": "09988", "weight": 0.4}
            ]
        }
    )
    print(f"   Agent: {config_custom['name']}")
    print(f"   腾讯权重: {config_custom['stocks'][0]['weight']}")
    
    # 测试保存
    print("\n3️⃣ 保存配置")
    manager.save_config("test_basic", config)
    
    # 测试加载
    print("\n4️⃣ 加载配置")
    loaded = manager.load_config("test_basic")
    if loaded:
        print(f"   加载成功: {loaded['name']}")
    else:
        print("   加载失败")
    
    print("\n✅ ConfigManager 测试完成！")
