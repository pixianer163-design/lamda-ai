#!/usr/bin/env python3
"""
统一配置加载器
从多个来源加载和合并配置：环境变量、YAML文件、默认值
"""

import os
import sys
import yaml
import json
from typing import Dict, Any, Optional, List
from pathlib import Path

# 导入共享常量
try:
    import constants
    SHARED_CONSTANTS_AVAILABLE = True
except ImportError:
    SHARED_CONSTANTS_AVAILABLE = False
    print("⚠️ 共享常量模块不可用，使用本地定义")
    constants = None


class ConfigLoader:
    """
    统一配置加载器
    
    配置加载顺序（优先级从高到低）：
    1. 环境变量
    2. 配置文件（YAML/JSON）
    3. 默认值
    """
    
    def __init__(self, config_dirs: List[str] = None, env_prefix: str = "AGENT_"):
        """
        初始化配置加载器
        
        Args:
            config_dirs: 配置文件目录列表
            env_prefix: 环境变量前缀
        """
        self.env_prefix = env_prefix
        
        # 确定配置文件目录
        if config_dirs is None:
            self.config_dirs = self._discover_config_dirs()
        else:
            self.config_dirs = config_dirs
            
        # 缓存配置
        self._config_cache = {}
        
        print(f"⚙️  配置加载器初始化完成")
        print(f"   配置目录: {self.config_dirs}")
        print(f"   环境变量前缀: {self.env_prefix}")
    
    def _discover_config_dirs(self) -> List[str]:
        """发现配置文件目录"""
        config_dirs = []
        
        # 项目根目录
        if SHARED_CONSTANTS_AVAILABLE and constants is not None:
            project_root = constants.PROJECT_ROOT
        else:
            # 尝试计算项目根目录
            current_dir = os.path.dirname(os.path.abspath(__file__))
            project_root = os.path.dirname(current_dir)
        
        # 可能的配置目录
        possible_dirs = [
            os.path.join(project_root, "local_config"),      # 本地开发配置
            os.path.join(project_root, "config"),            # 生产配置
            os.path.join(project_root, "factory", "configs"), # 工厂配置
            os.path.join(project_root, "prod", "config"),    # 生产环境配置
        ]
        
        # 只保留存在的目录
        for dir_path in possible_dirs:
            if os.path.exists(dir_path):
                config_dirs.append(dir_path)
        
        # 如果没有任何目录存在，创建第一个
        if not config_dirs:
            os.makedirs(possible_dirs[0], exist_ok=True)
            config_dirs.append(possible_dirs[0])
            
        return config_dirs
    
    def load_config(self, config_name: str, config_type: str = "agent") -> Dict[str, Any]:
        """
        加载配置
        
        Args:
            config_name: 配置名称（如 "hktech_001" 或 "feishu"）
            config_type: 配置类型 ("agent", "feishu", "system", "strategy")
            
        Returns:
            合并后的配置字典
        """
        cache_key = f"{config_type}_{config_name}"
        if cache_key in self._config_cache:
            return self._config_cache[cache_key].copy()
        
        # 1. 加载默认配置
        default_config = self._get_default_config(config_type)
        
        # 2. 加载文件配置
        file_config = self._load_file_config(config_name, config_type)
        
        # 3. 加载环境变量配置
        env_config = self._load_env_config(config_type)
        
        # 4. 合并配置（优先级：环境变量 > 文件配置 > 默认值）
        merged_config = self._deep_merge(default_config, file_config)
        merged_config = self._deep_merge(merged_config, env_config)
        
        # 5. 验证配置
        self._validate_config(merged_config, config_type)
        
        # 缓存结果
        self._config_cache[cache_key] = merged_config.copy()
        
        return merged_config
    
    def _get_default_config(self, config_type: str) -> Dict[str, Any]:
        """获取默认配置"""
        if config_type == "agent":
            return {
                "agent_id": "default_agent",
                "name": "默认Agent",
                "environment": "local",
                "stocks": self._get_default_stocks_config(),
                "risk_management": self._get_default_risk_config(),
                "strategy": self._get_default_strategy_config(),
                "llm": self._get_default_llm_config(),
                "schedule": self._get_default_schedule_config(),
                "data_source": "yfinance",
                "log_level": "INFO"
            }
        elif config_type == "feishu":
            return {
                "enabled": False,
                "app_id": "",
                "app_secret": "",
                "chat_id": "",
                "webhook_url": "",
                "notification_level": "info"
            }
        elif config_type == "system":
            return {
                "data_dir": self._get_default_data_dir(),
                "log_dir": self._get_default_log_dir(),
                "cache_dir": self._get_default_cache_dir(),
                "max_workers": 4,
                "timeout": 30
            }
        elif config_type == "strategy":
            return {
                "type": "multi_factor",
                "factors": {
                    "technical": 0.4,
                    "fundamental": 0.3,
                    "sentiment": 0.3
                },
                "parameters": {
                    "rsi_period": 14,
                    "ma_fast": 10,
                    "ma_slow": 50,
                    "bollinger_period": 20,
                    "bollinger_std": 2.0
                }
            }
        else:
            return {}
    
    def _get_default_stocks_config(self) -> List[Dict[str, Any]]:
        """获取默认股票配置"""
        stocks_config = []
        
        if SHARED_CONSTANTS_AVAILABLE and constants is not None:
            stock_codes = constants.DEFAULT_STOCKS
            for code in stock_codes:
                stock_info = constants.STOCKS.get(code, {})
                stocks_config.append({
                    "code": code,
                    "name": stock_info.get("name", code),
                    "sector": stock_info.get("sector", "未知"),
                    "weight": 1.0 / len(stock_codes),
                    "stop_loss": 0.08,
                    "take_profit": 0.15
                })
        else:
            # 本地回退
            stock_codes = constants.DEFAULT_STOCKS if SHARED_CONSTANTS_AVAILABLE and constants is not None else ["00700", "09988", "03690"]
            for code in stock_codes:
                stocks_config.append({
                    "code": code,
                    "name": constants.STOCK_NAMES.get(code, code) if SHARED_CONSTANTS_AVAILABLE and constants is not None else {"00700": "腾讯控股", "09988": "阿里巴巴", "03690": "美团-W"}.get(code, code),
                    "sector": "未知",
                    "weight": 1.0 / 3,
                    "stop_loss": 0.08,
                    "take_profit": 0.15
                })
        
        return stocks_config
    
    def _get_default_risk_config(self) -> Dict[str, Any]:
        """获取默认风险控制配置"""
        return {
            "position_control": {
                "max_single_stock_weight": 0.40,
                "max_total_position": 0.80,
                "min_cash_ratio": 0.20,
                "max_positions": 5
            },
            "stop_loss_take_profit": {
                "stop_loss_pct": -0.08,
                "take_profit_pct": 0.15
            }
        }
    
    def _get_default_strategy_config(self) -> Dict[str, Any]:
        """获取默认策略配置"""
        return {
            "type": "multi_factor",
            "factors": {
                "technical": 0.4,
                "fundamental": 0.3,
                "sentiment": 0.3
            }
        }
    
    def _get_default_llm_config(self) -> Dict[str, Any]:
        """获取默认LLM配置"""
        return {
            "model": "deepseek-chat",
            "temperature": 0.7,
            "max_tokens": 2000,
            "api_key": "",
            "api_base": "https://api.deepseek.com"
        }
    
    def _get_default_schedule_config(self) -> Dict[str, Any]:
        """获取默认调度配置"""
        return {
            "pre_market": "09:00",
            "midday": "12:30",
            "post_market": "16:30",
            "interval_minutes": 60
        }
    
    def _get_default_data_dir(self) -> str:
        """获取默认数据目录"""
        if SHARED_CONSTANTS_AVAILABLE and constants is not None:
            return constants.get_data_dir()
        else:
            # 尝试计算
            current_dir = os.path.dirname(os.path.abspath(__file__))
            return os.path.join(os.path.dirname(current_dir), "data")
    
    def _get_default_log_dir(self) -> str:
        """获取默认日志目录"""
        if SHARED_CONSTANTS_AVAILABLE and constants is not None:
            return constants.get_log_dir()
        else:
            current_dir = os.path.dirname(os.path.abspath(__file__))
            return os.path.join(os.path.dirname(current_dir), "prod", "logs")
    
    def _get_default_cache_dir(self) -> str:
        """获取默认缓存目录"""
        if SHARED_CONSTANTS_AVAILABLE and constants is not None:
            project_root = constants.PROJECT_ROOT
        else:
            current_dir = os.path.dirname(os.path.abspath(__file__))
            project_root = os.path.dirname(current_dir)
        
        cache_dir = os.path.join(project_root, "cache")
        os.makedirs(cache_dir, exist_ok=True)
        return cache_dir
    
    def _load_file_config(self, config_name: str, config_type: str) -> Dict[str, Any]:
        """从文件加载配置"""
        config = {}
        
        # 可能的文件名
        possible_files = []
        for config_dir in self.config_dirs:
            possible_files.extend([
                os.path.join(config_dir, f"{config_name}.yaml"),
                os.path.join(config_dir, f"{config_name}.yml"),
                os.path.join(config_dir, f"{config_name}.json"),
                os.path.join(config_dir, f"{config_type}.yaml"),
                os.path.join(config_dir, f"{config_type}.yml"),
                os.path.join(config_dir, f"{config_type}.json"),
            ])
        
        # 尝试加载第一个存在的文件
        for file_path in possible_files:
            if os.path.exists(file_path):
                try:
                    if file_path.endswith('.json'):
                        with open(file_path, 'r', encoding='utf-8') as f:
                            file_config = json.load(f)
                    else:
                        with open(file_path, 'r', encoding='utf-8') as f:
                            file_config = yaml.safe_load(f)
                    
                    print(f"   📁 加载配置文件: {file_path}")
                    config = self._deep_merge(config, file_config)
                    break  # 只加载第一个匹配的文件
                    
                except Exception as e:
                    print(f"   ❌ 配置文件加载失败 {file_path}: {e}")
        
        return config
    
    def _load_env_config(self, config_type: str) -> Dict[str, Any]:
        """从环境变量加载配置"""
        config = {}
        prefix = self.env_prefix
        
        # 映射环境变量到配置结构
        env_mappings = {
            "AGENT_ENV": ["environment"],
            "AGENT_DATA_DIR": ["data_dir"],
            "AGENT_LOG_DIR": ["log_dir"],
            "FEISHU_WEBHOOK_URL": ["feishu", "webhook_url"],
            "FEISHU_APP_ID": ["feishu", "app_id"],
            "FEISHU_APP_SECRET": ["feishu", "app_secret"],
            "FEISHU_CHAT_ID": ["feishu", "chat_id"],
            "DEEPSEEK_API_KEY": ["llm", "api_key"],
            "DEEPSEEK_API_BASE": ["llm", "api_base"],
            "LLM_MODEL": ["llm", "model"],
            "LOG_LEVEL": ["log_level"],
        }
        
        for env_var, config_path in env_mappings.items():
            value = os.environ.get(env_var)
            if value is not None:
                # 设置嵌套配置
                current = config
                for key in config_path[:-1]:
                    if key not in current:
                        current[key] = {}
                    current = current[key]
                current[config_path[-1]] = value
        
        return config
    
    def _deep_merge(self, base: Dict, override: Dict) -> Dict:
        """深度合并字典"""
        result = base.copy()
        
        for key, value in override.items():
            if key in result and isinstance(result[key], dict) and isinstance(value, dict):
                result[key] = self._deep_merge(result[key], value)
            else:
                result[key] = value
        
        return result
    
    def _validate_config(self, config: Dict[str, Any], config_type: str):
        """验证配置完整性"""
        if config_type == "agent":
            required_fields = ["agent_id", "name", "environment", "stocks"]
            for field in required_fields:
                if field not in config:
                    raise ValueError(f"Agent配置缺少必需字段: {field}")
            
            if not config["stocks"]:
                raise ValueError("股票列表不能为空")
        
        elif config_type == "feishu":
            # 飞书配置验证
            if config.get("enabled", False):
                required_fields = ["app_id", "app_secret", "chat_id"]
                for field in required_fields:
                    if not config.get(field):
                        raise ValueError(f"飞书配置缺少必需字段: {field}")
        
        # 其他验证...
        return True
    
    def save_config(self, config: Dict[str, Any], config_name: str, config_type: str = "agent"):
        """
        保存配置到文件
        
        Args:
            config: 配置字典
            config_name: 配置名称
            config_type: 配置类型
        """
        # 确定保存目录（使用第一个配置目录）
        if not self.config_dirs:
            raise ValueError("没有可用的配置目录")
        
        save_dir = self.config_dirs[0]
        os.makedirs(save_dir, exist_ok=True)
        
        # 保存文件
        file_path = os.path.join(save_dir, f"{config_name}.yaml")
        with open(file_path, 'w', encoding='utf-8') as f:
            yaml.dump(config, f, allow_unicode=True, default_flow_style=False)
        
        print(f"💾 配置已保存: {file_path}")
        
        # 清除缓存
        cache_key = f"{config_type}_{config_name}"
        self._config_cache.pop(cache_key, None)
    
    def get_agent_config(self, agent_id: str) -> Dict[str, Any]:
        """获取Agent配置（便捷方法）"""
        return self.load_config(agent_id, "agent")
    
    def get_feishu_config(self) -> Dict[str, Any]:
        """获取飞书配置（便捷方法）"""
        return self.load_config("feishu", "feishu")
    
    def get_system_config(self) -> Dict[str, Any]:
        """获取系统配置（便捷方法）"""
        return self.load_config("system", "system")
    
    def get_strategy_config(self, strategy_name: str = "default") -> Dict[str, Any]:
        """获取策略配置（便捷方法）"""
        return self.load_config(strategy_name, "strategy")


# 全局配置加载器实例
_config_loader = None

def get_config_loader() -> ConfigLoader:
    """获取全局配置加载器实例"""
    global _config_loader
    if _config_loader is None:
        _config_loader = ConfigLoader()
    return _config_loader

def load_agent_config(agent_id: str) -> Dict[str, Any]:
    """加载Agent配置（全局函数）"""
    return get_config_loader().get_agent_config(agent_id)

def load_feishu_config() -> Dict[str, Any]:
    """加载飞书配置（全局函数）"""
    return get_config_loader().get_feishu_config()

def load_system_config() -> Dict[str, Any]:
    """加载系统配置（全局函数）"""
    return get_config_loader().get_system_config()


# 测试代码
if __name__ == "__main__":
    print("="*60)
    print("⚙️  ConfigLoader 测试")
    print("="*60)
    
    # 创建配置加载器
    loader = ConfigLoader()
    
    # 测试系统配置
    print("\n1️⃣ 系统配置:")
    system_config = loader.get_system_config()
    print(f"   数据目录: {system_config.get('data_dir')}")
    print(f"   日志目录: {system_config.get('log_dir')}")
    
    # 测试Agent配置
    print("\n2️⃣ Agent配置 (hktech_001):")
    try:
        agent_config = loader.get_agent_config("hktech_001")
        print(f"   Agent名称: {agent_config.get('name')}")
        print(f"   股票数量: {len(agent_config.get('stocks', []))}")
        print(f"   环境: {agent_config.get('environment')}")
    except Exception as e:
        print(f"   ⚠️  Agent配置加载失败: {e}")
    
    # 测试默认Agent配置
    print("\n3️⃣ 默认Agent配置:")
    default_agent = loader.get_agent_config("default")
    print(f"   Agent名称: {default_agent.get('name')}")
    print(f"   股票数量: {len(default_agent.get('stocks', []))}")
    
    # 测试飞书配置
    print("\n4️⃣ 飞书配置:")
    feishu_config = loader.get_feishu_config()
    print(f"   飞书启用: {feishu_config.get('enabled')}")
    print(f"   App ID: {feishu_config.get('app_id')[:10] if feishu_config.get('app_id') else '未设置'}")
    
    print("\n✅ ConfigLoader 测试完成！")