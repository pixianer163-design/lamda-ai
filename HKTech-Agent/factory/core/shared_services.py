#!/usr/bin/env python3
"""
Agent Factory - 共享服务容器
整合现有组件，提供统一接口
"""

import os
import sys
from typing import Dict, Optional

# 添加现有代码路径
sys.path.insert(0, '/opt/hktech-agent')
sys.path.insert(0, '/opt/hktech-agent/active_src')

# 延迟导入现有组件
class SharedServices:
    """
    共享服务容器（单例模式）
    
    整合现有组件：
    - 记忆系统 (memory_system.py)
    - LLM客户端 (llm_client.py)
    - 数据采集器 (data_collector_v2.py)
    - 风控管理器 (risk_manager.py)
    - 飞书通知 (feishu_sender.py)
    """
    
    _instance = None
    
    def __new__(cls):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
            cls._instance._initialized = False
        return cls._instance
    
    def __init__(self):
        if self._initialized:
            return
        
        self._memory = None
        self._llm = None
        self._world_model = None
        self._agent_memory = None
        self._evolution_engine = None
        self._data_collectors = {}
        self._risk_managers = {}
        self._notifiers = {}
        
        self._initialized = True
        print("✅ SharedServices 初始化完成")
    
    # ============== 记忆服务 ==============
    def get_memory(self, agent_id: str):
        """获取Agent的记忆集合"""
        if self._memory is None:
            # 使用现有的记忆系统
            sys.path.insert(0, '/root/.openclaw/workspace')
            from memory_system import LocalMemory
            self._memory = LocalMemory()
        
        return self._memory.get_collection(f"agent_{agent_id}")
    
    # ============== LLM服务 ==============
    def get_llm(self, model: str = "deepseek-chat"):
        """获取LLM客户端"""
        if self._llm is None:
            from llm_client import DeepSeekClient
            self._llm = DeepSeekClient()
        return self._llm
    
    # ============== 世界模型服务 ==============
    def get_world_model(self):
        """获取世界模型（增强版）"""
        if self._world_model is None:
            sys.path.insert(0, '/opt/hktech-agent/prod/src')
            # 使用适配器，内部调用增强版实现
            from world_model_adapter import WorldModel
            self._world_model = WorldModel()
            print("✅ WorldModel Enhanced 已加载")
        return self._world_model
    
    # ============== 情景记忆服务 ==============
    def get_agent_memory(self, agent_id: str):
        """获取Agent情景记忆"""
        sys.path.insert(0, '/opt/hktech-agent/prod/src')
        from agent_memory import AgentMemory
        
        # 每个Agent独立的记忆集合
        memory_dir = f"/opt/hktech-agent/data/memory/{agent_id}"
        return AgentMemory(memory_dir)
    
    # ============== 进化引擎服务 ==============
    def get_evolution_engine(self, agent_id: str = None):
        """获取进化引擎"""
        if self._evolution_engine is None:
            sys.path.insert(0, '/opt/hktech-agent/prod/src')
            from evolution_engine import EvolutionEngine
            
            data_dir = f"/opt/hktech-agent/data/evolution/{agent_id}" if agent_id else "/opt/hktech-agent/data"
            os.makedirs(data_dir, exist_ok=True)
            self._evolution_engine = EvolutionEngine(data_dir)
            print("✅ EvolutionEngine 已加载")
        return self._evolution_engine
    
    # ============== 数据服务 ==============
    def get_data_collector(self, market: str = "HK", stocks: list = None):
        """获取数据采集器"""
        cache_key = f"{market}_{','.join(sorted(stocks or []))}"
        
        if cache_key not in self._data_collectors:
            if market == "HK":
                from data_collector_v2 import HKStockDataCollector, DataSourceConfig
                
                # 如果指定了股票，临时修改配置
                if stocks:
                    # 创建自定义配置
                    config = DataSourceConfig()
                    filtered_stocks = {k: v for k, v in config.STOCKS.items() if k in stocks}
                    if filtered_stocks:
                        config.STOCKS = filtered_stocks
                
                collector = HKStockDataCollector()
                if stocks:
                    # 动态设置股票列表
                    collector.config.STOCKS = {k: v for k, v in collector.config.STOCKS.items() if k in stocks}
                
                self._data_collectors[cache_key] = collector
            else:
                raise ValueError(f"不支持的市场: {market}")
        
        return self._data_collectors[cache_key]
    
    # ============== 风控服务 ==============
    def get_risk_manager(self, agent_id: str, config: dict = None):
        """获取风控管理器"""
        if agent_id not in self._risk_managers:
            from risk_manager import RiskManager
            
            # 创建临时配置文件
            if config:
                import json
                config_file = f"/opt/hktech-agent/data/risk_config_{agent_id}.json"
                with open(config_file, 'w') as f:
                    json.dump(config, f)
                self._risk_managers[agent_id] = RiskManager(config_file)
            else:
                self._risk_managers[agent_id] = RiskManager()
        
        return self._risk_managers[agent_id]
    
    # ============== 通知服务 ==============
    def get_notifier(self, agent_id: str, config: dict = None):
        """获取通知器"""
        if agent_id not in self._notifiers:
            # 复用现有的飞书发送器
            sys.path.insert(0, '/opt/hktech-agent/prod/src')
            try:
                from feishu_sender import FeishuSender
                self._notifiers[agent_id] = FeishuSender()
            except ImportError:
                # 降级：使用简化版通知器
                self._notifiers[agent_id] = SimpleNotifier(config)
        
        return self._notifiers[agent_id]


class SimpleNotifier:
    """简化版通知器（降级方案）"""
    
    def __init__(self, config: dict = None):
        self.config = config or {}
    
    def send_message(self, message: str, msg_type: str = "text"):
        """发送消息（仅打印）"""
        print(f"[NOTIFY] {msg_type}: {message[:100]}...")
        return True
    
    def send_market_update(self, market_data: dict, trades: list = None):
        """发送市场更新"""
        print(f"[NOTIFY] Market update: {len(market_data)} stocks")
        return True


# 便捷函数
def get_services() -> SharedServices:
    """获取共享服务实例"""
    return SharedServices()


if __name__ == "__main__":
    # 测试
    print("="*60)
    print("🧪 SharedServices 测试")
    print("="*60)
    
    services = get_services()
    
    # 测试记忆
    print("\n1️⃣ 记忆服务")
    memory = services.get_memory("test_agent")
    print(f"   ✅ 记忆集合: {memory}")
    
    # 测试LLM
    print("\n2️⃣ LLM服务")
    llm = services.get_llm()
    print(f"   ✅ LLM客户端已加载")
    
    # 测试数据采集器
    print("\n3️⃣ 数据服务")
    collector = services.get_data_collector("HK", ["00700", "09988"])
    print(f"   ✅ 数据采集器: {len(collector.config.STOCKS)} 只股票")
    
    # 测试风控
    print("\n4️⃣ 风控服务")
    risk = services.get_risk_manager("test_agent")
    print(f"   ✅ 风控管理器已加载")
    
    print("\n✅ SharedServices 测试完成！")
