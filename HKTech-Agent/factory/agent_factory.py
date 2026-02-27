#!/usr/bin/env python3
"""
Agent Factory - 工厂主类
创建和管理Agent实例
"""

import os
import sys
from typing import Dict, List, Optional
from copy import deepcopy

from core.config_manager import ConfigManager
from core.trading_agent import TradingAgent
from templates.agent_templates import get_template, list_templates


class AgentFactory:
    """
    Agent工厂
    
    核心功能：
    1. 基于模板创建Agent
    2. 加载配置文件创建Agent
    3. 克隆已有Agent（A/B测试）
    4. 管理Agent生命周期
    5. 批量操作
    """
    
    def __init__(self, config_dir: str = None):
        if config_dir is None:
            project_root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
            config_dir = os.path.join(project_root, "configs")
        self.config_manager = ConfigManager(config_dir)
        self.active_agents: Dict[str, TradingAgent] = {}
    
    # ============== 创建方法 ==============
    
    def create_agent(
        self,
        template_name: str,
        agent_id: str,
        name: str = None,
        overrides: Dict = None
    ) -> TradingAgent:
        """
        创建新Agent
        
        Args:
            template_name: 模板名称
            agent_id: Agent唯一标识
            name: 显示名称（可选）
            overrides: 配置覆盖项
            
        Returns:
            TradingAgent实例
        """
        # 检查是否已存在
        if agent_id in self.active_agents:
            raise ValueError(f"Agent {agent_id} 已存在")
        
        # 创建配置
        config = self.config_manager.create_config(
            agent_id=agent_id,
            template_name=template_name,
            overrides=overrides
        )
        
        # 应用自定义名称
        if name:
            config["name"] = name
        
        # 实例化Agent
        agent = TradingAgent(config)
        
        # 注册到活跃列表
        self.active_agents[agent_id] = agent
        
        # 保存配置
        self.config_manager.save_config(agent_id, config)
        
        print(f"✅ Agent创建成功: {agent_id} ({config['name']})")
        return agent
    
    def create_from_config(self, agent_id: str) -> TradingAgent:
        """
        从配置文件创建Agent
        
        Args:
            agent_id: Agent ID（对应配置文件名）
            
        Returns:
            TradingAgent实例
        """
        # 检查是否已存在
        if agent_id in self.active_agents:
            return self.active_agents[agent_id]
        
        # 加载配置
        config = self.config_manager.load_config(agent_id)
        if not config:
            raise ValueError(f"找不到配置文件: {agent_id}.yaml")
        
        # 实例化Agent
        agent = TradingAgent(config)
        
        # 注册
        self.active_agents[agent_id] = agent
        
        print(f"✅ Agent从配置加载: {agent_id} ({config['name']})")
        return agent
    
    def clone_agent(
        self,
        source_agent_id: str,
        new_agent_id: str,
        config_changes: Dict = None,
        name_suffix: str = "(Clone)"
    ) -> TradingAgent:
        """
        克隆Agent
        
        用途：
        - A/B测试不同策略
        - 快速创建相似Agent
        - 策略参数调优
        
        Args:
            source_agent_id: 源Agent ID
            new_agent_id: 新Agent ID
            config_changes: 配置修改项
            name_suffix: 名称后缀
            
        Returns:
            新的TradingAgent实例
        """
        # 获取源Agent配置
        if source_agent_id in self.active_agents:
            source_agent = self.active_agents[source_agent_id]
            source_config = deepcopy(source_agent.config)
        else:
            source_config = self.config_manager.load_config(source_agent_id)
            if not source_config:
                raise ValueError(f"源Agent不存在: {source_agent_id}")
        
        # 修改配置
        new_config = deepcopy(source_config)
        new_config["agent_id"] = new_agent_id
        new_config["name"] = f"{source_config['name']} {name_suffix}"
        new_config["cloned_from"] = source_agent_id
        new_config["created_at"] = None  # 重新计时
        
        # 应用修改
        if config_changes:
            new_config = self._deep_merge(new_config, config_changes)
        
        # 创建新Agent
        agent = TradingAgent(new_config)
        self.active_agents[new_agent_id] = agent
        self.config_manager.save_config(new_agent_id, new_config)
        
        print(f"✅ Agent克隆成功: {source_agent_id} -> {new_agent_id}")
        return agent
    
    # ============== 管理方法 ==============
    
    def get_agent(self, agent_id: str) -> Optional[TradingAgent]:
        """获取Agent实例"""
        return self.active_agents.get(agent_id)
    
    def list_agents(self, active_only: bool = True) -> List[Dict]:
        """
        列出所有Agent
        
        Args:
            active_only: 只显示活跃的（已加载的）
            
        Returns:
            Agent信息列表
        """
        if active_only:
            return [
                {
                    "agent_id": aid,
                    "name": agent.name,
                    "state": agent.state,
                    "day_count": agent.day_count,
                    "portfolio_value": agent.portfolio.get('total_value', 0)
                }
                for aid, agent in self.active_agents.items()
            ]
        else:
            # 包括未加载的（仅从配置）
            configs = self.config_manager.list_configs()
            active_ids = set(self.active_agents.keys())
            
            result = []
            for config in configs:
                agent_id = config["agent_id"]
                if agent_id in active_ids:
                    agent = self.active_agents[agent_id]
                    result.append({
                        "agent_id": agent_id,
                        "name": agent.name,
                        "state": agent.state,
                        "active": True
                    })
                else:
                    result.append({
                        "agent_id": agent_id,
                        "name": config["name"],
                        "state": "inactive",
                        "active": False
                    })
            return result
    
    def destroy_agent(self, agent_id: str, keep_config: bool = True):
        """
        销毁Agent
        
        Args:
            agent_id: Agent ID
            keep_config: 是否保留配置文件
        """
        if agent_id not in self.active_agents:
            print(f"⚠️ Agent {agent_id} 不存在")
            return
        
        agent = self.active_agents[agent_id]
        agent.shutdown()
        
        del self.active_agents[agent_id]
        
        if not keep_config:
            config_file = os.path.join(
                self.config_manager.config_dir, 
                f"{agent_id}.yaml"
            )
            if os.path.exists(config_file):
                os.remove(config_file)
                print(f"🗑️  配置文件已删除: {config_file}")
        
        print(f"✅ Agent已销毁: {agent_id}")
    
    def shutdown_all(self):
        """关闭所有Agent"""
        for agent_id, agent in list(self.active_agents.items()):
            agent.shutdown()
        self.active_agents.clear()
        print("✅ 所有Agent已关闭")
    
    # ============== 批量操作 ==============
    
    def run_all(self, agent_ids: List[str] = None):
        """
        运行所有或指定Agent
        
        Args:
            agent_ids: 指定运行的Agent列表，None表示全部
        """
        import asyncio
        
        targets = agent_ids or list(self.active_agents.keys())
        
        async def run_agents():
            tasks = []
            for agent_id in targets:
                agent = self.active_agents.get(agent_id)
                if agent:
                    tasks.append(agent.run_cycle())
            
            if tasks:
                await asyncio.gather(*tasks, return_exceptions=True)
        
        asyncio.run(run_agents())
    
    def get_stats(self) -> Dict:
        """获取工厂统计"""
        return {
            "active_agents": len(self.active_agents),
            "total_configs": len(self.config_manager.list_configs()),
            "available_templates": list(list_templates().keys())
        }
    
    # ============== 辅助方法 ==============
    
    def _deep_merge(self, base: Dict, override: Dict) -> Dict:
        """深度合并字典"""
        result = deepcopy(base)
        
        for key, value in override.items():
            if key in result and isinstance(result[key], dict) and isinstance(value, dict):
                result[key] = self._deep_merge(result[key], value)
            else:
                result[key] = value
        
        return result


# ============== 便捷函数 ==============

def create_factory() -> AgentFactory:
    """创建工厂实例"""
    return AgentFactory()


# ============== 命令行接口 ==============

if __name__ == "__main__":
    import sys
    
    factory = create_factory()
    
    if len(sys.argv) < 2:
        print("="*60)
        print("🤖 Agent Factory CLI")
        print("="*60)
        print("\n用法:")
        print("  python agent_factory.py list-templates    # 列出模板")
        print("  python agent_factory.py list-agents       # 列出Agent")
        print("  python agent_factory.py create <template> <agent_id>  # 创建Agent")
        print("  python agent_factory.py stats             # 统计信息")
        sys.exit(0)
    
    cmd = sys.argv[1]
    
    if cmd == "list-templates":
        print("📋 可用模板:")
        for name, desc in list_templates().items():
            print(f"\n  {name}:")
            print(f"    {desc}")
    
    elif cmd == "list-agents":
        agents = factory.list_agents(active_only=False)
        print("📊 Agent列表:")
        for agent in agents:
            status = "🟢" if agent.get("active") else "⚪"
            print(f"  {status} {agent['agent_id']}: {agent['name']} [{agent['state']}]")
    
    elif cmd == "create" and len(sys.argv) >= 4:
        template = sys.argv[2]
        agent_id = sys.argv[3]
        try:
            agent = factory.create_agent(template, agent_id)
            print(f"\n✅ 创建成功!")
            print(f"   ID: {agent.agent_id}")
            print(f"   名称: {agent.name}")
            print(f"   股票: {', '.join(agent.stocks)}")
        except Exception as e:
            print(f"❌ 创建失败: {e}")
    
    elif cmd == "stats":
        stats = factory.get_stats()
        print("📈 工厂统计:")
        print(f"  活跃Agent: {stats['active_agents']}")
        print(f"  总配置数: {stats['total_configs']}")
        print(f"  可用模板: {', '.join(stats['available_templates'])}")
    
    else:
        print(f"❌ 未知命令: {cmd}")
