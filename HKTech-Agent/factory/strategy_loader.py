#!/usr/bin/env python3
"""
策略加载器 - 支持 DSL 和 Python 策略

功能:
1. 自动检测策略类型（DSL/Python）
2. DSL 策略自动编译
3. Python 策略直接加载
4. 支持热重载
"""

import os
import sys
from pathlib import Path
from typing import Optional, Dict, Any, Union

from .dsl_compiler import DSLCompiler


class StrategyLoader:
    """
    统一策略加载器
    
    支持:
    - DSL 策略（.dsl 文件，自动编译）
    - Python 策略（.py 文件，直接加载）
    
    使用示例:
        loader = StrategyLoader()
        
        # 加载 DSL 策略
        strategy = loader.load('rsi_strategy.dsl')
        
        # 加载 Python 策略
        strategy = loader.load('macd_strategy.py')
        
        # 热重载
        loader.reload('rsi_strategy')
    """
    
    def __init__(self, cache_enabled: bool = True):
        """
        初始化策略加载器
        
        Args:
            cache_enabled: 是否启用缓存
        """
        self.dsl_compiler = DSLCompiler(cache_enabled=cache_enabled)
        self.loaded_strategies: Dict[str, Any] = {}
        self.strategy_metadata: Dict[str, Dict] = {}
    
    def load(
        self,
        strategy_path: str,
        strategy_type: str = 'auto',
        **kwargs
    ) -> Any:
        """
        加载策略
        
        Args:
            strategy_path: 策略文件路径或策略名称
            strategy_type: 'dsl', 'python', or 'auto'
            **kwargs: 策略参数
        
        Returns:
            策略实例
        
        Raises:
            FileNotFoundError: 文件不存在
            ValueError: 不支持的策略类型
        """
        strategy_path_obj = Path(strategy_path)
        
        # 检查是否已加载
        strategy_name = strategy_path_obj.stem
        if strategy_name in self.loaded_strategies:
            print(f"ℹ️  策略已加载，使用缓存：{strategy_name}")
            return self.loaded_strategies[strategy_name]
        
        # 自动检测策略类型
        if strategy_type == 'auto':
            if strategy_path_obj.suffix == '.dsl':
                strategy_type = 'dsl'
            elif strategy_path_obj.suffix == '.py':
                strategy_type = 'python'
            else:
                # 尝试查找文件
                dsl_file = strategy_path_obj.with_suffix('.dsl')
                py_file = strategy_path_obj.with_suffix('.py')
                
                if dsl_file.exists():
                    strategy_type = 'dsl'
                    strategy_path = str(dsl_file)
                elif py_file.exists():
                    strategy_type = 'python'
                    strategy_path = str(py_file)
                else:
                    raise FileNotFoundError(
                        f"找不到策略文件：{strategy_path} (.dsl 或 .py)"
                    )
        
        # 加载策略
        if strategy_type == 'dsl':
            strategy = self._load_dsl_strategy(strategy_path, **kwargs)
        elif strategy_type == 'python':
            strategy = self._load_python_strategy(strategy_path, **kwargs)
        else:
            raise ValueError(f"不支持的策略类型：{strategy_type}")
        
        # 缓存策略
        self.loaded_strategies[strategy_name] = strategy
        self.strategy_metadata[strategy_name] = {
            'type': strategy_type,
            'path': strategy_path,
            'params': kwargs
        }
        
        return strategy
    
    def reload(self, strategy_name: str) -> Any:
        """
        热重载策略
        
        Args:
            strategy_name: 策略名称
        
        Returns:
            重新加载后的策略实例
        """
        if strategy_name not in self.strategy_metadata:
            raise ValueError(f"策略未加载：{strategy_name}")
        
        metadata = self.strategy_metadata[strategy_name]
        strategy_type = metadata['type']
        strategy_path = metadata['path']
        params = metadata['params']
        
        print(f"🔄 热重载策略：{strategy_name} ({strategy_type})")
        
        # 清除缓存
        if strategy_name in self.loaded_strategies:
            del self.loaded_strategies[strategy_name]
        
        # 重新加载
        if strategy_type == 'dsl':
            # DSL 策略需要重新编译
            self.dsl_compiler.reload_strategy(strategy_name)
        
        return self.load(strategy_path, strategy_type, **params)
    
    def unload(self, strategy_name: str):
        """
        卸载策略
        
        Args:
            strategy_name: 策略名称
        """
        if strategy_name in self.loaded_strategies:
            del self.loaded_strategies[strategy_name]
        if strategy_name in self.strategy_metadata:
            del self.strategy_metadata[strategy_name]
        
        print(f"✅ 已卸载策略：{strategy_name}")
    
    def list_strategies(self) -> list:
        """
        列出已加载的策略
        
        Returns:
            策略名称列表
        """
        return list(self.loaded_strategies.keys())
    
    def get_strategy_info(self, strategy_name: str) -> Optional[Dict]:
        """
        获取策略信息
        
        Args:
            strategy_name: 策略名称
        
        Returns:
            策略信息字典
        """
        if strategy_name not in self.strategy_metadata:
            return None
        
        return {
            'name': strategy_name,
            **self.strategy_metadata[strategy_name]
        }
    
    def _load_dsl_strategy(self, dsl_file: str, **kwargs) -> Any:
        """
        加载 DSL 策略
        
        Args:
            dsl_file: DSL 文件路径
            **kwargs: 策略参数
        
        Returns:
            策略实例
        """
        # 编译 DSL
        python_code = self.dsl_compiler.compile_strategy(dsl_file)
        
        # 动态加载
        namespace = {}
        exec(python_code, namespace)
        
        # 获取策略类
        strategy_class = None
        for name, obj in namespace.items():
            if isinstance(obj, type) and name.endswith('Strategy'):
                strategy_class = obj
                break
        
        if strategy_class is None:
            raise ValueError(f"未在 {dsl_file} 中找到策略类")
        
        # 创建实例
        if kwargs:
            instance = strategy_class(**kwargs)
        else:
            instance = strategy_class()
        
        return instance
    
    def _load_python_strategy(self, py_file: str, **kwargs) -> Any:
        """
        加载 Python 策略
        
        Args:
            py_file: Python 文件路径
            **kwargs: 策略参数
        
        Returns:
            策略实例
        """
        # 导入模块
        module_name = Path(py_file).stem
        spec = __import__('importlib.util').util.spec_from_file_location(
            module_name,
            py_file
        )
        module = __import__('importlib.util').util.module_from_spec(spec)
        spec.loader.exec_module(module)
        
        # 获取策略类
        strategy_class = None
        for name in dir(module):
            if name.endswith('Strategy'):
                obj = getattr(module, name)
                if isinstance(obj, type):
                    strategy_class = obj
                    break
        
        if strategy_class is None:
            raise ValueError(f"未在 {py_file} 中找到策略类")
        
        # 创建实例
        if kwargs:
            instance = strategy_class(**kwargs)
        else:
            instance = strategy_class()
        
        return instance


# 全局加载器实例
_global_loader: Optional[StrategyLoader] = None


def get_loader(cache_enabled: bool = True) -> StrategyLoader:
    """
    获取全局加载器实例
    
    Args:
        cache_enabled: 是否启用缓存
    
    Returns:
        StrategyLoader 实例
    """
    global _global_loader
    if _global_loader is None:
        _global_loader = StrategyLoader(cache_enabled=cache_enabled)
    return _global_loader


def load_strategy(
    strategy_path: str,
    strategy_type: str = 'auto',
    **kwargs
) -> Any:
    """
    便捷函数：加载策略
    
    Args:
        strategy_path: 策略文件路径
        strategy_type: 策略类型
        **kwargs: 策略参数
    
    Returns:
        策略实例
    """
    loader = get_loader()
    return loader.load(strategy_path, strategy_type, **kwargs)
