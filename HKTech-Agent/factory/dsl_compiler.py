#!/usr/bin/env python3
"""
DSL 编译器集成 - HKTech-Agent 版本

功能:
1. 编译 DSL 策略为 Python
2. 缓存编译结果（35x 加速）
3. 热加载策略
4. 错误诊断和报告
"""

import os
import sys
from pathlib import Path
from typing import Optional, Dict, Any

# 添加父目录到路径
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from .dsl.compiler import DSLParser
from .dsl.cache import CompilationCache
from .dsl.types import CompileError


class DSLCompiler:
    """
    HKTech-Agent DSL 编译器
    
    核心功能:
    - 编译 DSL 策略为 Python 代码
    - 自动缓存编译结果
    - 支持热重载
    - 完整的错误诊断
    
    使用示例:
        compiler = DSLCompiler()
        
        # 编译策略
        python_code = compiler.compile_strategy('rsi_strategy.dsl')
        
        # 加载策略
        strategy = compiler.load_strategy('rsi_strategy')
        
        # 获取策略实例
        instance = compiler.get_strategy_instance('rsi_strategy')
    """
    
    def __init__(self, cache_enabled: bool = True, cache_dir: str = None):
        """
        初始化 DSL 编译器
        
        Args:
            cache_enabled: 是否启用缓存（默认 True）
            cache_dir: 缓存目录（默认 .dsl_cache）
        """
        self.cache_enabled = cache_enabled
        self.parser = DSLParser(use_cache=cache_enabled)
        
        if cache_enabled:
            cache_path = cache_dir or '.dsl_cache'
            self.cache = CompilationCache(cache_dir=cache_path)
        else:
            self.cache = None
        
        # 已编译的策略缓存
        self.compiled_strategies: Dict[str, Any] = {}
        self.strategy_instances: Dict[str, Any] = {}
    
    def compile_strategy(
        self,
        dsl_file: str,
        output_dir: str = None,
        use_cache: bool = None
    ) -> str:
        """
        编译单个 DSL 策略
        
        Args:
            dsl_file: DSL 文件路径
            output_dir: 输出目录（默认 generated_strategies/）
            use_cache: 是否使用缓存（默认使用初始化设置）
        
        Returns:
            生成的 Python 代码
        
        Raises:
            CompileError: 编译错误
        """
        # 读取 DSL 文件
        dsl_path = Path(dsl_file)
        if not dsl_path.exists():
            raise FileNotFoundError(f"DSL 文件不存在：{dsl_file}")
        
        with open(dsl_path, 'r', encoding='utf-8') as f:
            dsl_code = f.read()
        
        # 编译 DSL
        try:
            python_code = self.parser.compile(dsl_code, use_cache=use_cache)
        except CompileError as e:
            # 格式化错误信息
            error_msg = self._format_error(e, dsl_code)
            raise CompileError(
                line=e.line,
                column=e.column,
                message=error_msg,
                severity=e.severity
            )
        
        # 保存生成的代码
        if output_dir:
            output_path = self._save_python_code(
                python_code,
                dsl_path.stem,
                output_dir
            )
            print(f"✅ 已保存：{output_path}")
        
        # 缓存编译结果
        strategy_name = dsl_path.stem
        self.compiled_strategies[strategy_name] = {
            'code': python_code,
            'dsl_file': str(dsl_path),
            'timestamp': Path(dsl_path).stat().st_mtime
        }
        
        return python_code
    
    def load_strategy(self, strategy_name: str) -> Any:
        """
        加载已编译的策略
        
        Args:
            strategy_name: 策略名称
        
        Returns:
            策略类
        """
        if strategy_name not in self.compiled_strategies:
            raise ValueError(f"策略未编译：{strategy_name}")
        
        strategy_data = self.compiled_strategies[strategy_name]
        python_code = strategy_data['code']
        
        # 动态加载策略
        namespace = {}
        exec(python_code, namespace)
        
        # 获取策略类（最后一个类定义）
        strategy_class = None
        for name, obj in namespace.items():
            if isinstance(obj, type) and name.endswith('Strategy'):
                strategy_class = obj
                break
        
        if strategy_class is None:
            raise ValueError(f"未在 {strategy_name} 中找到策略类")
        
        return strategy_class
    
    def get_strategy_instance(
        self,
        strategy_name: str,
        **kwargs
    ) -> Any:
        """
        获取策略实例
        
        Args:
            strategy_name: 策略名称
            **kwargs: 策略参数
        
        Returns:
            策略实例
        """
        # 检查缓存
        cache_key = f"{strategy_name}_{str(kwargs)}"
        if cache_key in self.strategy_instances:
            return self.strategy_instances[cache_key]
        
        # 加载策略类
        strategy_class = self.load_strategy(strategy_name)
        
        # 创建实例
        if kwargs:
            instance = strategy_class(**kwargs)
        else:
            instance = strategy_class()
        
        # 缓存实例
        self.strategy_instances[cache_key] = instance
        
        return instance
    
    def reload_strategy(self, strategy_name: str) -> Any:
        """
        热重载策略
        
        Args:
            strategy_name: 策略名称
        
        Returns:
            重新编译后的策略类
        """
        if strategy_name not in self.compiled_strategies:
            raise ValueError(f"策略不存在：{strategy_name}")
        
        strategy_data = self.compiled_strategies[strategy_name]
        dsl_file = strategy_data['dsl_file']
        
        # 重新编译
        print(f"🔄 重新编译策略：{strategy_name}")
        self.compile_strategy(dsl_file, use_cache=False)
        
        # 清除缓存的实例
        keys_to_remove = [
            key for key in self.strategy_instances.keys()
            if key.startswith(strategy_name)
        ]
        for key in keys_to_remove:
            del self.strategy_instances[key]
        
        # 加载新策略
        return self.load_strategy(strategy_name)
    
    def list_strategies(self) -> list:
        """
        列出所有已编译的策略
        
        Returns:
            策略名称列表
        """
        return list(self.compiled_strategies.keys())
    
    def get_stats(self) -> Dict:
        """
        获取编译器统计信息
        
        Returns:
            统计字典
        """
        stats = {
            'compiled_count': len(self.compiled_strategies),
            'instance_count': len(self.strategy_instances),
            'cache_enabled': self.cache_enabled
        }
        
        if self.cache:
            stats['cache_stats'] = self.cache.get_stats()
        
        return stats
    
    def _save_python_code(
        self,
        python_code: str,
        strategy_name: str,
        output_dir: str
    ) -> str:
        """保存 Python 代码到文件"""
        output_path = Path(output_dir) / f"{strategy_name}_strategy.py"
        output_path.parent.mkdir(parents=True, exist_ok=True)
        
        with open(output_path, 'w', encoding='utf-8') as f:
            f.write(python_code)
        
        return str(output_path)
    
    def _format_error(self, error: CompileError, dsl_code: str) -> str:
        """格式化错误信息"""
        if error.suggestion:
            return f"{error.message}\n💡 提示：{error.suggestion}"
        return error.message


# 全局编译器实例
_global_compiler: Optional[DSLCompiler] = None


def get_compiler(cache_enabled: bool = True) -> DSLCompiler:
    """
    获取全局编译器实例
    
    Args:
        cache_enabled: 是否启用缓存
    
    Returns:
        DSLCompiler 实例
    """
    global _global_compiler
    if _global_compiler is None:
        _global_compiler = DSLCompiler(cache_enabled=cache_enabled)
    return _global_compiler


def compile_dsl_strategy(
    dsl_file: str,
    output_dir: str = None,
    use_cache: bool = True
) -> str:
    """
    便捷函数：编译 DSL 策略
    
    Args:
        dsl_file: DSL 文件路径
        output_dir: 输出目录
        use_cache: 是否使用缓存
    
    Returns:
        Python 代码
    """
    compiler = get_compiler(cache_enabled=use_cache)
    return compiler.compile_strategy(dsl_file, output_dir)
