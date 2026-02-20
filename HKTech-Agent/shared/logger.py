#!/usr/bin/env python3
"""
统一结构化日志模块

提供分级日志记录（DEBUG, INFO, WARNING, ERROR, CRITICAL）
支持结构化日志输出（JSON格式可选）
支持文件和控制台双重输出
"""

import os
import sys
import json
import logging
import logging.handlers
from datetime import datetime
from typing import Dict, Any, Optional, Union
from enum import Enum


class LogLevel(Enum):
    """日志级别枚举"""
    DEBUG = logging.DEBUG
    INFO = logging.INFO
    WARNING = logging.WARNING
    ERROR = logging.ERROR
    CRITICAL = logging.CRITICAL


class StructuredLogger:
    """
    结构化日志记录器
    
    特性：
    1. 分级日志记录
    2. 结构化输出（支持JSON格式）
    3. 文件和控制台双重输出
    4. 日志轮转（按大小或时间）
    5. 上下文信息自动添加
    """
    
    _instance = None
    _initialized = False
    
    def __new__(cls):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
        return cls._instance
    
    def __init__(self):
        if self._initialized:
            return
        
        # 默认配置
        self._log_dir = self._get_default_log_dir()
        self._log_level = LogLevel.INFO
        self._structured_output = False
        self._log_to_console = True
        self._log_to_file = True
        
        # 创建日志目录
        os.makedirs(self._log_dir, exist_ok=True)
        
        # 初始化logging配置
        self._setup_logging()
        
        self._initialized = True
    
    def _get_default_log_dir(self) -> str:
        """获取默认日志目录"""
        # 尝试从环境变量获取
        log_dir = os.environ.get('AGENT_LOG_DIR')
        if log_dir and os.path.exists(log_dir):
            return log_dir
        
        # 默认目录
        current_dir = os.path.dirname(os.path.abspath(__file__))
        return os.path.join(current_dir, '../prod/logs')
    
    def _setup_logging(self):
        """设置logging配置"""
        # 创建根logger
        self.logger = logging.getLogger('hktech_agent')
        self.logger.setLevel(self._log_level.value)
        
        # 清除现有handler
        self.logger.handlers.clear()
        
        # 设置日志格式
        if self._structured_output:
            formatter = logging.Formatter(
                '{"timestamp": "%(asctime)s", "level": "%(levelname)s", "module": "%(name)s", "message": "%(message)s", "context": "%(context)s"}',
                datefmt='%Y-%m-%d %H:%M:%S'
            )
        else:
            formatter = logging.Formatter(
                '%(asctime)s - %(levelname)s - %(name)s - %(message)s',
                datefmt='%Y-%m-%d %H:%M:%S'
            )
        
        # 控制台handler
        if self._log_to_console:
            console_handler = logging.StreamHandler(sys.stdout)
            console_handler.setLevel(self._log_level.value)
            console_handler.setFormatter(formatter)
            self.logger.addHandler(console_handler)
        
        # 文件handler（按天轮转）
        if self._log_to_file:
            log_file = os.path.join(self._log_dir, 'hktech_agent.log')
            file_handler = logging.handlers.TimedRotatingFileHandler(
                log_file, when='midnight', interval=1, backupCount=30,
                encoding='utf-8'
            )
            file_handler.setLevel(self._log_level.value)
            file_handler.setFormatter(formatter)
            self.logger.addHandler(file_handler)
    
    def configure(self, 
                 log_level: Union[LogLevel, str] = LogLevel.INFO,
                 structured_output: bool = False,
                 log_to_console: bool = True,
                 log_to_file: bool = True,
                 log_dir: Optional[str] = None):
        """
        配置日志器
        
        Args:
            log_level: 日志级别
            structured_output: 是否使用结构化输出（JSON格式）
            log_to_console: 是否输出到控制台
            log_to_file: 是否输出到文件
            log_dir: 日志目录
        """
        if isinstance(log_level, str):
            log_level = LogLevel[log_level.upper()]
        
        self._log_level = log_level
        self._structured_output = structured_output
        self._log_to_console = log_to_console
        self._log_to_file = log_to_file
        
        if log_dir:
            self._log_dir = log_dir
            os.makedirs(self._log_dir, exist_ok=True)
        
        # 重新配置
        self._setup_logging()
    
    def debug(self, message: str, context: Optional[Dict[str, Any]] = None, **kwargs):
        """记录DEBUG级别日志"""
        extra = {'context': json.dumps(context or {})}
        self.logger.debug(message, extra=extra, **kwargs)
    
    def info(self, message: str, context: Optional[Dict[str, Any]] = None, **kwargs):
        """记录INFO级别日志"""
        extra = {'context': json.dumps(context or {})}
        self.logger.info(message, extra=extra, **kwargs)
    
    def warning(self, message: str, context: Optional[Dict[str, Any]] = None, **kwargs):
        """记录WARNING级别日志"""
        extra = {'context': json.dumps(context or {})}
        self.logger.warning(message, extra=extra, **kwargs)
    
    def error(self, message: str, context: Optional[Dict[str, Any]] = None, **kwargs):
        """记录ERROR级别日志"""
        extra = {'context': json.dumps(context or {})}
        self.logger.error(message, extra=extra, **kwargs)
    
    def critical(self, message: str, context: Optional[Dict[str, Any]] = None, **kwargs):
        """记录CRITICAL级别日志"""
        extra = {'context': json.dumps(context or {})}
        self.logger.critical(message, extra=extra, **kwargs)
    
    def log_performance(self, operation: str, duration: float, 
                       details: Optional[Dict[str, Any]] = None):
        """记录性能日志"""
        context = {
            "operation": operation,
            "duration_seconds": round(duration, 4),
            "details": details or {}
        }
        self.info(f"性能指标: {operation} 耗时 {duration:.3f}秒", context)
    
    def log_decision(self, stock_code: str, action: str, confidence: float,
                    reason: str, engine: str = "unknown"):
        """记录投资决策日志"""
        context = {
            "stock_code": stock_code,
            "action": action,
            "confidence": confidence,
            "reason": reason[:100],  # 限制长度
            "engine": engine,
            "timestamp": datetime.now().isoformat()
        }
        self.info(f"投资决策: {stock_code} - {action} (置信度: {confidence:.0%})", context)
    
    def log_data_collection(self, source: str, count: int, 
                           success: bool, error: Optional[str] = None):
        """记录数据收集日志"""
        context = {
            "source": source,
            "count": count,
            "success": success,
            "error": error,
            "timestamp": datetime.now().isoformat()
        }
        status = "成功" if success else "失败"
        message = f"数据收集: {source} - {status} ({count}条记录)"
        if success:
            self.info(message, context)
        else:
            self.error(f"{message}: {error}", context)
    
    def log_system_event(self, event_type: str, component: str,
                        status: str, details: Optional[Dict[str, Any]] = None):
        """记录系统事件日志"""
        context = {
            "event_type": event_type,
            "component": component,
            "status": status,
            "details": details or {},
            "timestamp": datetime.now().isoformat()
        }
        self.info(f"系统事件: {component} - {event_type} - {status}", context)


# 全局日志器实例
_logger_instance = None

def get_logger() -> StructuredLogger:
    """获取全局日志器实例"""
    global _logger_instance
    if _logger_instance is None:
        _logger_instance = StructuredLogger()
    return _logger_instance

def setup_logging(log_level: str = "INFO", structured_output: bool = False):
    """快速设置日志配置"""
    logger = get_logger()
    logger.configure(
        log_level=log_level,
        structured_output=structured_output,
        log_to_console=True,
        log_to_file=True
    )
    return logger


# 便捷函数
def debug(msg: str, **kwargs):
    get_logger().debug(msg, **kwargs)

def info(msg: str, **kwargs):
    get_logger().info(msg, **kwargs)

def warning(msg: str, **kwargs):
    get_logger().warning(msg, **kwargs)

def error(msg: str, **kwargs):
    get_logger().error(msg, **kwargs)

def critical(msg: str, **kwargs):
    get_logger().critical(msg, **kwargs)


if __name__ == "__main__":
    # 测试日志模块
    print("🧪 测试结构化日志模块")
    print("=" * 60)
    
    # 配置日志
    logger = setup_logging(log_level="DEBUG", structured_output=False)
    
    # 测试各种日志级别
    logger.debug("这是一条DEBUG消息")
    logger.info("这是一条INFO消息")
    logger.warning("这是一条WARNING消息")
    logger.error("这是一条ERROR消息")
    
    # 测试结构化日志
    logger.log_decision("00700", "buy", 0.75, "RSI超卖，技术面看好", "vectorbt")
    logger.log_data_collection("yfinance", 3, True)
    logger.log_data_collection("sina", 0, False, "网络连接失败")
    logger.log_performance("backtest", 2.345, {"stocks": 3, "days": 365})
    logger.log_system_event("startup", "LLMEnhancedAgent", "success")
    
    print("✅ 日志测试完成，请检查日志文件")