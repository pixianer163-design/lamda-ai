#!/usr/bin/env python3
"""
性能跟踪与异常检测模块

记录关键性能指标（KPI），建立基线，检测异常。
"""

import os
import json
import time
import statistics
import threading
from datetime import datetime, timedelta
from typing import Dict, List, Any, Optional, Tuple, Callable
from pathlib import Path
from dataclasses import dataclass, asdict
from enum import Enum

# 尝试导入可选依赖
try:
    import psutil
    PSUTIL_AVAILABLE = True
except ImportError:
    PSUTIL_AVAILABLE = False
    psutil = None

try:
    from logger import get_logger
    _logger = get_logger()
    LOGGER_AVAILABLE = True
except ImportError:
    LOGGER_AVAILABLE = False
    _logger = None


class MetricType(Enum):
    """指标类型"""
    EXECUTION_TIME = "execution_time"      # 执行时间（秒）
    MEMORY_USAGE = "memory_usage"          # 内存使用（MB）
    CPU_USAGE = "cpu_usage"                # CPU使用率（%）
    REQUEST_COUNT = "request_count"        # 请求计数
    ERROR_COUNT = "error_count"            # 错误计数
    CUSTOM = "custom"                      # 自定义指标


@dataclass
class MetricRecord:
    """指标记录"""
    metric_type: str
    value: float
    timestamp: datetime
    tags: Dict[str, str]
    metadata: Dict[str, Any]
    
    def to_dict(self) -> Dict[str, Any]:
        """转换为字典"""
        return {
            "metric_type": self.metric_type,
            "value": self.value,
            "timestamp": self.timestamp.isoformat(),
            "tags": self.tags,
            "metadata": self.metadata
        }
    
    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'MetricRecord':
        """从字典创建"""
        return cls(
            metric_type=data["metric_type"],
            value=data["value"],
            timestamp=datetime.fromisoformat(data["timestamp"]),
            tags=data.get("tags", {}),
            metadata=data.get("metadata", {})
        )


@dataclass
class PerformanceBaseline:
    """性能基线"""
    metric_type: str
    tags: Dict[str, str]
    window_days: int
    count: int
    mean: float
    std_dev: float
    min_value: float
    max_value: float
    percentile_95: float
    updated_at: datetime
    
    def to_dict(self) -> Dict[str, Any]:
        """转换为字典"""
        return {
            "metric_type": self.metric_type,
            "tags": self.tags,
            "window_days": self.window_days,
            "count": self.count,
            "mean": self.mean,
            "std_dev": self.std_dev,
            "min_value": self.min_value,
            "max_value": self.max_value,
            "percentile_95": self.percentile_95,
            "updated_at": self.updated_at.isoformat()
        }
    
    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'PerformanceBaseline':
        """从字典创建"""
        return cls(
            metric_type=data["metric_type"],
            tags=data.get("tags", {}),
            window_days=data.get("window_days", 7),
            count=data["count"],
            mean=data["mean"],
            std_dev=data["std_dev"],
            min_value=data["min_value"],
            max_value=data["max_value"],
            percentile_95=data["percentile_95"],
            updated_at=datetime.fromisoformat(data["updated_at"])
        )
    
    def is_anomaly(self, value: float, sigma_threshold: float = 3.0) -> Tuple[bool, float]:
        """
        检测值是否为异常
        
        Args:
            value: 待检测值
            sigma_threshold: 标准差阈值（默认3σ）
            
        Returns:
            (是否异常, 偏离标准差的倍数)
        """
        if self.std_dev == 0:
            deviation = abs(value - self.mean)
            is_anomaly = deviation > 0.1  # 如果标准差为0，任何偏差都视为异常
            sigma = deviation
        else:
            sigma = abs(value - self.mean) / self.std_dev
            is_anomaly = sigma > sigma_threshold
        
        return is_anomaly, sigma


class PerformanceTracker:
    """性能跟踪器"""
    
    def __init__(self, data_dir: Optional[str] = None, config: Optional[Dict[str, Any]] = None):
        self.config = config or {}
        self.data_dir = Path(data_dir) if data_dir else Path(__file__).parent.parent / "data" / "performance"
        self.data_dir.mkdir(parents=True, exist_ok=True)
        
        # 指标存储
        self.metrics_file = self.data_dir / "metrics.json"
        self.baselines_file = self.data_dir / "baselines.json"
        
        # 内存中的指标缓存（最近的数据）
        self.metrics_cache: List[MetricRecord] = []
        self.max_cache_size = self.config.get('max_cache_size', 1000)
        
        # 基线缓存
        self.baselines_cache: Dict[str, PerformanceBaseline] = {}
        
        # 锁用于线程安全
        self.lock = threading.RLock()
        
        # 加载现有数据
        self._load_data()
    
    def _load_data(self):
        """加载存储的数据"""
        try:
            if self.metrics_file.exists():
                with open(self.metrics_file, 'r', encoding='utf-8') as f:
                    data = json.load(f)
                    self.metrics_cache = [MetricRecord.from_dict(item) for item in data[-self.max_cache_size:]]
        except Exception as e:
            if LOGGER_AVAILABLE and _logger:
                _logger.warning(f"加载性能指标失败: {e}")
        
        try:
            if self.baselines_file.exists():
                with open(self.baselines_file, 'r', encoding='utf-8') as f:
                    data = json.load(f)
                    self.baselines_cache = {
                        f"{baseline['metric_type']}_{json.dumps(baseline['tags'], sort_keys=True)}": 
                        PerformanceBaseline.from_dict(baseline)
                        for baseline in data
                    }
        except Exception as e:
            if LOGGER_AVAILABLE and _logger:
                _logger.warning(f"加载性能基线失败: {e}")
    
    def _save_metrics(self):
        """保存指标到文件"""
        try:
            with open(self.metrics_file, 'w', encoding='utf-8') as f:
                data = [metric.to_dict() for metric in self.metrics_cache]
                json.dump(data, f, indent=2, ensure_ascii=False)
        except Exception as e:
            if LOGGER_AVAILABLE and _logger:
                _logger.error(f"保存性能指标失败: {e}")
    
    def _save_baselines(self):
        """保存基线到文件"""
        try:
            with open(self.baselines_file, 'w', encoding='utf-8') as f:
                data = [baseline.to_dict() for baseline in self.baselines_cache.values()]
                json.dump(data, f, indent=2, ensure_ascii=False)
        except Exception as e:
            if LOGGER_AVAILABLE and _logger:
                _logger.error(f"保存性能基线失败: {e}")
    
    def record_metric(self, metric_type: str, value: float, 
                      tags: Optional[Dict[str, str]] = None,
                      metadata: Optional[Dict[str, Any]] = None) -> MetricRecord:
        """
        记录性能指标
        
        Args:
            metric_type: 指标类型
            value: 指标值
            tags: 标签（用于分类）
            metadata: 元数据
            
        Returns:
            创建的记录
        """
        with self.lock:
            record = MetricRecord(
                metric_type=metric_type,
                value=value,
                timestamp=datetime.now(),
                tags=tags or {},
                metadata=metadata or {}
            )
            
            self.metrics_cache.append(record)
            
            # 限制缓存大小
            if len(self.metrics_cache) > self.max_cache_size:
                self.metrics_cache = self.metrics_cache[-self.max_cache_size:]
            
            # 定期保存
            if len(self.metrics_cache) % 100 == 0:
                self._save_metrics()
            
            return record
    
    def record_execution_time(self, operation: str, duration_seconds: float,
                              component: Optional[str] = None,
                              metadata: Optional[Dict[str, Any]] = None) -> MetricRecord:
        """
        记录执行时间
        
        Args:
            operation: 操作名称
            duration_seconds: 执行时间（秒）
            component: 组件名称
            metadata: 元数据
        """
        tags = {"operation": operation}
        if component:
            tags["component"] = component
        
        return self.record_metric(
            metric_type=MetricType.EXECUTION_TIME.value,
            value=duration_seconds,
            tags=tags,
            metadata=metadata
        )
    
    def record_memory_usage(self, component: Optional[str] = None,
                            metadata: Optional[Dict[str, Any]] = None) -> Optional[MetricRecord]:
        """
        记录内存使用情况
        
        Args:
            component: 组件名称
            metadata: 元数据
        """
        if not PSUTIL_AVAILABLE:
            return None
        
        try:
            process = psutil.Process()
            memory_mb = process.memory_info().rss / (1024 * 1024)
            
            tags = {}
            if component:
                tags["component"] = component
            
            return self.record_metric(
                metric_type=MetricType.MEMORY_USAGE.value,
                value=memory_mb,
                tags=tags,
                metadata=metadata
            )
        except Exception as e:
            if LOGGER_AVAILABLE and _logger:
                _logger.warning(f"记录内存使用失败: {e}")
            return None
    
    def record_cpu_usage(self, component: Optional[str] = None,
                         metadata: Optional[Dict[str, Any]] = None) -> Optional[MetricRecord]:
        """
        记录CPU使用率
        
        Args:
            component: 组件名称
            metadata: 元数据
        """
        if not PSUTIL_AVAILABLE:
            return None
        
        try:
            cpu_percent = psutil.cpu_percent(interval=0.1)
            
            tags = {}
            if component:
                tags["component"] = component
            
            return self.record_metric(
                metric_type=MetricType.CPU_USAGE.value,
                value=cpu_percent,
                tags=tags,
                metadata=metadata
            )
        except Exception as e:
            if LOGGER_AVAILABLE and _logger:
                _logger.warning(f"记录CPU使用率失败: {e}")
            return None
    
    def get_metrics(self, metric_type: Optional[str] = None,
                    tags: Optional[Dict[str, str]] = None,
                    start_time: Optional[datetime] = None,
                    end_time: Optional[datetime] = None) -> List[MetricRecord]:
        """
        获取指标记录
        
        Args:
            metric_type: 指标类型过滤
            tags: 标签过滤
            start_time: 开始时间
            end_time: 结束时间
            
        Returns:
            过滤后的指标记录
        """
        with self.lock:
            filtered = self.metrics_cache.copy()
            
            # 按指标类型过滤
            if metric_type:
                filtered = [m for m in filtered if m.metric_type == metric_type]
            
            # 按标签过滤
            if tags:
                filtered = [
                    m for m in filtered
                    if all(m.tags.get(key) == value for key, value in tags.items())
                ]
            
            # 按时间过滤
            if start_time:
                filtered = [m for m in filtered if m.timestamp >= start_time]
            if end_time:
                filtered = [m for m in filtered if m.timestamp <= end_time]
            
            return filtered
    
    def compute_baseline(self, metric_type: str, tags: Optional[Dict[str, str]] = None,
                         window_days: int = 7) -> Optional[PerformanceBaseline]:
        """
        计算性能基线
        
        Args:
            metric_type: 指标类型
            tags: 标签过滤
            window_days: 时间窗口（天）
            
        Returns:
            性能基线（如果数据足够）
        """
        # 获取时间窗口内的数据
        end_time = datetime.now()
        start_time = end_time - timedelta(days=window_days)
        
        metrics = self.get_metrics(metric_type, tags, start_time, end_time)
        
        if len(metrics) < 5:  # 最少需要5个数据点
            if LOGGER_AVAILABLE and _logger:
                _logger.debug(f"数据点不足，无法计算基线: {len(metrics)} < 5")
            return None
        
        values = [m.value for m in metrics]
        
        try:
            mean = statistics.mean(values)
            std_dev = statistics.stdev(values) if len(values) > 1 else 0
            min_value = min(values)
            max_value = max(values)
            
            # 计算95百分位数
            sorted_values = sorted(values)
            idx = int(0.95 * len(sorted_values))
            percentile_95 = sorted_values[idx] if idx < len(sorted_values) else sorted_values[-1]
            
            baseline = PerformanceBaseline(
                metric_type=metric_type,
                tags=tags or {},
                window_days=window_days,
                count=len(metrics),
                mean=mean,
                std_dev=std_dev,
                min_value=min_value,
                max_value=max_value,
                percentile_95=percentile_95,
                updated_at=datetime.now()
            )
            
            # 缓存基线
            cache_key = f"{metric_type}_{json.dumps(tags or {}, sort_keys=True)}"
            self.baselines_cache[cache_key] = baseline
            
            # 保存基线
            self._save_baselines()
            
            return baseline
            
        except Exception as e:
            if LOGGER_AVAILABLE and _logger:
                _logger.error(f"计算基线失败: {e}", exc_info=True)
            return None
    
    def get_or_compute_baseline(self, metric_type: str, tags: Optional[Dict[str, str]] = None,
                                window_days: int = 7, force_recompute: bool = False) -> Optional[PerformanceBaseline]:
        """
        获取或计算性能基线
        
        Args:
            metric_type: 指标类型
            tags: 标签过滤
            window_days: 时间窗口（天）
            force_recompute: 强制重新计算
            
        Returns:
            性能基线
        """
        cache_key = f"{metric_type}_{json.dumps(tags or {}, sort_keys=True)}"
        
        with self.lock:
            # 检查缓存
            if not force_recompute and cache_key in self.baselines_cache:
                baseline = self.baselines_cache[cache_key]
                # 检查基线是否过期（超过窗口时间的一半）
                baseline_age = datetime.now() - baseline.updated_at
                if baseline_age < timedelta(days=window_days / 2):
                    return baseline
            
            # 计算新基线
            return self.compute_baseline(metric_type, tags, window_days)
    
    def detect_anomaly(self, metric_type: str, value: float, 
                       tags: Optional[Dict[str, str]] = None,
                       sigma_threshold: float = 3.0) -> Tuple[bool, Optional[PerformanceBaseline], float]:
        """
        检测性能异常
        
        Args:
            metric_type: 指标类型
            value: 待检测值
            tags: 标签过滤
            sigma_threshold: 标准差阈值
            
        Returns:
            (是否异常, 基线对象, 偏离标准差的倍数)
        """
        baseline = self.get_or_compute_baseline(metric_type, tags)
        
        if not baseline:
            return False, None, 0
        
        is_anomaly, sigma = baseline.is_anomaly(value, sigma_threshold)
        
        return is_anomaly, baseline, sigma
    
    def track_execution(self, operation: str, component: Optional[str] = None,
                        metadata: Optional[Dict[str, Any]] = None) -> Callable:
        """
        创建执行时间跟踪装饰器/上下文管理器
        
        Args:
            operation: 操作名称
            component: 组件名称
            metadata: 元数据
            
        Returns:
            装饰器函数
        """
        def decorator(func):
            def wrapper(*args, **kwargs):
                start_time = time.time()
                try:
                    result = func(*args, **kwargs)
                    return result
                finally:
                    end_time = time.time()
                    duration = end_time - start_time
                    
                    self.record_execution_time(
                        operation=operation,
                        duration_seconds=duration,
                        component=component,
                        metadata={**(metadata or {}), "function": func.__name__}
                    )
                    
                    # 检测异常
                    baseline_key = f"{MetricType.EXECUTION_TIME.value}_{json.dumps({'operation': operation, 'component': component} if component else {'operation': operation}, sort_keys=True)}"
                    if baseline_key in self.baselines_cache:
                        baseline = self.baselines_cache[baseline_key]
                        is_anomaly, sigma = baseline.is_anomaly(duration)
                        if is_anomaly:
                            if LOGGER_AVAILABLE and _logger:
                                _logger.warning(
                                    f"检测到执行时间异常: {operation}, "
                                    f"值={duration:.3f}s, 均值={baseline.mean:.3f}s, "
                                    f"标准差={baseline.std_dev:.3f}, σ={sigma:.2f}"
                                )
            
            return wrapper
        return decorator
    
    def generate_report(self, days: int = 7) -> Dict[str, Any]:
        """
        生成性能报告
        
        Args:
            days: 报告天数
            
        Returns:
            性能报告
        """
        end_time = datetime.now()
        start_time = end_time - timedelta(days=days)
        
        # 获取所有指标
        metrics = self.get_metrics(start_time=start_time, end_time=end_time)
        
        # 按指标类型分组
        metrics_by_type = {}
        for metric in metrics:
            if metric.metric_type not in metrics_by_type:
                metrics_by_type[metric.metric_type] = []
            metrics_by_type[metric.metric_type].append(metric)
        
        # 生成统计信息
        report = {
            "period": {
                "start": start_time.isoformat(),
                "end": end_time.isoformat(),
                "days": days
            },
            "total_metrics": len(metrics),
            "metrics_by_type": {},
            "baselines": {},
            "anomalies": []
        }
        
        # 每种指标类型的统计
        for metric_type, type_metrics in metrics_by_type.items():
            values = [m.value for m in type_metrics]
            
            try:
                mean = statistics.mean(values) if values else 0
                std_dev = statistics.stdev(values) if len(values) > 1 else 0
                min_val = min(values) if values else 0
                max_val = max(values) if values else 0
                
                report["metrics_by_type"][metric_type] = {
                    "count": len(values),
                    "mean": mean,
                    "std_dev": std_dev,
                    "min": min_val,
                    "max": max_val,
                    "latest": values[-1] if values else None
                }
            except Exception:
                pass
        
        # 基线信息
        for baseline in self.baselines_cache.values():
            report["baselines"][baseline.metric_type] = baseline.to_dict()
        
        # 检测最近的异常
        recent_metrics = self.get_metrics(start_time=end_time - timedelta(hours=24))
        for metric in recent_metrics[-50:]:  # 检查最近50个指标
            is_anomaly, baseline, sigma = self.detect_anomaly(
                metric.metric_type, metric.value, metric.tags
            )
            if is_anomaly:
                report["anomalies"].append({
                    "metric_type": metric.metric_type,
                    "value": metric.value,
                    "timestamp": metric.timestamp.isoformat(),
                    "tags": metric.tags,
                    "sigma": sigma,
                    "baseline_mean": baseline.mean if baseline else None,
                    "baseline_std_dev": baseline.std_dev if baseline else None
                })
        
        return report


# 全局性能跟踪器实例
_performance_tracker = None

def get_performance_tracker(config: Optional[Dict[str, Any]] = None) -> PerformanceTracker:
    """获取全局性能跟踪器实例"""
    global _performance_tracker
    if _performance_tracker is None:
        _performance_tracker = PerformanceTracker(config=config)
    return _performance_tracker

def track_execution(operation: str, component: Optional[str] = None,
                    metadata: Optional[Dict[str, Any]] = None) -> Callable:
    """跟踪执行时间（装饰器工厂）"""
    tracker = get_performance_tracker()
    return tracker.track_execution(operation, component, metadata)

def record_execution_time(operation: str, duration_seconds: float,
                          component: Optional[str] = None,
                          metadata: Optional[Dict[str, Any]] = None) -> MetricRecord:
    """记录执行时间（便捷函数）"""
    tracker = get_performance_tracker()
    return tracker.record_execution_time(operation, duration_seconds, component, metadata)


if __name__ == '__main__':
    # 测试性能跟踪器
    print("🧪 测试性能跟踪器...")
    
    tracker = PerformanceTracker()
    
    # 记录一些测试指标
    for i in range(10):
        tracker.record_execution_time("test_operation", i * 0.1, "test_component")
        tracker.record_memory_usage("test_component")
    
    # 计算基线
    baseline = tracker.compute_baseline(
        metric_type=MetricType.EXECUTION_TIME.value,
        tags={"operation": "test_operation", "component": "test_component"}
    )
    
    if baseline:
        print(f"✅ 基线计算成功: 均值={baseline.mean:.3f}, 标准差={baseline.std_dev:.3f}")
        
        # 测试异常检测
        test_value = baseline.mean + 4 * baseline.std_dev
        is_anomaly, sigma = baseline.is_anomaly(test_value)
        print(f"  异常检测: 值={test_value:.3f}, σ={sigma:.2f}, 异常={is_anomaly}")
    
    # 生成报告
    report = tracker.generate_report(days=1)
    print(f"📊 性能报告: 总指标数={report['total_metrics']}")
    
    print("✅ 性能跟踪器测试完成")