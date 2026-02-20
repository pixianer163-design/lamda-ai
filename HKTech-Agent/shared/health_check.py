#!/usr/bin/env python3
"""
系统健康检查模块

提供组件状态检查、依赖验证、性能监控和系统指标收集。
"""

import os
import sys
import json
import time
import platform
import subprocess
from datetime import datetime, timedelta
from typing import Dict, List, Any, Optional, Tuple
from pathlib import Path

# 尝试导入psutil（可选依赖）
try:
    import psutil
    PSUTIL_AVAILABLE = True
except ImportError:
    PSUTIL_AVAILABLE = False
    psutil = None

# 尝试导入项目模块
try:
    from logger import get_logger
    _logger = get_logger()
    LOGGER_AVAILABLE = True
except ImportError:
    LOGGER_AVAILABLE = False
    _logger = None

try:
    import constants
    CONSTANTS_AVAILABLE = True
except ImportError:
    CONSTANTS_AVAILABLE = False
    constants = None

# 尝试导入告警模块（可选）
try:
    from alert import send_health_alert
    ALERT_AVAILABLE = True
except ImportError:
    ALERT_AVAILABLE = False
    send_health_alert = None


class HealthCheck:
    """
    系统健康检查器
    
    检查项：
    1. 系统资源 (CPU, 内存, 磁盘)
    2. 依赖包状态
    3. 数据目录权限
    4. 外部API连通性
    5. 组件运行状态
    6. 日志文件状态
    """
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        self.config = config or {}
        self.checks = []
        self.results = {}
        self.start_time = time.time()
        
        # 注册默认检查项
        self._register_default_checks()
    
    def _register_default_checks(self):
        """注册默认检查项"""
        self.register_check("system_resources", self.check_system_resources, 
                          "系统资源检查", critical=True)
        self.register_check("python_environment", self.check_python_environment,
                          "Python环境检查", critical=True)
        self.register_check("data_directories", self.check_data_directories,
                          "数据目录检查", critical=True)
        self.register_check("dependencies", self.check_dependencies,
                          "依赖包检查", critical=False)
        self.register_check("external_apis", self.check_external_apis,
                          "外部API检查", critical=False)
        self.register_check("component_status", self.check_component_status,
                          "组件状态检查", critical=True)
        self.register_check("log_files", self.check_log_files,
                          "日志文件检查", critical=False)
    
    def register_check(self, name: str, func: callable, 
                      description: str = "", critical: bool = False):
        """注册检查项"""
        self.checks.append({
            "name": name,
            "func": func,
            "description": description,
            "critical": critical
        })
    
    def run_all_checks(self, send_alerts: bool = False) -> Dict[str, Any]:
        """
        运行所有健康检查
        
        Args:
            send_alerts: 是否发送告警（如果告警模块可用）
            
        Returns:
            检查结果汇总
        """
        self.results = {
            "timestamp": datetime.now().isoformat(),
            "system": platform.system(),
            "python_version": platform.python_version(),
            "checks": {},
            "summary": {
                "total": 0,
                "passed": 0,
                "failed": 0,
                "critical_failed": 0,
                "overall_status": "healthy"
            }
        }
        
        for check in self.checks:
            check_name = check["name"]
            check_desc = check["description"]
            is_critical = check["critical"]
            
            if LOGGER_AVAILABLE and _logger:
                _logger.info(f"运行健康检查: {check_desc}")
            
            try:
                result = check["func"]()
                result["critical"] = is_critical
                result["description"] = check_desc
                result["timestamp"] = datetime.now().isoformat()
                
                self.results["checks"][check_name] = result
                
                if result.get("status") == "healthy":
                    self.results["summary"]["passed"] += 1
                else:
                    self.results["summary"]["failed"] += 1
                    if is_critical:
                        self.results["summary"]["critical_failed"] += 1
                
                self.results["summary"]["total"] += 1
                
            except Exception as e:
                error_result = {
                    "status": "unhealthy",
                    "error": str(e),
                    "critical": is_critical,
                    "description": check_desc,
                    "timestamp": datetime.now().isoformat()
                }
                self.results["checks"][check_name] = error_result
                self.results["summary"]["failed"] += 1
                if is_critical:
                    self.results["summary"]["critical_failed"] += 1
                self.results["summary"]["total"] += 1
        
        # 确定总体状态
        if self.results["summary"]["critical_failed"] > 0:
            self.results["summary"]["overall_status"] = "critical"
        elif self.results["summary"]["failed"] > 0:
            self.results["summary"]["overall_status"] = "degraded"
        else:
            self.results["summary"]["overall_status"] = "healthy"
        
        # 计算检查耗时
        self.results["summary"]["duration_seconds"] = round(
            time.time() - self.start_time, 3
        )
        
        # 发送告警（如果启用且告警模块可用）
        if send_alerts and ALERT_AVAILABLE and send_health_alert:
            try:
                send_health_alert(self.results)
                if LOGGER_AVAILABLE and _logger:
                    _logger.info("健康检查告警已发送")
            except Exception as e:
                if LOGGER_AVAILABLE and _logger:
                    _logger.error(f"发送健康检查告警失败: {e}", exc_info=True)
        
        return self.results
    
    # ============================================================================
    # 具体检查实现
    # ============================================================================
    
    def check_system_resources(self) -> Dict[str, Any]:
        """检查系统资源"""
        try:
            # 检查psutil是否可用
            if not PSUTIL_AVAILABLE:
                return {
                    "status": "degraded",
                    "error": "psutil模块不可用，跳过系统资源检查",
                    "metrics": {},
                    "details": {
                        "skip_reason": "missing_dependency",
                        "recommendation": "安装psutil以获得系统监控功能: pip install psutil"
                    }
                }
            
            assert psutil is not None  # 类型检查器提示
            cpu_percent = psutil.cpu_percent(interval=0.5)
            memory = psutil.virtual_memory()
            disk = psutil.disk_usage('/')
            
            # 阈值定义
            cpu_threshold = 90.0
            memory_threshold = 90.0
            disk_threshold = 90.0
            
            cpu_status = "healthy" if cpu_percent < cpu_threshold else "warning"
            memory_status = "healthy" if memory.percent < memory_threshold else "warning"
            disk_status = "healthy" if disk.percent < disk_threshold else "warning"
            
            overall_status = "healthy"
            if cpu_status == "warning" or memory_status == "warning" or disk_status == "warning":
                overall_status = "degraded"
            
            return {
                "status": overall_status,
                "metrics": {
                    "cpu_percent": cpu_percent,
                    "memory_percent": memory.percent,
                    "memory_available_gb": round(memory.available / (1024**3), 2),
                    "disk_percent": disk.percent,
                    "disk_free_gb": round(disk.free / (1024**3), 2)
                },
                "thresholds": {
                    "cpu": cpu_threshold,
                    "memory": memory_threshold,
                    "disk": disk_threshold
                },
                "details": {
                    "cpu_status": cpu_status,
                    "memory_status": memory_status,
                    "disk_status": disk_status
                }
            }
        except Exception as e:
            return {
                "status": "unhealthy",
                "error": f"系统资源检查失败: {e}",
                "metrics": {}
            }
    
    def check_python_environment(self) -> Dict[str, Any]:
        """检查Python环境"""
        try:
            import sys
            import importlib.util
            
            # 检查关键Python模块
            required_modules = ["json", "os", "sys", "datetime", "typing"]
            missing_modules = []
            
            for module in required_modules:
                if importlib.util.find_spec(module) is None:
                    missing_modules.append(module)
            
            status = "healthy" if not missing_modules else "unhealthy"
            
            return {
                "status": status,
                "python_version": platform.python_version(),
                "python_path": sys.executable,
                "sys_path": sys.path[:5],  # 只显示前5个路径
                "missing_modules": missing_modules,
                "details": {
                    "module_check": "passed" if not missing_modules else "failed"
                }
            }
        except Exception as e:
            return {
                "status": "unhealthy",
                "error": f"Python环境检查失败: {e}"
            }
    
    def check_data_directories(self) -> Dict[str, Any]:
        """检查数据目录"""
        try:
            # 获取项目根目录
            current_dir = os.path.dirname(os.path.abspath(__file__))
            project_root = os.path.join(current_dir, '..')
            
            # 关键目录列表
            critical_dirs = [
                os.path.join(project_root, 'data'),
                os.path.join(project_root, 'prod/logs'),
                os.path.join(project_root, 'shared')
            ]
            
            results = []
            missing_dirs = []
            
            for dir_path in critical_dirs:
                exists = os.path.exists(dir_path)
                writable = False
                if exists:
                    writable = os.access(dir_path, os.W_OK)
                
                results.append({
                    "path": dir_path,
                    "exists": exists,
                    "writable": writable,
                    "status": "healthy" if exists and writable else "unhealthy"
                })
                
                if not exists or not writable:
                    missing_dirs.append(dir_path)
            
            overall_status = "healthy" if not missing_dirs else "unhealthy"
            
            return {
                "status": overall_status,
                "directories": results,
                "missing_or_unwritable": missing_dirs,
                "details": {
                    "total_checked": len(critical_dirs),
                    "healthy": len([r for r in results if r["status"] == "healthy"]),
                    "unhealthy": len(missing_dirs)
                }
            }
        except Exception as e:
            return {
                "status": "unhealthy",
                "error": f"数据目录检查失败: {e}"
            }
    
    def check_dependencies(self) -> Dict[str, Any]:
        """检查依赖包"""
        try:
            # 核心依赖
            core_dependencies = [
                ("numpy", "数值计算"),
                ("pandas", "数据处理"),
                ("yfinance", "财经数据"),
                ("requests", "HTTP请求")
            ]
            
            # 可选依赖
            optional_dependencies = [
                ("torch", "深度学习"),
                ("vectorbt", "向量化回测"),
                ("psutil", "系统监控")
            ]
            
            results = []
            missing_core = []
            missing_optional = []
            
            import importlib.util
            
            # 检查核心依赖
            for package, description in core_dependencies:
                spec = importlib.util.find_spec(package)
                available = spec is not None
                
                results.append({
                    "package": package,
                    "description": description,
                    "type": "core",
                    "available": available,
                    "status": "healthy" if available else "unhealthy"
                })
                
                if not available:
                    missing_core.append(package)
            
            # 检查可选依赖
            for package, description in optional_dependencies:
                spec = importlib.util.find_spec(package)
                available = spec is not None
                
                results.append({
                    "package": package,
                    "description": description,
                    "type": "optional",
                    "available": available,
                    "status": "healthy" if available else "warning"
                })
                
                if not available:
                    missing_optional.append(package)
            
            overall_status = "healthy"
            if missing_core:
                overall_status = "unhealthy"
            elif missing_optional:
                overall_status = "degraded"
            
            return {
                "status": overall_status,
                "dependencies": results,
                "missing_core": missing_core,
                "missing_optional": missing_optional,
                "details": {
                    "total_core": len(core_dependencies),
                    "available_core": len(core_dependencies) - len(missing_core),
                    "total_optional": len(optional_dependencies),
                    "available_optional": len(optional_dependencies) - len(missing_optional)
                }
            }
        except Exception as e:
            return {
                "status": "unhealthy",
                "error": f"依赖包检查失败: {e}"
            }
    
    def check_external_apis(self) -> Dict[str, Any]:
        """检查外部API连通性"""
        try:
            import requests
            import socket
            
            # 测试的API端点
            api_endpoints = [
                {
                    "name": "Yahoo Finance",
                    "url": "https://finance.yahoo.com",
                    "timeout": 5,
                    "required": False
                },
                {
                    "name": "Sina Finance",
                    "url": "https://hq.sinajs.cn",
                    "timeout": 5,
                    "required": False
                },
                {
                    "name": "Internet Connectivity",
                    "url": "https://www.google.com",
                    "timeout": 3,
                    "required": True
                }
            ]
            
            results = []
            failed_apis = []
            
            for endpoint in api_endpoints:
                start_time = time.time()
                try:
                    response = requests.get(
                        endpoint["url"], 
                        timeout=endpoint["timeout"],
                        headers={"User-Agent": "HKTech-Agent-HealthCheck"}
                    )
                    latency = round((time.time() - start_time) * 1000, 2)  # 毫秒
                    
                    status = "healthy" if response.status_code < 400 else "unhealthy"
                    
                    results.append({
                        "name": endpoint["name"],
                        "url": endpoint["url"],
                        "status_code": response.status_code,
                        "latency_ms": latency,
                        "required": endpoint["required"],
                        "status": status
                    })
                    
                    if status != "healthy" and endpoint["required"]:
                        failed_apis.append(endpoint["name"])
                        
                except Exception as e:
                    results.append({
                        "name": endpoint["name"],
                        "url": endpoint["url"],
                        "error": str(e),
                        "required": endpoint["required"],
                        "status": "unhealthy"
                    })
                    
                    if endpoint["required"]:
                        failed_apis.append(endpoint["name"])
            
            overall_status = "healthy"
            if any(r["status"] == "unhealthy" and r.get("required", False) for r in results):
                overall_status = "unhealthy"
            elif any(r["status"] == "unhealthy" for r in results):
                overall_status = "degraded"
            
            return {
                "status": overall_status,
                "apis": results,
                "failed_required": failed_apis,
                "details": {
                    "total_checked": len(api_endpoints),
                    "healthy": len([r for r in results if r["status"] == "healthy"]),
                    "unhealthy": len([r for r in results if r["status"] == "unhealthy"])
                }
            }
        except ImportError:
            # requests可能未安装
            return {
                "status": "degraded",
                "error": "requests模块未安装，无法检查API连通性",
                "apis": [],
                "details": {"skip_reason": "missing_dependency"}
            }
        except Exception as e:
            return {
                "status": "unhealthy",
                "error": f"API检查失败: {e}"
            }
    
    def check_component_status(self) -> Dict[str, Any]:
        """检查组件状态"""
        try:
            components = []
            
            # 检查共享模块
            shared_modules = ["constants", "config_loader", "strategy_engine", "logger"]
            for module_name in shared_modules:
                try:
                    __import__(module_name)
                    components.append({
                        "name": module_name,
                        "type": "shared_module",
                        "status": "healthy",
                        "loaded": True
                    })
                except ImportError:
                    components.append({
                        "name": module_name,
                        "type": "shared_module",
                        "status": "unhealthy",
                        "loaded": False
                    })
            
            # 检查生产模块
            prod_modules = ["llm_enhanced_agent", "llm_signal_extractor", "llm_decision_enhancer"]
            for module_name in prod_modules:
                try:
                    # 尝试从prod.src导入
                    import importlib.util
                    module_path = f"prod.src.{module_name}"
                    spec = importlib.util.find_spec(module_path)
                    
                    components.append({
                        "name": module_name,
                        "type": "production_module",
                        "status": "healthy" if spec else "unhealthy",
                        "loaded": spec is not None
                    })
                except Exception:
                    components.append({
                        "name": module_name,
                        "type": "production_module",
                        "status": "unhealthy",
                        "loaded": False
                    })
            
            unhealthy_components = [c for c in components if c["status"] != "healthy"]
            overall_status = "healthy" if not unhealthy_components else "unhealthy"
            
            return {
                "status": overall_status,
                "components": components,
                "unhealthy_components": unhealthy_components,
                "details": {
                    "total_components": len(components),
                    "healthy_components": len(components) - len(unhealthy_components),
                    "unhealthy_components": len(unhealthy_components)
                }
            }
        except Exception as e:
            return {
                "status": "unhealthy",
                "error": f"组件状态检查失败: {e}"
            }
    
    def check_log_files(self) -> Dict[str, Any]:
        """检查日志文件"""
        try:
            # 查找日志目录
            current_dir = os.path.dirname(os.path.abspath(__file__))
            log_dir = os.path.join(current_dir, '../prod/logs')
            
            if not os.path.exists(log_dir):
                return {
                    "status": "unhealthy",
                    "error": f"日志目录不存在: {log_dir}",
                    "log_files": [],
                    "details": {"directory_exists": False}
                }
            
            # 获取日志文件
            log_files = []
            total_size = 0
            
            for filename in os.listdir(log_dir):
                if filename.endswith('.log'):
                    filepath = os.path.join(log_dir, filename)
                    stats = os.stat(filepath)
                    
                    log_files.append({
                        "name": filename,
                        "size_mb": round(stats.st_size / (1024 * 1024), 3),
                        "modified": datetime.fromtimestamp(stats.st_mtime).isoformat(),
                        "age_days": (datetime.now() - datetime.fromtimestamp(stats.st_mtime)).days
                    })
                    
                    total_size += stats.st_size
            
            # 检查日志文件状态
            recent_logs = [f for f in log_files if f["age_days"] <= 7]
            large_logs = [f for f in log_files if f["size_mb"] > 100]  # 大于100MB
            
            status = "healthy"
            warnings = []
            
            if not recent_logs:
                warnings.append("最近7天内无日志文件")
                status = "warning"
            
            if large_logs:
                warnings.append(f"发现 {len(large_logs)} 个大日志文件(>100MB)")
                status = "warning"
            
            return {
                "status": status,
                "log_dir": log_dir,
                "log_files": log_files,
                "total_size_mb": round(total_size / (1024 * 1024), 2),
                "warnings": warnings,
                "details": {
                    "total_files": len(log_files),
                    "recent_files": len(recent_logs),
                    "large_files": len(large_logs)
                }
            }
        except Exception as e:
            return {
                "status": "unhealthy",
                "error": f"日志文件检查失败: {e}"
            }
    
    def generate_report(self, format: str = "text") -> str:
        """
        生成健康检查报告
        
        Args:
            format: 报告格式 ("text", "json", "html")
        
        Returns:
            格式化报告
        """
        if not self.results:
            self.run_all_checks()
        
        if format == "json":
            return json.dumps(self.results, indent=2, ensure_ascii=False)
        
        elif format == "html":
            # 简单的HTML报告
            html = f"""
            <!DOCTYPE html>
            <html>
            <head>
                <title>HKTech-Agent 健康检查报告</title>
                <style>
                    body {{ font-family: -apple-system, BlinkMacSystemFont, sans-serif; margin: 40px; background: #f5f7fa; }}
                    .container {{ max-width: 1200px; margin: 0 auto; background: white; padding: 30px; border-radius: 10px; box-shadow: 0 4px 20px rgba(0,0,0,0.1); }}
                    h1 {{ color: #3370ff; border-bottom: 2px solid #3370ff; padding-bottom: 10px; }}
                    .summary {{ display: grid; grid-template-columns: repeat(4, 1fr); gap: 20px; margin: 30px 0; }}
                    .stat-card {{ background: #f8f9fa; padding: 20px; border-radius: 8px; text-align: center; }}
                    .stat-card.healthy {{ border-left: 5px solid #52c41a; }}
                    .stat-card.warning {{ border-left: 5px solid #faad14; }}
                    .stat-card.critical {{ border-left: 5px solid #ff4d4f; }}
                    .stat-value {{ font-size: 2em; font-weight: bold; margin: 10px 0; }}
                    .check-section {{ margin: 30px 0; }}
                    .check-item {{ padding: 15px; margin: 10px 0; border-left: 5px solid #ddd; background: #f8f9fa; border-radius: 5px; }}
                    .check-item.healthy {{ border-left-color: #52c41a; }}
                    .check-item.warning {{ border-left-color: #faad14; }}
                    .check-item.critical {{ border-left-color: #ff4d4f; }}
                    .status-badge {{ display: inline-block; padding: 4px 12px; border-radius: 20px; font-size: 0.9em; margin-right: 10px; }}
                    .status-healthy {{ background: #f6ffed; color: #52c41a; border: 1px solid #b7eb8f; }}
                    .status-warning {{ background: #fff7e6; color: #faad14; border: 1px solid #ffd591; }}
                    .status-critical {{ background: #fff2f0; color: #ff4d4f; border: 1px solid #ffccc7; }}
                </style>
            </head>
            <body>
                <div class="container">
                    <h1>🚀 HKTech-Agent 健康检查报告</h1>
                    <p>生成时间: {self.results.get('timestamp', 'N/A')}</p>
                    
                    <div class="summary">
                        <div class="stat-card {self.results['summary']['overall_status']}">
                            <div>总体状态</div>
                            <div class="stat-value">{self.results['summary']['overall_status'].upper()}</div>
                        </div>
                        <div class="stat-card">
                            <div>总检查项</div>
                            <div class="stat-value">{self.results['summary']['total']}</div>
                        </div>
                        <div class="stat-card">
                            <div>通过</div>
                            <div class="stat-value">{self.results['summary']['passed']}</div>
                        </div>
                        <div class="stat-card">
                            <div>失败</div>
                            <div class="stat-value">{self.results['summary']['failed']}</div>
                        </div>
                    </div>
            """
            
            # 添加每个检查项
            for check_name, check_result in self.results.get("checks", {}).items():
                status = check_result.get("status", "unknown")
                description = check_result.get("description", check_name)
                
                status_class = {
                    "healthy": "status-healthy",
                    "degraded": "status-warning", 
                    "unhealthy": "status-critical",
                    "warning": "status-warning"
                }.get(status, "")
                
                html += f"""
                    <div class="check-section">
                        <h3>{description}</h3>
                        <div class="check-item {status}">
                            <span class="status-badge {status_class}">{status.upper()}</span>
                            <span>{check_result.get('details', {}).get('summary', '')}</span>
                        </div>
                    </div>
                """
            
            html += """
                </div>
            </body>
            </html>
            """
            return html
        
        else:  # text格式
            report_lines = []
            report_lines.append("=" * 70)
            report_lines.append("🚀 HKTech-Agent 健康检查报告")
            report_lines.append("=" * 70)
            report_lines.append(f"生成时间: {self.results.get('timestamp', 'N/A')}")
            report_lines.append(f"系统: {self.results.get('system', 'N/A')}")
            report_lines.append(f"Python版本: {self.results.get('python_version', 'N/A')}")
            report_lines.append("")
            
            summary = self.results.get("summary", {})
            report_lines.append("📊 检查摘要:")
            report_lines.append(f"  总体状态: {summary.get('overall_status', 'unknown').upper()}")
            report_lines.append(f"  总检查项: {summary.get('total', 0)}")
            report_lines.append(f"  通过: {summary.get('passed', 0)}")
            report_lines.append(f"  失败: {summary.get('failed', 0)}")
            report_lines.append(f"  关键失败: {summary.get('critical_failed', 0)}")
            report_lines.append(f"  耗时: {summary.get('duration_seconds', 0)}秒")
            report_lines.append("")
            
            report_lines.append("🔍 详细检查结果:")
            for check_name, check_result in self.results.get("checks", {}).items():
                status = check_result.get("status", "unknown")
                description = check_result.get("description", check_name)
                critical = "⚠️  " if check_result.get("critical") else "   "
                
                status_symbol = {
                    "healthy": "✅",
                    "degraded": "🟡",
                    "unhealthy": "❌",
                    "warning": "⚠️"
                }.get(status, "❓")
                
                report_lines.append(f"{critical}{status_symbol} {description}: {status.upper()}")
                
                if "error" in check_result:
                    report_lines.append(f"     错误: {check_result['error']}")
                
                details = check_result.get("details", {})
                if details:
                    for key, value in details.items():
                        if isinstance(value, (int, float)):
                            report_lines.append(f"     {key}: {value}")
            
            report_lines.append("")
            report_lines.append("=" * 70)
            
            return "\n".join(report_lines)


# 全局健康检查器实例
_health_checker = None

def get_health_checker(config: Optional[Dict[str, Any]] = None) -> HealthCheck:
    """获取全局健康检查器实例"""
    global _health_checker
    if _health_checker is None:
        _health_checker = HealthCheck(config)
    return _health_checker

def run_health_check(format: str = "text", send_alerts: bool = False) -> str:
    """运行健康检查并返回报告"""
    checker = get_health_checker()
    checker.run_all_checks(send_alerts=send_alerts)
    return checker.generate_report(format)


if __name__ == "__main__":
    # 命令行入口点
    import argparse
    
    parser = argparse.ArgumentParser(description="HKTech-Agent 健康检查工具")
    parser.add_argument("--format", choices=["text", "json", "html"], 
                       default="text", help="输出格式")
    parser.add_argument("--output", help="输出文件路径（可选）")
    parser.add_argument("--send-alerts", action="store_true", 
                       help="发送告警（如果告警模块已配置）")
    
    args = parser.parse_args()
    
    print("🧪 运行系统健康检查...")
    report = run_health_check(args.format, send_alerts=args.send_alerts)
    
    if args.output:
        with open(args.output, 'w', encoding='utf-8') as f:
            f.write(report)
        print(f"✅ 报告已保存到: {args.output}")
    else:
        print(report)