#!/usr/bin/env python3
"""
告警系统模块

提供多通道告警功能：控制台、邮件、Slack、Webhook等。
支持分级告警和静默配置。
"""

import os
import sys
import json
import smtplib
import logging
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart
from typing import Dict, List, Any, Optional, Union
from datetime import datetime
from pathlib import Path

# 尝试导入可选依赖
try:
    import requests
    REQUESTS_AVAILABLE = True
except ImportError:
    REQUESTS_AVAILABLE = False
    requests = None

try:
    from logger import get_logger
    _logger = get_logger()
    LOGGER_AVAILABLE = True
except ImportError:
    LOGGER_AVAILABLE = False
    _logger = None


class AlertLevel:
    """告警级别"""
    INFO = "info"
    WARNING = "warning"
    ERROR = "error"
    CRITICAL = "critical"
    
    @staticmethod
    def is_valid(level: str) -> bool:
        """验证告警级别是否有效"""
        return level in [AlertLevel.INFO, AlertLevel.WARNING, AlertLevel.ERROR, AlertLevel.CRITICAL]
    
    @staticmethod
    def get_numeric_level(level: str) -> int:
        """获取告警级别的数值表示（用于比较）"""
        levels = {
            AlertLevel.INFO: 0,
            AlertLevel.WARNING: 1,
            AlertLevel.ERROR: 2,
            AlertLevel.CRITICAL: 3
        }
        return levels.get(level, 0)


class AlertChannel:
    """告警通道基类"""
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        self.config = config or {}
        self.enabled = self.config.get('enabled', True)
        self.min_level = self.config.get('min_level', AlertLevel.WARNING)
        
    def send(self, title: str, message: str, level: str = AlertLevel.ERROR, 
             metadata: Optional[Dict[str, Any]] = None) -> bool:
        """
        发送告警
        
        Args:
            title: 告警标题
            message: 告警消息
            level: 告警级别
            metadata: 附加元数据
            
        Returns:
            是否发送成功
        """
        if not self.enabled:
            return False
        
        # 检查级别是否达到最小要求
        if AlertLevel.get_numeric_level(level) < AlertLevel.get_numeric_level(self.min_level):
            return False
        
        try:
            return self._send_impl(title, message, level, metadata or {})
        except Exception as e:
            if LOGGER_AVAILABLE and _logger:
                _logger.error(f"告警发送失败: {e}", exc_info=True)
            else:
                print(f"⚠️ 告警发送失败: {e}")
            return False
    
    def _send_impl(self, title: str, message: str, level: str, 
                   metadata: Dict[str, Any]) -> bool:
        """具体通道的实现"""
        raise NotImplementedError


class ConsoleAlertChannel(AlertChannel):
    """控制台告警通道"""
    
    def _send_impl(self, title: str, message: str, level: str, 
                   metadata: Dict[str, Any]) -> bool:
        """在控制台打印告警"""
        level_colors = {
            AlertLevel.INFO: "\033[94m",      # 蓝色
            AlertLevel.WARNING: "\033[93m",   # 黄色
            AlertLevel.ERROR: "\033[91m",     # 红色
            AlertLevel.CRITICAL: "\033[95m"   # 紫色
        }
        reset_color = "\033[0m"
        
        color = level_colors.get(level, "\033[0m")
        timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        
        print(f"{color}[{timestamp}] [{level.upper()}] {title}{reset_color}")
        print(f"   {message}")
        
        if metadata:
            print(f"   元数据: {json.dumps(metadata, indent=2, ensure_ascii=False)}")
        
        return True


class FileAlertChannel(AlertChannel):
    """文件告警通道（写入日志文件）"""
    
    def _send_impl(self, title: str, message: str, level: str,
                   metadata: Dict[str, Any]) -> bool:
        """写入文件"""
        log_dir = self.config.get('log_dir', 'logs/alerts')
        Path(log_dir).mkdir(parents=True, exist_ok=True)
        
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        filename = f"alert_{timestamp}_{level}.log"
        filepath = Path(log_dir) / filename
        
        alert_data = {
            "timestamp": datetime.now().isoformat(),
            "level": level,
            "title": title,
            "message": message,
            "metadata": metadata
        }
        
        try:
            with open(filepath, 'w', encoding='utf-8') as f:
                json.dump(alert_data, f, indent=2, ensure_ascii=False)
            return True
        except Exception as e:
            print(f"文件告警写入失败: {e}")
            return False


class EmailAlertChannel(AlertChannel):
    """邮件告警通道"""
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        super().__init__(config)
        
        # 邮件配置
        self.smtp_server = self.config.get('smtp_server', 'smtp.gmail.com')
        self.smtp_port = self.config.get('smtp_port', 587)
        self.smtp_username = self.config.get('smtp_username', '')
        self.smtp_password = self.config.get('smtp_password', '')
        self.sender_email = self.config.get('sender_email', '')
        self.recipient_emails = self.config.get('recipient_emails', [])
        
        # 检查必要配置
        if not all([self.smtp_username, self.smtp_password, self.sender_email, self.recipient_emails]):
            self.enabled = False
            if LOGGER_AVAILABLE and _logger:
                _logger.warning("邮件告警通道配置不完整，已禁用")
    
    def _send_impl(self, title: str, message: str, level: str,
                   metadata: Dict[str, Any]) -> bool:
        """发送邮件"""
        if not self.enabled:
            return False
        
        try:
            # 创建邮件
            msg = MIMEMultipart('alternative')
            msg['Subject'] = f"[HKTech-Agent {level.upper()}] {title}"
            msg['From'] = self.sender_email
            msg['To'] = ', '.join(self.recipient_emails)
            
            # 纯文本版本
            text = f"""
告警级别: {level.upper()}
标题: {title}
时间: {datetime.now().isoformat()}
消息: {message}

元数据: {json.dumps(metadata, indent=2, ensure_ascii=False)}
"""
            
            # HTML版本
            html = f"""
<!DOCTYPE html>
<html>
<body>
    <h2 style="color: {'blue' if level == AlertLevel.INFO else 'orange' if level == AlertLevel.WARNING else 'red' if level == AlertLevel.ERROR else 'purple'}">
        [{level.upper()}] {title}
    </h2>
    <p><strong>时间:</strong> {datetime.now().isoformat()}</p>
    <p><strong>消息:</strong> {message}</p>
    <h3>元数据:</h3>
    <pre>{json.dumps(metadata, indent=2, ensure_ascii=False)}</pre>
</body>
</html>
"""
            
            part1 = MIMEText(text, 'plain', 'utf-8')
            part2 = MIMEText(html, 'html', 'utf-8')
            msg.attach(part1)
            msg.attach(part2)
            
            # 发送邮件
            with smtplib.SMTP(self.smtp_server, self.smtp_port) as server:
                server.starttls()
                server.login(self.smtp_username, self.smtp_password)
                server.sendmail(self.sender_email, self.recipient_emails, msg.as_string())
            
            if LOGGER_AVAILABLE and _logger:
                _logger.info(f"邮件告警已发送: {title}")
            
            return True
            
        except Exception as e:
            if LOGGER_AVAILABLE and _logger:
                _logger.error(f"邮件告警发送失败: {e}", exc_info=True)
            return False


class SlackAlertChannel(AlertChannel):
    """Slack告警通道"""
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        super().__init__(config)
        
        if not REQUESTS_AVAILABLE:
            self.enabled = False
            if LOGGER_AVAILABLE and _logger:
                _logger.warning("requests模块未安装，Slack告警通道已禁用")
            return
        
        self.webhook_url = self.config.get('webhook_url', '')
        self.channel = self.config.get('channel', '#alerts')
        self.username = self.config.get('username', 'HKTech-Agent')
        self.icon_emoji = self.config.get('icon_emoji', ':robot_face:')
        
        if not self.webhook_url:
            self.enabled = False
            if LOGGER_AVAILABLE and _logger:
                _logger.warning("Slack webhook URL未配置，Slack告警通道已禁用")
    
    def _send_impl(self, title: str, message: str, level: str,
                   metadata: Dict[str, Any]) -> bool:
        """发送Slack消息"""
        if not self.enabled or not REQUESTS_AVAILABLE:
            return False
        
        try:
            # 根据级别选择颜色
            colors = {
                AlertLevel.INFO: "#36a64f",      # 绿色
                AlertLevel.WARNING: "#ffcc00",   # 黄色
                AlertLevel.ERROR: "#ff0000",     # 红色
                AlertLevel.CRITICAL: "#8b00ff"   # 紫色
            }
            
            # 创建Slack消息
            slack_data = {
                "channel": self.channel,
                "username": self.username,
                "icon_emoji": self.icon_emoji,
                "attachments": [{
                    "color": colors.get(level, "#808080"),
                    "title": f"[{level.upper()}] {title}",
                    "text": message,
                    "fields": [
                        {
                            "title": "时间",
                            "value": datetime.now().isoformat(),
                            "short": True
                        },
                        {
                            "title": "级别",
                            "value": level.upper(),
                            "short": True
                        }
                    ],
                    "footer": "HKTech-Agent 告警系统",
                    "ts": datetime.now().timestamp()
                }]
            }
            
            # 添加元数据字段
            if metadata:
                metadata_text = "\n".join([f"• {k}: {v}" for k, v in metadata.items()])
                slack_data["attachments"][0]["fields"].append({
                    "title": "元数据",
                    "value": metadata_text,
                    "short": False
                })
            
            # 发送请求
            response = requests.post(
                self.webhook_url,
                json=slack_data,
                headers={'Content-Type': 'application/json'},
                timeout=10
            )
            
            if response.status_code == 200:
                if LOGGER_AVAILABLE and _logger:
                    _logger.info(f"Slack告警已发送: {title}")
                return True
            else:
                error_msg = f"Slack API错误: {response.status_code} - {response.text}"
                if LOGGER_AVAILABLE and _logger:
                    _logger.error(error_msg)
                return False
                
        except Exception as e:
            if LOGGER_AVAILABLE and _logger:
                _logger.error(f"Slack告警发送失败: {e}", exc_info=True)
            return False


class AlertManager:
    """告警管理器"""
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        self.config = config or {}
        self.channels = []
        self._init_channels()
        
        # 告警历史（用于去重和频率限制）
        self.alert_history = []
        self.max_history_size = self.config.get('max_history_size', 100)
        
        # 频率限制（秒）
        self.rate_limit = self.config.get('rate_limit', 300)  # 默认5分钟
        
    def _init_channels(self):
        """初始化告警通道"""
        channels_config = self.config.get('channels', {})
        
        # 控制台通道（默认启用）
        console_config = channels_config.get('console', {'enabled': True})
        self.channels.append(ConsoleAlertChannel(console_config))
        
        # 文件通道
        file_config = channels_config.get('file', {'enabled': False})
        if file_config.get('enabled', False):
            self.channels.append(FileAlertChannel(file_config))
        
        # 邮件通道
        email_config = channels_config.get('email', {'enabled': False})
        if email_config.get('enabled', False):
            self.channels.append(EmailAlertChannel(email_config))
        
        # Slack通道
        slack_config = channels_config.get('slack', {'enabled': False})
        if slack_config.get('enabled', False):
            self.channels.append(SlackAlertChannel(slack_config))
    
    def send_alert(self, title: str, message: str, level: str = AlertLevel.ERROR,
                   metadata: Optional[Dict[str, Any]] = None, 
                   deduplicate_key: Optional[str] = None) -> bool:
        """
        发送告警
        
        Args:
            title: 告警标题
            message: 告警消息
            level: 告警级别
            metadata: 附加元数据
            deduplicate_key: 去重键（相同键的告警在一定时间内只发送一次）
            
        Returns:
            是否至少有一个通道发送成功
        """
        # 验证告警级别
        if not AlertLevel.is_valid(level):
            if LOGGER_AVAILABLE and _logger:
                _logger.warning(f"无效的告警级别: {level}，使用ERROR代替")
            level = AlertLevel.ERROR
        
        # 去重检查
        if deduplicate_key:
            current_time = datetime.now().timestamp()
            
            # 清理旧历史
            self.alert_history = [
                h for h in self.alert_history 
                if current_time - h['timestamp'] < self.rate_limit
            ]
            
            # 检查是否已有相同键的告警
            for alert in self.alert_history:
                if alert['deduplicate_key'] == deduplicate_key:
                    if LOGGER_AVAILABLE and _logger:
                        _logger.debug(f"告警已去重: {deduplicate_key}")
                    return False
        
        # 发送到所有通道
        success = False
        for channel in self.channels:
            if channel.send(title, message, level, metadata):
                success = True
        
        # 记录历史（用于去重）
        if deduplicate_key and success:
            self.alert_history.append({
                'timestamp': datetime.now().timestamp(),
                'deduplicate_key': deduplicate_key,
                'title': title,
                'level': level
            })
            
            # 限制历史大小
            if len(self.alert_history) > self.max_history_size:
                self.alert_history = self.alert_history[-self.max_history_size:]
        
        return success
    
    def send_health_alert(self, health_results: Dict[str, Any]) -> bool:
        """
        发送健康检查告警
        
        Args:
            health_results: 健康检查结果
            
        Returns:
            是否发送成功
        """
        summary = health_results.get('summary', {})
        overall_status = summary.get('overall_status', 'unknown')
        critical_failed = summary.get('critical_failed', 0)
        failed = summary.get('failed', 0)
        
        # 确定告警级别
        if overall_status == 'critical' or critical_failed > 0:
            level = AlertLevel.CRITICAL
        elif overall_status == 'degraded' or failed > 0:
            level = AlertLevel.WARNING
        else:
            level = AlertLevel.INFO
        
        # 构建消息
        timestamp = health_results.get('timestamp', 'unknown')
        title = f"系统健康检查: {overall_status.upper()}"
        
        message = f"""
系统健康检查结果:
• 总体状态: {overall_status.upper()}
• 总检查项: {summary.get('total', 0)}
• 通过: {summary.get('passed', 0)}
• 失败: {failed} (关键失败: {critical_failed})
• 耗时: {summary.get('duration_seconds', 0)}秒
• 时间: {timestamp}
"""
        # 添加失败检查项详情
        failed_checks = []
        for check_name, check_result in health_results.get('checks', {}).items():
            if check_result.get('status') in ['unhealthy', 'degraded', 'warning']:
                failed_checks.append({
                    'name': check_name,
                    'description': check_result.get('description', check_name),
                    'status': check_result.get('status'),
                    'error': check_result.get('error', '')
                })
        
        if failed_checks:
            message += "\n失败检查项:\n"
            for check in failed_checks[:5]:  # 只显示前5个
                message += f"• {check['description']}: {check['status']}"
                if check['error']:
                    message += f" ({check['error']})"
                message += "\n"
            
            if len(failed_checks) > 5:
                message += f"• ... 还有 {len(failed_checks) - 5} 个失败项\n"
        
        metadata = {
            'system': health_results.get('system'),
            'python_version': health_results.get('python_version'),
            'critical_failed': critical_failed,
            'failed': failed,
            'failed_checks': [c['name'] for c in failed_checks]
        }
        
        # 使用健康检查时间戳作为去重键的一部分
        deduplicate_key = f"health_{timestamp}"
        
        return self.send_alert(title, message, level, metadata, deduplicate_key)


# 全局告警管理器实例
_alert_manager = None

def get_alert_manager(config: Optional[Dict[str, Any]] = None) -> AlertManager:
    """获取全局告警管理器实例"""
    global _alert_manager
    if _alert_manager is None:
        _alert_manager = AlertManager(config)
    return _alert_manager

def send_alert(title: str, message: str, level: str = AlertLevel.ERROR,
               metadata: Optional[Dict[str, Any]] = None) -> bool:
    """发送告警（便捷函数）"""
    manager = get_alert_manager()
    return manager.send_alert(title, message, level, metadata)

def send_health_alert(health_results: Dict[str, Any]) -> bool:
    """发送健康检查告警（便捷函数）"""
    manager = get_alert_manager()
    return manager.send_health_alert(health_results)


if __name__ == '__main__':
    # 测试告警系统
    print("🧪 测试告警系统...")
    
    # 创建测试配置
    test_config = {
        'channels': {
            'console': {'enabled': True, 'min_level': 'info'},
            'file': {'enabled': False},
            'email': {'enabled': False},
            'slack': {'enabled': False}
        }
    }
    
    manager = AlertManager(test_config)
    
    # 测试不同级别的告警
    test_alerts = [
        ("测试信息", "这是一个信息级别告警", AlertLevel.INFO),
        ("测试警告", "这是一个警告级别告警", AlertLevel.WARNING),
        ("测试错误", "这是一个错误级别告警", AlertLevel.ERROR),
        ("测试关键", "这是一个关键级别告警", AlertLevel.CRITICAL),
    ]
    
    for title, message, level in test_alerts:
        success = manager.send_alert(title, message, level)
        print(f"  {level}: {title} - {'✅ 成功' if success else '❌ 失败'}")
    
    print("✅ 告警系统测试完成")