#!/usr/bin/env python3
"""
飞书通知模块 - 告警推送
"""

import requests
import json
import os
from datetime import datetime
from typing import Optional


class FeishuNotifier:
    """飞书通知器 - 支持环境变量和配置文件"""
    
    def __init__(self, webhook_url: Optional[str] = None):
        """
        初始化通知器
        
        Args:
            webhook_url: 飞书Webhook地址，如未提供则从环境变量或配置文件读取
        """
        # 优先级: 参数 > 环境变量 > 配置文件 > 模拟模式
        self.webhook_url = (
            webhook_url or 
            os.environ.get('FEISHU_WEBHOOK_URL') or
            self._load_webhook_from_config()
        )
        self.enabled = self.webhook_url is not None and self.webhook_url.startswith("http")
        
        if self.enabled:
            print(f"✅ 飞书通知已启用: {self.webhook_url[:50]}...")
        else:
            print(f"⚠️ 飞书通知处于模拟模式（未配置webhook）")
    
    def _load_webhook_from_config(self) -> Optional[str]:
        """从配置文件加载webhook"""
        config_paths = [
            '/opt/hktech-agent/config/feishu_webhook.txt',
            '/opt/hktech-agent/config/webhook_url.txt',
            '/opt/hktech-agent/.env'
        ]
        
        for path in config_paths:
            if os.path.exists(path):
                try:
                    with open(path, 'r') as f:
                        content = f.read().strip()
                        # 支持 KEY=VALUE 格式
                        if '=' in content:
                            for line in content.split('\n'):
                                if line.strip().startswith('FEISHU_WEBHOOK_URL') or \
                                   line.strip().startswith('WEBHOOK_URL'):
                                    return line.split('=', 1)[1].strip().strip('"').strip("'")
                        # 直接URL格式
                        elif content.startswith('http'):
                            return content
                except Exception as e:
                    print(f"⚠️ 读取配置失败 {path}: {e}")
        
        return None
    
    def send_text(self, message: str, title: Optional[str] = None) -> bool:
        """
        发送文本消息
        
        Args:
            message: 消息内容
            title: 消息标题（可选）
            
        Returns:
            是否发送成功
        """
        if not self.enabled:
            print(f"📱 [飞书模拟] {title or '消息'}:\n{message}")
            return True
        
        try:
            full_message = f"{title}\n\n{message}" if title else message
            
            payload = {
                "msg_type": "text",
                "content": {
                    "text": full_message
                }
            }
            
            response = requests.post(
                self.webhook_url,
                json=payload,
                timeout=10
            )
            
            if response.status_code == 200:
                result = response.json()
                if result.get("code") == 0:
                    print(f"✅ 飞书消息发送成功")
                    return True
                else:
                    print(f"❌ 飞书API错误: {result}")
                    return False
            else:
                print(f"❌ HTTP错误: {response.status_code}")
                return False
                
        except Exception as e:
            print(f"❌ 发送失败: {e}")
            return False
    
    def send_market_update(self, market_data: dict, trades: list) -> bool:
        """发送市场更新"""
        title = f"📊 恒生科技Agent日报 - {datetime.now().strftime('%m月%d日')}"
        
        # 市场行情
        market_text = "【市场行情】\n"
        for code, data in market_data.items():
            trend = "📈" if data.get('change_pct', 0) > 0 else "📉"
            market_text += f"{trend} {data['name']}: {data['price']} ({data['change_pct']:+.2f}%)\n"
        
        # 交易记录
        trade_text = "\n【今日交易】\n"
        if trades:
            for trade in trades:
                action_emoji = "🟢买入" if trade['action'] == 'BUY' else "🔴卖出"
                if trade['action'] == 'BUY':
                    trade_text += f"{action_emoji} {trade['name']}: {trade['shares']}股 @ {trade['executed_price']} = {trade['cost']:.2f}\n"
                else:
                    trade_text += f"{action_emoji} {trade['name']}: {trade['shares']}股 @ {trade['executed_price']} = {trade['revenue']:.2f}\n"
        else:
            trade_text += "今日无交易\n"
        
        message = market_text + trade_text
        return self.send_text(message, title)
    
    def send_alert(self, alert_type: str, content: str) -> bool:
        """发送告警"""
        emoji_map = {
            "warning": "⚠️",
            "error": "🚨",
            "info": "ℹ️",
            "success": "✅"
        }
        emoji = emoji_map.get(alert_type, "📢")
        title = f"{emoji} Agent告警"
        return self.send_text(content, title)


def test_notifier():
    """测试通知器"""
    # 无Webhook时使用模拟模式
    notifier = FeishuNotifier()
    
    # 测试市场更新
    test_data = {
        "00700": {"name": "腾讯控股", "price": 385.2, "change_pct": 1.5},
        "09988": {"name": "阿里巴巴", "price": 84.5, "change_pct": -0.8},
        "03690": {"name": "美团", "price": 132.0, "change_pct": 2.1}
    }
    
    test_trades = [
        {"action": "BUY", "name": "腾讯控股", "shares": 65, "executed_price": 385.2, "cost": 25038.0}
    ]
    
    print("="*50)
    print("测试飞书通知（模拟模式）")
    print("="*50)
    notifier.send_market_update(test_data, test_trades)


if __name__ == "__main__":
    test_notifier()
