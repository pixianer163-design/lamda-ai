#!/usr/bin/env python3
"""
飞书发送器 - 双模式支持
模式1: API方式 (App ID + Secret)
模式2: Webhook方式 (Webhook URL)
"""

import json
import requests
from typing import Optional

class FeishuSender:
    """飞书消息发送器"""
    
    def __init__(self, config_path: str = "/opt/hktech-agent/config/feishu_config.json"):
        """初始化，加载配置"""
        with open(config_path, 'r') as f:
            self.config = json.load(f)
        
        self.app_id = self.config.get('app_id')
        self.app_secret = self.config.get('app_secret')
        self.chat_id = self.config.get('chat_id')
        self.webhook_url = self.config.get('webhook_url')
        
        self.token = None
    
    def _get_token(self) -> Optional[str]:
        """获取access_token"""
        if not self.app_id or not self.app_secret:
            return None
        
        try:
            response = requests.post(
                "https://open.feishu.cn/open-apis/auth/v3/tenant_access_token/internal",
                json={"app_id": self.app_id, "app_secret": self.app_secret},
                timeout=10
            )
            result = response.json()
            if result.get("code") == 0:
                return result.get("tenant_access_token")
        except:
            pass
        return None
    
    def send_by_api(self, content: str) -> bool:
        """使用API发送消息"""
        token = self._get_token()
        if not token:
            print("❌ 无法获取token")
            return False
        
        if not self.chat_id:
            print("❌ 未配置chat_id")
            return False
        
        try:
            response = requests.post(
                f"https://open.feishu.cn/open-apis/im/v1/messages?receive_id_type=chat_id",
                headers={"Authorization": f"Bearer {token}"},
                json={
                    "receive_id": self.chat_id,
                    "msg_type": "text",
                    "content": json.dumps({"text": content})
                },
                timeout=10
            )
            result = response.json()
            if result.get("code") == 0:
                print("✅ API方式发送成功")
                return True
            else:
                print(f"❌ API发送失败: {result.get('msg')}")
                return False
        except Exception as e:
            print(f"❌ API请求异常: {e}")
            return False
    
    def send_by_webhook(self, content: str) -> bool:
        """使用Webhook发送消息"""
        if not self.webhook_url:
            print("❌ 未配置webhook_url")
            return False
        
        try:
            response = requests.post(
                self.webhook_url,
                json={"msg_type": "text", "content": {"text": content}},
                timeout=10
            )
            if response.status_code == 200:
                print("✅ Webhook方式发送成功")
                return True
            else:
                print(f"❌ Webhook发送失败: {response.status_code}")
                return False
        except Exception as e:
            print(f"❌ Webhook请求异常: {e}")
            return False
    
    def send(self, content: str, prefer_api: bool = True) -> bool:
        """
        发送消息，自动选择方式
        
        Args:
            content: 消息内容
            prefer_api: 优先使用API方式
        
        Returns:
            是否发送成功
        """
        if prefer_api and self.app_id:
            # 先尝试API方式
            if self.send_by_api(content):
                return True
            # API失败，尝试Webhook
            print("⚠️ API方式失败，尝试Webhook...")
            return self.send_by_webhook(content)
        else:
            # 直接使用Webhook
            return self.send_by_webhook(content)


# 测试
if __name__ == "__main__":
    print("🧪 测试飞书发送器")
    print("="*60)
    
    sender = FeishuSender()
    
    content = """🧪 双模式测试消息

时间: 2026-02-17 22:20
状态: 测试发送

如果收到此消息，说明配置正确！"""
    
    print("\n尝试发送消息...")
    success = sender.send(content, prefer_api=True)
    
    if success:
        print("\n✅ 测试通过！")
    else:
        print("\n❌ 测试失败，请检查配置")
