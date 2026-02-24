#!/usr/bin/env python3
"""
飞书发送器 - 使用 open_id 方式（修复版）
基于直接 API 调用，不依赖 OpenClaw 插件
"""

import json
import os
import requests
from typing import Optional

class FeishuSender:
    """飞书消息发送器"""
    
    def __init__(self, config_path: str = None):
        """初始化，加载配置"""
<<<<<<< HEAD
        try:
            with open(config_path, 'r') as f:
                self.config = json.load(f)
        except:
            # 默认配置
            self.config = {}
=======
        if config_path is None:
            config_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), '../local_config/feishu_config.json')
        with open(config_path, 'r') as f:
            self.config = json.load(f)
>>>>>>> origin/main
        
        self.app_id = self.config.get('app_id', 'cli_a918213443b8dcd6')
        self.app_secret = self.config.get('app_secret', 'ybp63RNb0sH2PQvOLyBKFcRwhGTwBD4z')
        self.chat_id = self.config.get('chat_id', 'oc_1c509d279aa4bd1b785a57f1cc13427c')
        self.open_id = self.config.get('open_id')  # 不使用默认值
        self.webhook_url = self.config.get('webhook_url')
        
        self.token = None
    
    def _get_token(self) -> Optional[str]:
        """获取 access_token"""
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
                self.token = result.get("tenant_access_token")
                return self.token
        except Exception as e:
            print(f"❌ 获取 token 异常：{e}")
        return None
    
    def send_by_api(self, content: str) -> bool:
        """使用 API 发送消息（优先 chat_id）"""
        token = self._get_token()
        if not token:
            print("❌ 无法获取 token")
            return False
        
        # 优先使用 chat_id（已验证有效）
        receive_id = self.chat_id or self.open_id
        if not receive_id:
            print("❌ 未配置 chat_id 或 open_id")
            return False
        
        # 根据 ID 类型判断 receive_id_type
        if receive_id.startswith('oc_'):
            receive_id_type = 'chat_id'
        elif receive_id.startswith('ou_'):
            receive_id_type = 'open_id'
        else:
            receive_id_type = 'chat_id'  # 默认
        
        try:
            response = requests.post(
                f"https://open.feishu.cn/open-apis/im/v1/messages?receive_id_type={receive_id_type}",
                headers={"Authorization": f"Bearer {token}"},
                json={
                    "receive_id": receive_id,
                    "msg_type": "text",
                    "content": json.dumps({"text": content})
                },
                timeout=10
            )
            result = response.json()
            if result.get("code") == 0:
                msg_id = result.get('data', {}).get('message_id', 'unknown')
                print(f"✅ API 方式发送成功 (msg_id: {msg_id})")
                return True
            else:
                print(f"❌ API 发送失败：{result.get('msg')} (code: {result.get('code')})")
                return False
        except Exception as e:
            print(f"❌ API 请求异常：{e}")
            return False
    
    def send_by_webhook(self, content: str) -> bool:
        """使用 Webhook 发送消息"""
        if not self.webhook_url:
            print("⚠️ 未配置 webhook_url")
            return False
        
        try:
            response = requests.post(
                self.webhook_url,
                json={"msg_type": "text", "content": {"text": content}},
                timeout=10
            )
            if response.status_code == 200:
                print("✅ Webhook 方式发送成功")
                return True
            else:
                print(f"❌ Webhook 发送失败：{response.status_code}")
                return False
        except Exception as e:
            print(f"❌ Webhook 请求异常：{e}")
            return False
    
    def send(self, content: str, prefer_api: bool = True) -> bool:
        """
        发送消息，优先使用 API 方式
        
        Args:
            content: 消息内容
            prefer_api: 优先使用 API 方式
        
        Returns:
            是否发送成功
        """
        if prefer_api and self.app_id:
            # 先尝试 API 方式
            if self.send_by_api(content):
                return True
            # API 失败，尝试 Webhook
            print("⚠️ API 方式失败，尝试 Webhook...")
            return self.send_by_webhook(content)
        else:
            # 直接使用 Webhook
            return self.send_by_webhook(content)


# 测试
if __name__ == "__main__":
    print("🧪 测试飞书发送器（open_id 版本）")
    print("="*60)
    
    sender = FeishuSender()
    
    content = f"""🧪 飞书发送器测试

时间：{requests.get('http://worldtimeapi.org/api/timezone/Asia/Shanghai').json().get('datetime', 'unknown') if False else '2026-02-23 13:00'}
状态：测试发送
方式：API (open_id)

如果收到此消息，说明配置正确！"""
    
    print("\n尝试发送消息...")
    success = sender.send(content, prefer_api=True)
    
    if success:
        print("\n✅ 测试通过！")
    else:
        print("\n❌ 测试失败，请检查配置")
