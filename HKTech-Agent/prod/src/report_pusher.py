#!/usr/bin/env python3
"""
恒生 Agent 报告推送程序

职责：
1. 读取主程序生成的分析结果
2. 格式化报告内容
3. 推送到飞书群组

运行方式：
- 手动：python3 report_pusher.py
- 定时：crontab 调用

依赖：
- 主程序先生成数据文件
"""

import json
import os
import sys
import requests
from datetime import datetime
from typing import Dict, Optional

# 配置
import os

# 智能数据目录检测
def detect_data_dir():
    """检测数据目录"""
    # 1. 环境变量优先
    env_dir = os.environ.get('DATA_DIR')
    if env_dir and os.path.exists(env_dir):
        return env_dir
    
    # 2. 尝试标准路径
    standard_paths = [
        "/opt/hktech-agent/data",
        "/opt/hktech-agent/HKTech-Agent/data",
        os.path.join(os.path.dirname(__file__), '../../data')
    ]
    
    for path in standard_paths:
        abs_path = os.path.abspath(path)
        if os.path.exists(abs_path):
            # 检查是否有决策文件
            import glob
            if glob.glob(os.path.join(abs_path, "decisions_*.json")):
                return abs_path
    
    # 默认返回第一个
    return "/opt/hktech-agent/data"

DATA_DIR = detect_data_dir()
CONFIG_FILE = "/opt/hktech-agent/config/push_config.json"
FEISHU_CONFIG_FILE = "/opt/hktech-agent/config/feishu_config.json"


class ReportPusher:
    """报告推送器"""
    
    def __init__(self):
        self.config = self._load_config()
        self.feishu_config = self._load_feishu_config()
        self.session = requests.Session()
    
    def _load_config(self) -> Dict:
        """加载推送配置"""
        try:
            with open(CONFIG_FILE, "r", encoding="utf-8") as f:
                return json.load(f)
        except Exception as e:
            print(f"⚠️ 加载推送配置失败：{e}")
            return {
                "enabled": True,
                "groups": [],
                "retry": {"max_attempts": 3, "delay_seconds": 5},
                "fallback": {"enabled": True}
            }
    
    def _load_feishu_config(self) -> Dict:
        """加载飞书配置"""
        try:
            with open(FEISHU_CONFIG_FILE, "r", encoding="utf-8") as f:
                return json.load(f)
        except Exception as e:
            print(f"⚠️ 加载飞书配置失败：{e}")
            return {}
    
    def _load_today_decisions(self) -> Optional[Dict]:
        """读取今日投资决策"""
        today = datetime.now().strftime("%Y%m%d")
        decisions_file = os.path.join(DATA_DIR, f"decisions_{today}.json")
        
        # 尝试读取今日文件
        if os.path.exists(decisions_file):
            try:
                with open(decisions_file, "r", encoding="utf-8") as f:
                    return json.load(f)
            except Exception as e:
                print(f"⚠️ 读取今日决策失败：{e}")
        
        # 回退到 latest 文件
        latest_file = os.path.join(DATA_DIR, "decisions_latest.json")
        if os.path.exists(latest_file):
            try:
                with open(latest_file, "r", encoding="utf-8") as f:
                    return json.load(f)
            except Exception as e:
                print(f"⚠️ 读取最新决策失败：{e}")
        
        # 尝试读取最近的日期文件
        try:
            pattern = os.path.join(DATA_DIR, "decisions_*.json")
            files = sorted([f for f in os.listdir(DATA_DIR) if f.startswith("decisions_") and f.endswith(".json")])
            if files:
                latest = files[-1]
                with open(os.path.join(DATA_DIR, latest), "r", encoding="utf-8") as f:
                    return json.load(f)
        except Exception as e:
            print(f"⚠️ 读取最近决策失败：{e}")
        
        return None
    
    def _load_market_data(self) -> Optional[Dict]:
        """读取市场数据"""
        latest_file = os.path.join(DATA_DIR, "market_data_latest.json")
        
        if os.path.exists(latest_file):
            try:
                with open(latest_file, "r", encoding="utf-8") as f:
                    return json.load(f)
            except Exception as e:
                print(f"⚠️ 读取市场数据失败：{e}")
        
        return None
    
    def _format_report(self, decisions: Dict, market_data: Dict) -> str:
        """格式化报告内容"""
        date = decisions.get("date", datetime.now().strftime("%Y%m%d"))
        timestamp = decisions.get("timestamp", "")
        
        # 报告标题
        report = f"""📊 **恒生 Agent - 投资决策报告**
⏰ 时间：{timestamp[:16] if timestamp else 'N/A'}

━━━━━━━━━━━━━━━━━━━━

🎯 **今日决策**
"""
        
        # 投资决策
        stock_names = {
            "00700": "腾讯控股",
            "09988": "阿里巴巴",
            "03690": "美团-W",
            "01810": "小米集团",
            "01024": "快手-W",
            "09618": "京东集团"
        }
        
        action_emoji = {"buy": "🟢", "sell": "🔴", "hold": "🟡"}
        
        for code, dec in decisions.get("decisions", {}).items():
            name = stock_names.get(code, code)
            action = dec.get("action", "hold")
            conf = dec.get("confidence", 0.5)
            reason = dec.get("reason", "")[:50]
            
            emoji = action_emoji.get(action, "⚪")
            report += f"""
{emoji} **{name} ({code})**
   决策：{action.upper()}
   置信度：{conf:.0%}
   理由：{reason if reason else 'N/A'}
"""
        
        # 世界模型预测
        wm = decisions.get("world_model", {})
        if wm.get("enabled"):
            report += f"""
━━━━━━━━━━━━━━━━━━━━

🧠 **世界模型预测**
   建议：{wm.get("recommendation", "N/A").upper()}
   置信度：{wm.get("confidence", 0):.0%}
"""
        
        # 市场数据摘要
        if market_data:
            report += """
━━━━━━━━━━━━━━━━━━━━

📈 **市场数据**
"""
            for code, data in market_data.items():
                name = stock_names.get(code, code)
                price = data.get("price", 0)
                change = data.get("change_pct", 0)
                trend = data.get("trend", "N/A")
                
                change_str = f"{change:+.2f}%" if change else "N/A"
                report += f"   {name}: ¥{price:.2f} ({change_str}) [{trend}]\n"
        
        # 报告尾部
        report += """
━━━━━━━━━━━━━━━━━━━━

⚠️ **风险提示**: 本报告仅供参考，不构成投资建议。
📁 数据来源：恒生 Agent 自动分析系统
"""
        
        return report
    
    def _send_feishu_webhook(self, text: str) -> bool:
        """通过 Webhook 发送飞书消息"""
        webhook_url = self.feishu_config.get("webhook_url")
        if not webhook_url:
            print("   ⚠️ 飞书 Webhook URL 未配置")
            return False
        
        payload = {
            "msg_type": "text",
            "content": {
                "text": text
            }
        }
        
        try:
            response = self.session.post(webhook_url, json=payload, timeout=10)
            if response.status_code == 200:
                result = response.json()
                if result.get("StatusCode") == 0 or result.get("code") == 0:
                    return True
            
            print(f"   ⚠️ 飞书返回错误：{response.text}")
            return False
        except Exception as e:
            print(f"   ❌ 发送失败：{e}")
            return False
    
    def _send_feishu_chat(self, text: str, chat_id: str) -> bool:
        """通过飞书 API 发送消息到指定群组"""
        app_id = self.feishu_config.get("app_id")
        app_secret = self.feishu_config.get("app_secret")
        
        if not app_id or not app_secret:
            print("   ⚠️ 飞书 App ID/Secret 未配置")
            return False
        
        # 1. 获取 tenant_access_token
        token_url = "https://open.feishu.cn/open-apis/auth/v3/tenant_access_token/internal"
        token_payload = {
            "app_id": app_id,
            "app_secret": app_secret
        }
        
        try:
            token_response = self.session.post(token_url, json=token_payload, timeout=10)
            token_result = token_response.json()
            
            if token_result.get("code") != 0:
                print(f"   ⚠️ 获取 Token 失败：{token_result}")
                return False
            
            tenant_token = token_result.get("tenant_access_token")
            
            # 2. 发送消息 - 使用 post 方式（query params）
            msg_url = "https://open.feishu.cn/open-apis/im/v1/messages"
            headers = {
                "Authorization": f"Bearer {tenant_token}",
                "Content-Type": "application/json"
            }
            # 飞书 API v1 要求：receive_id 在 query params，content 必须是 JSON 字符串
            import json as json_lib
            msg_payload = {
                "receive_id": chat_id,
                "msg_type": "text",
                "content": json_lib.dumps({"text": text})  # 必须是 JSON 字符串，不是 dict！
            }
            
            msg_response = self.session.post(msg_url, headers=headers, params={"receive_id_type": "chat_id"}, json=msg_payload, timeout=10)
            msg_result = msg_response.json()
            
            if msg_result.get("code") == 0:
                return True
            else:
                print(f"   ⚠️ 发送消息失败：{msg_result}")
                return False
                
        except Exception as e:
            print(f"   ❌ 发送失败：{e}")
            return False
    
    def push(self) -> bool:
        """执行推送"""
        print("\n" + "="*60)
        print("📬 恒生 Agent - 报告推送")
        print(f"⏰ {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        print("="*60)
        
        # 1. 加载数据
        print("\n1️⃣ 加载数据...")
        decisions = self._load_today_decisions()
        if not decisions:
            print("   ❌ 未找到投资决策数据，跳过推送")
            return False
        
        market_data = self._load_market_data()
        print(f"   ✅ 数据加载成功")
        
        # 2. 格式化报告
        print("\n2️⃣ 格式化报告...")
        report_text = self._format_report(decisions, market_data)
        print(f"   ✅ 报告生成成功 ({len(report_text)} 字符)")
        
        # 3. 推送
        print("\n3️⃣ 推送报告...")
        success_count = 0
        total_count = 0
        
        groups = self.config.get("groups", [])
        
        for group in groups:
            if not group.get("enabled", False):
                continue
            
            group_name = group.get("name", "未知群组")
            chat_id = group.get("chat_id")
            use_webhook = group.get("use_webhook", False)
            
            if not chat_id:
                print(f"   ⚠️ {group_name}: Chat ID 缺失")
                continue
            
            total_count += 1
            print(f"   → {group_name}...")
            
            # 重试逻辑
            max_attempts = self.config.get("retry", {}).get("max_attempts", 3)
            delay = self.config.get("retry", {}).get("delay_seconds", 5)
            
            success = False
            for attempt in range(max_attempts):
                if use_webhook:
                    success = self._send_feishu_webhook(report_text)
                else:
                    success = self._send_feishu_chat(report_text, chat_id)
                
                if success:
                    print(f"      ✅ 发送成功")
                    success_count += 1
                    break
                else:
                    print(f"      ⚠️ 尝试 {attempt+1}/{max_attempts} 失败")
                    if attempt < max_attempts - 1:
                        import time
                        time.sleep(delay)
            
            if not success:
                print(f"      ❌ {group_name}: 所有重试失败")
        
        # 4. 总结
        print("\n" + "="*60)
        print(f"✅ 推送完成：{success_count}/{total_count} 个群组")
        print("="*60 + "\n")
        
        return success_count > 0


def main():
    """主函数"""
    pusher = ReportPusher()
    success = pusher.push()
    
    # 返回状态码（供 cron 使用）
    sys.exit(0 if success else 1)


if __name__ == "__main__":
    main()
