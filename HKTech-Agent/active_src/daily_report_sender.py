#!/usr/bin/env python3
"""
恒生科技Agent - 日报推送脚本（交互式卡片版）
支持飞书交互式卡片，带按钮可操作
"""

import json
import os
import sys
import requests
from datetime import datetime
from typing import Optional

# 添加src路径
sys.path.insert(0, '/opt/hktech-agent/prod/src')

class FeishuCardSender:
    """飞书交互式卡片发送器"""
    
    def __init__(self, config_path: str = "/opt/hktech-agent/config/feishu_config.json"):
        """初始化，加载配置"""
        with open(config_path, 'r') as f:
            self.config = json.load(f)
        
        self.app_id = self.config.get('app_id')
        self.app_secret = self.config.get('app_secret')
        self.chat_id = self.config.get('chat_id')
        self.token = None
        
        # 股票名称映射
        self.stock_names = {
            "00700": "腾讯控股",
            "09988": "阿里巴巴", 
            "03690": "美团-W"
        }
    
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
                self.token = result.get("tenant_access_token")
                return self.token
        except Exception as e:
            print(f"❌ 获取token失败: {e}")
        return None
    
    def _send_card(self, card_content: dict) -> bool:
        """发送卡片消息"""
        if not self.token:
            self.token = self._get_token()
        
        if not self.token or not self.chat_id:
            print("❌ 未配置token或chat_id")
            return False
        
        try:
            url = f"https://open.feishu.cn/open-apis/im/v1/messages?receive_id_type=chat_id"
            headers = {
                "Authorization": f"Bearer {self.token}",
                "Content-Type": "application/json"
            }
            
            payload = {
                "receive_id": self.chat_id,
                "msg_type": "interactive",
                "content": json.dumps(card_content)
            }
            
            response = requests.post(url, json=payload, headers=headers, timeout=10)
            result = response.json()
            
            if result.get("code") == 0:
                print(f"✅ 交互式卡片发送成功")
                return True
            else:
                print(f"❌ 卡片发送失败: {result.get('msg')}")
                return False
                
        except Exception as e:
            print(f"❌ 请求异常: {e}")
            return False
    
    def send_daily_report_card(self, day_count: int, portfolio: dict, market_data: dict, trades: list = None):
        """发送日报交互式卡片"""
        today = datetime.now().strftime('%Y年%m月%d日')
        
        # 组合信息
        cash = portfolio.get("cash", 0)
        holdings = portfolio.get("holdings", {})
        total_value = cash
        
        # 构建持仓文本
        holdings_text = []
        for code, info in holdings.items():
            shares = info.get("shares", 0)
            cost = info.get("cost_basis", 0)
            total_value += cost
            name = info.get('name', self.stock_names.get(code, code))
            holdings_text.append(f"**{name}**: {shares}股")
        
        # 构建行情文本
        market_text = []
        for code, mdata in market_data.items():
            name = self.stock_names.get(code, code)
            price = mdata.get('price', 0)
            change_pct = mdata.get('change_pct', 0)
            trend = "📈" if change_pct >= 0 else "📉"
            market_text.append(f"{trend} **{name}**: ¥{price:.2f} ({change_pct:+.2f}%)")
        
        # 构建卡片内容
        card = {
            "config": {"wide_screen_mode": True},
            "header": {
                "title": {
                    "tag": "plain_text",
                    "content": f"🌅 盘前学习报告 - Day {day_count}"
                },
                "template": "blue"
            },
            "elements": [
                {
                    "tag": "div",
                    "text": {
                        "tag": "lark_md",
                        "content": f"**📅 {today}**\n\n💰 **组合概况**\n• 总市值: ¥{total_value:,.0f}\n• 现金: ¥{cash:,.0f}"
                    }
                },
                {"tag": "hr"},
                {
                    "tag": "div",
                    "text": {
                        "tag": "lark_md",
                        "content": "**📈 当前持仓**\n" + "\n".join(holdings_text) if holdings_text else "**📈 当前持仓**\n• 暂无持仓"
                    }
                },
                {"tag": "hr"},
                {
                    "tag": "div",
                    "text": {
                        "tag": "lark_md",
                        "content": "**📊 市场行情**\n" + "\n".join(market_text) if market_text else "**📊 市场行情**\n• 暂无数据"
                    }
                },
                {"tag": "hr"},
                {
                    "tag": "action",
                    "actions": [
                        {
                            "tag": "button",
                            "text": {"tag": "plain_text", "content": "📊 查看详情"},
                            "type": "primary",
                            "value": {"action": "view_details", "day": day_count}
                        },
                        {
                            "tag": "button",
                            "text": {"tag": "plain_text", "content": "⏸️ 暂停策略"},
                            "type": "default",
                            "value": {"action": "pause_strategy"}
                        },
                        {
                            "tag": "button",
                            "text": {"tag": "plain_text", "content": "🚨 紧急平仓"},
                            "type": "danger",
                            "value": {"action": "emergency_close"}
                        }
                    ]
                },
                {
                    "tag": "note",
                    "elements": [
                        {
                            "tag": "plain_text",
                            "content": "🤖 Agent v1.4.6 | 点击按钮执行操作"
                        }
                    ]
                }
            ]
        }
        
        return self._send_card(card)


def load_data():
    """加载Agent数据"""
    import sys
    sys.path.insert(0, '/opt/hktech-agent/active_src')
    
    data_dir = "/opt/hktech-agent/data"
    
    data = {
        "portfolio": {},
        "day_count": {"count": 1},
        "market_data": {}
    }
    
    # 1. 加载组合数据
    try:
        with open(f"{data_dir}/portfolio.json", 'r') as f:
            data["portfolio"] = json.load(f)
    except: pass
    
    # 2. 加载天数
    try:
        with open(f"{data_dir}/day_count.json", 'r') as f:
            data["day_count"] = json.load(f)
    except: pass
    
    # 3. 获取实时市场数据（优先从API获取）
    try:
        print("   🌐 从实时数据源获取市场数据...")
        from data_collector import HKStockDataCollector
        
        collector = HKStockDataCollector()
        raw_data = collector.get_daily_data(days=5)
        
        # 转换为卡片需要的格式
        market_data = {}
        for code, info in raw_data.items():
            market_data[code] = {
                "price": info.get("price", 0),
                "change_pct": info.get("change_pct", 0),
                "ma5": info.get("ma5", info.get("price", 0)),
                "ma20": info.get("ma20", info.get("price", 0)),
                "rsi": info.get("rsi", 50),
                "volume": info.get("volume", 0),
                "data_source": info.get("data_source", "unknown")
            }
        
        data["market_data"] = market_data
        print(f"   ✅ 成功获取 {len(market_data)} 只股票实时数据")
        
    except Exception as e:
        print(f"   ⚠️ 实时数据获取失败: {e}")
        print("   🔄 尝试从本地文件加载...")
        
        # 回退：尝试今天的文件
        try:
            today = datetime.now().strftime('%Y%m%d')
            with open(f"{data_dir}/market_data_{today}.json", 'r') as f:
                data["market_data"] = json.load(f)
                print(f"   ✅ 使用今日本地数据")
        except:
            # 最后尝试昨天的文件
            try:
                yesterday = (datetime.now() - __import__('datetime').timedelta(days=1)).strftime('%Y%m%d')
                with open(f"{data_dir}/market_data_{yesterday}.json", 'r') as f:
                    data["market_data"] = json.load(f)
                    print(f"   ⚠️ 使用昨日数据（可能过时）")
            except:
                print("   ❌ 无法获取任何市场数据！")
    
    return data


def main():
    """主函数"""
    print(f"📊 生成交互式日报: {datetime.now().isoformat()}")
    
    # 加载数据
    data = load_data()
    portfolio = data.get("portfolio", {})
    day_count = data.get("day_count", {}).get("count", 1)
    market_data = data.get("market_data", {})
    
    # 发送交互式卡片
    sender = FeishuCardSender()
    success = sender.send_daily_report_card(day_count, portfolio, market_data)
    
    if success:
        print(f"✅ 交互式日报推送完成")
    else:
        print(f"⚠️ 推送失败，请检查配置")
        # 备用：记录到日志
        log_file = "/opt/hktech-agent/logs/daily_report.log"
        with open(log_file, 'a', encoding='utf-8') as f:
            f.write(f"\n{'='*50}\n")
            f.write(f"时间: {datetime.now().isoformat()}\n")
            f.write("交互式卡片发送失败\n")
            f.write(f"{'='*50}\n")


if __name__ == "__main__":
    main()
