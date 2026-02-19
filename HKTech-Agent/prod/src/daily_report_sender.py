#!/usr/bin/env python3
"""
恒生科技Agent - 日报推送脚本（交互式卡片版）
支持飞书交互式卡片，带按钮可操作
本地适配版本：使用环境变量和相对路径
"""

import json
import os
import sys
import requests
from datetime import datetime, timedelta
from typing import Optional

# 使用环境变量配置路径
PROD_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
HK_AGENT_DIR = os.path.dirname(PROD_DIR)
ACTIVE_SRC_DIR = os.path.join(HK_AGENT_DIR, "active_src")
LOCAL_CONFIG_DIR = os.path.join(HK_AGENT_DIR, "local_config")
DEFAULT_DATA_DIR = os.path.join(HK_AGENT_DIR, "data")
DEFAULT_LOG_DIR = os.path.join(PROD_DIR, "logs")

# 添加路径到sys.path
if ACTIVE_SRC_DIR not in sys.path:
    sys.path.insert(0, ACTIVE_SRC_DIR)
if os.path.dirname(__file__) not in sys.path:
    sys.path.insert(0, os.path.dirname(__file__))

class FeishuCardSender:
    """飞书交互式卡片发送器"""
    
    def __init__(self, config_path: Optional[str] = None):
        """初始化，加载配置"""
        if config_path is None:
            # 尝试多个可能的配置路径
            possible_paths = [
                os.path.join(LOCAL_CONFIG_DIR, "feishu_config.json"),
                os.path.join(HK_AGENT_DIR, "config", "feishu_config.json"),
            ]
            
            for path in possible_paths:
                if os.path.exists(path):
                    config_path = path
                    print(f"📁 使用配置文件: {path}")
                    break
        
        if config_path and os.path.exists(config_path):
            try:
                with open(config_path, 'r') as f:
                    self.config = json.load(f)
                print(f"✅ 配置文件加载成功")
            except Exception as e:
                print(f"❌ 配置文件加载失败: {e}")
                self.config = {}
        else:
            print("⚠️ 未找到配置文件，使用模拟模式")
            self.config = {}
        
        self.app_id = self.config.get('app_id')
        self.app_secret = self.config.get('app_secret')
        self.chat_id = self.config.get('chat_id')
        self.token = None
        
        # 股票名称映射
        self.stock_names = {
            "00700": "腾讯控股",
            "09988": "阿里巴巴", 
            "03690": "美团-W",
            "01810": "小米集团-W",
            "09618": "京东集团-SW",
            "09999": "网易-S",
            "09888": "百度集团-SW",
            "09923": "哔哩哔哩-W",
            "02020": "安踏体育",
            "02269": "药明生物"
        }
    
    def _get_token(self) -> Optional[str]:
        """获取access_token"""
        if not self.app_id or not self.app_secret:
            print("⚠️ 未配置 app_id 或 app_secret，跳过飞书推送")
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
                print(f"✅ 获取飞书token成功")
                return self.token
            else:
                print(f"❌ 获取token失败: {result.get('msg')}")
        except Exception as e:
            print(f"❌ 获取token失败: {e}")
        return None
    
    def _send_card(self, card_content: dict) -> bool:
        """发送卡片消息"""
        if not self.token:
            self.token = self._get_token()
        
        if not self.token or not self.chat_id:
            print("❌ 未配置token或chat_id，无法发送飞书卡片")
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
    # 使用环境变量或默认路径
    data_dir = os.environ.get("AGENT_DATA_DIR", DEFAULT_DATA_DIR)
    
    data = {
        "portfolio": {},
        "day_count": {"count": 1},
        "market_data": {}
    }
    
    # 1. 加载组合数据
    portfolio_path = os.path.join(data_dir, "portfolio.json")
    try:
        with open(portfolio_path, 'r') as f:
            data["portfolio"] = json.load(f)
        print(f"   ✅ 组合数据加载成功: {portfolio_path}")
    except Exception as e:
        print(f"   ⚠️ 组合数据加载失败: {e}")
    
    # 2. 加载天数
    day_count_path = os.path.join(data_dir, "day_count.json")
    try:
        with open(day_count_path, 'r') as f:
            data["day_count"] = json.load(f)
        print(f"   ✅ 天数数据加载成功: {day_count_path}")
    except Exception as e:
        print(f"   ⚠️ 天数数据加载失败: {e}")
    
    # 3. 获取实时市场数据（优先从API获取）
    try:
        print("   🌐 从实时数据源获取市场数据...")
        # 尝试导入 data_collector
        try:
            from data_collector import HKStockDataCollector
            
            collector = HKStockDataCollector(data_dir=data_dir)
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
            
        except ImportError as e:
            print(f"   ⚠️ 无法导入 data_collector: {e}")
            print("   🔄 尝试从本地文件加载...")
            raise ImportError("data_collector not available")
            
    except Exception as e:
        print(f"   ⚠️ 实时数据获取失败: {e}")
        print("   🔄 尝试从本地文件加载...")
        
        # 回退：尝试今天的文件
        today = datetime.now().strftime('%Y%m%d')
        today_path = os.path.join(data_dir, f"market_data_{today}.json")
        try:
            with open(today_path, 'r') as f:
                data["market_data"] = json.load(f)
                print(f"   ✅ 使用今日本地数据: {today_path}")
        except:
            # 最后尝试昨天的文件
            try:
                yesterday = (datetime.now() - timedelta(days=1)).strftime('%Y%m%d')
                yesterday_path = os.path.join(data_dir, f"market_data_{yesterday}.json")
                with open(yesterday_path, 'r') as f:
                    data["market_data"] = json.load(f)
                    print(f"   ⚠️ 使用昨日数据（可能过时）: {yesterday_path}")
            except:
                print("   ❌ 无法获取任何市场数据！使用模拟数据")
                # 创建模拟数据
                stock_names = {
                    "00700": "腾讯控股",
                    "09988": "阿里巴巴", 
                    "03690": "美团-W"
                }
                import random
                mock_data = {}
                for code, name in stock_names.items():
                    mock_data[code] = {
                        "price": round(random.uniform(100, 500), 2),
                        "change_pct": round(random.uniform(-5, 5), 2),
                        "ma5": round(random.uniform(100, 500), 2),
                        "ma20": round(random.uniform(100, 500), 2),
                        "rsi": round(random.uniform(30, 70), 1),
                        "volume": random.randint(1000000, 50000000),
                        "data_source": "mock"
                    }
                data["market_data"] = mock_data
                print(f"   🎭 生成 {len(mock_data)} 只股票模拟数据")
    
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
        print(f"⚠️ 推送失败，飞书功能未启用或配置有误")
        # 备用：记录到日志
        log_dir = os.environ.get("AGENT_LOG_DIR", DEFAULT_LOG_DIR)
        os.makedirs(log_dir, exist_ok=True)
        log_file = os.path.join(log_dir, "daily_report.log")
        try:
            with open(log_file, 'a', encoding='utf-8') as f:
                f.write(f"\n{'='*50}\n")
                f.write(f"时间: {datetime.now().isoformat()}\n")
                f.write(f"组合价值: {portfolio.get('cash', 0)}\n")
                f.write(f"持仓数量: {len(portfolio.get('holdings', {}))}\n")
                f.write(f"股票数量: {len(market_data)}\n")
                f.write(f"飞书推送: 失败\n")
                f.write(f"{'='*50}\n")
            print(f"📝 日志已保存: {log_file}")
        except Exception as e:
            print(f"❌ 日志保存失败: {e}")


if __name__ == "__main__":
    main()