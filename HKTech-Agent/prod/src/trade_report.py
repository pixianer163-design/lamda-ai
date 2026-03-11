#!/usr/bin/env python3
"""
交易报告推送

推送今日交易执行结果到飞书群
"""

import json
import os
import requests
from datetime import datetime
from typing import Dict, List

class TradeReporter:
    """交易报告推送器"""
    
    def __init__(self):
        self.data_dir = "/opt/hktech-agent/data"
        self.webhook_url = "https://open.feishu.cn/open-apis/bot/v2/hook/7a7dbe38-9181-4311-8094-ebaf6cf0f378"
        self.stock_names = {
            "00700": "腾讯控股",
            "09988": "阿里巴巴",
            "03690": "美团-W",
            "01810": "小米集团",
            "01024": "快手-W",
            "09618": "京东集团"
        }
    
    def load_today_trades(self) -> List[Dict]:
        """加载今日交易"""
        today = datetime.now().strftime("%Y%m%d")
        trade_file = os.path.join(self.data_dir, f"trades_{today}.json")
        
        if os.path.exists(trade_file):
            with open(trade_file, 'r', encoding='utf-8') as f:
                return json.load(f)
        
        return []
    
    def load_account(self) -> Dict:
        """加载账户信息"""
        account_file = os.path.join(self.data_dir, "paper_account.json")
        
        if os.path.exists(account_file):
            with open(account_file, 'r', encoding='utf-8') as f:
                return json.load(f)
        
        return {}
    
    def generate_report(self, trades: List[Dict], account: Dict) -> str:
        """生成交易报告"""
        today = datetime.now().strftime("%Y-%m-%d")
        
        # 统计交易
        buy_count = sum(1 for t in trades if t["action"] == "BUY")
        sell_count = sum(1 for t in trades if t["action"] == "SELL")
        total_amount = sum(t.get("total_cost", 0) for t in trades if t["action"] == "BUY")
        total_proceeds = sum(t.get("net_proceeds", 0) for t in trades if t["action"] == "SELL")
        
        # 账户摘要
        cash = account.get("cash", 0)
        positions = account.get("positions", {})
        positions_count = sum(1 for p in positions.values() if p.get("shares", 0) > 0)
        
        # 生成交易明细
        trade_lines = []
        for trade in trades:
            code = trade["code"]
            name = self.stock_names.get(code, code)
            action = "🟢买入" if trade["action"] == "BUY" else "🔴卖出"
            shares = trade["shares"]
            price = trade["executed_price"]
            
            if trade["action"] == "BUY":
                amount = trade.get("total_cost", 0)
                line = f"{action} {name}: {shares}股 @ {price:.2f}元 = {amount:,.0f}元"
            else:
                pnl = trade.get("realized_pnl", 0)
                pnl_symbol = "🟢" if pnl > 0 else "🔴"
                amount = trade.get("net_proceeds", 0)
                line = f"{action} {name}: {shares}股 @ {price:.2f}元 = {amount:,.0f}元 ({pnl_symbol}{pnl:+,.0f}元)"
            
            trade_lines.append(line)
        
        report = f"""📊 恒生 Agent 交易报告

📅 日期：{today}

💼 今日交易
• 买入：{buy_count} 笔
• 卖出：{sell_count} 笔
• 买入金额：{total_amount:,.0f} 元
• 卖出金额：{total_proceeds:,.0f} 元

📈 交易明细
{chr(10).join(trade_lines) if trade_lines else '今日无交易'}

💰 账户概览
• 可用现金：{cash:,.0f} 元
• 持仓数量：{positions_count} 只
• 总交易数：{len(account.get("trade_history", []))} 笔

---
恒生 Agent | 模拟交易
生成时间：{datetime.now().strftime('%H:%M:%S')}"""
        
        return report
    
    def push_report(self) -> bool:
        """推送交易报告"""
        trades = self.load_today_trades()
        account = self.load_account()
        
        if not trades:
            print("ℹ️ 今日无交易，跳过推送")
            return True
        
        report = self.generate_report(trades, account)
        
        print("="*60)
        print("📊 交易报告内容预览：")
        print("="*60)
        print(report)
        print("="*60)
        
        try:
            resp = requests.post(
                self.webhook_url,
                json={"msg_type": "text", "content": {"text": report}},
                timeout=10
            )
            
            result = resp.json()
            success = result.get("StatusCode") == 0 or result.get("code") == 0
            
            if success:
                print("✅ 交易报告推送成功")
                return True
            else:
                print(f"❌ 推送失败：{result}")
                return False
                
        except Exception as e:
            print(f"❌ 推送异常：{e}")
            return False


def main():
    """主函数"""
    reporter = TradeReporter()
    success = reporter.push_report()
    exit(0 if success else 1)


if __name__ == "__main__":
    main()
