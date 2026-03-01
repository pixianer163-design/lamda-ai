#!/usr/bin/env python3
"""
恒生科技自主Agent - 每日运行入口
"""

import os
import sys
import json
from datetime import datetime
from typing import Dict

# 确保能导入src模块
sys.path.insert(0, '/opt/hktech-agent')

from src.data_collector import HKStockDataCollector
from src.llm_client import DeepSeekClient
from src.feishu_notifier import FeishuNotifier


class HKTechAgent:
    """恒生科技交易Agent主类"""
    
    def __init__(self):
        """初始化Agent"""
        self.data_collector = HKStockDataCollector()
        self.llm_client = DeepSeekClient()
        self.notifier = FeishuNotifier()  # 飞书通知
        self.portfolio = self._load_portfolio()
        
    def _load_portfolio(self) -> Dict:
        """加载投资组合状态"""
        portfolio_file = "/opt/hktech-agent/data/portfolio.json"
        if os.path.exists(portfolio_file):
            with open(portfolio_file, "r") as f:
                return json.load(f)
        
        # 初始状态
        return {
            "cash": 100000,
            "total_value": 100000,
            "holdings": {},
            "history": [],
            "updated_at": datetime.now().isoformat()
        }
    
    def _save_portfolio(self):
        """保存投资组合状态"""
        portfolio_file = "/opt/hktech-agent/data/portfolio.json"
        self.portfolio["updated_at"] = datetime.now().isoformat()
        with open(portfolio_file, "w") as f:
            json.dump(self.portfolio, f, indent=2, default=str)
    
    def run_daily_analysis(self):
        """运行每日分析"""
        print("="*60)
        print(f"🤖 恒生科技自主Agent - {datetime.now().strftime('%Y-%m-%d %H:%M')}")
        print("="*60)
        
        # Step 1: 数据采集
        print("\n📊 Step 1: 采集市场数据...")
        market_data = self.data_collector.get_daily_data(days=30)
        
        if not market_data:
            print("❌ 数据获取失败，今日跳过")
            return
        
        # Step 2: LLM分析
        print("\n🧠 Step 2: LLM深度分析...")
        analysis_result = self.llm_client.analyze_market(market_data)
        
        # Step 3: 生成交易建议
        print("\n📋 Step 3: 生成交易建议...")
        recommendations = self._generate_recommendations(market_data, analysis_result)
        
        # Step 4: 风控检查
        print("\n🛡️  Step 4: 风控检查...")
        valid_trades = self._risk_check(recommendations)
        
        # Step 5: 模拟执行
        print("\n⚡ Step 5: 模拟交易执行...")
        executed_trades = self._simulate_execution(valid_trades, market_data)
        
        # Step 6: 更新持仓
        print("\n💰 Step 6: 更新投资组合...")
        self._update_portfolio(executed_trades, market_data)
        
        # Step 7: 生成报告
        print("\n📄 Step 7: 生成日报...")
        report = self._generate_report(market_data, analysis_result, executed_trades)
        
        # 保存报告
        self._save_report(report)
        
        # Step 8: 飞书通知
        print("\n📱 Step 8: 发送飞书通知...")
        self.notifier.send_market_update(market_data, executed_trades)
        
        print("\n" + "="*60)
        print("✅ 今日任务完成！")
        print(f"📁 报告保存于: /opt/hktech-agent/data/report_{datetime.now().strftime('%Y%m%d')}.txt")
        print("="*60)
        
        return report
    
    def _generate_recommendations(self, market_data: Dict, analysis: Dict) -> list:
        """基于分析和数据生成具体建议"""
        recommendations = []
        
        # 解析LLM分析结果
        llm_analysis = analysis.get("analysis", {})
        
        # 如果analysis是字符串（LLM原始输出），解析它
        if isinstance(llm_analysis, str):
            try:
                import json
                llm_analysis = json.loads(llm_analysis)
            except:
                llm_analysis = {}
        
        for code, stock_data in market_data.items():
            rec = {
                "code": code,
                "name": stock_data["name"],
                "current_price": stock_data["price"],
                "action": "HOLD",
                "confidence": 50
            }
            
            # 使用LLM建议
            if code in llm_analysis and isinstance(llm_analysis[code], dict):
                llm_rec = llm_analysis[code]
                rec["action"] = llm_rec.get("signal", "HOLD")
                rec["confidence"] = llm_rec.get("confidence", 50)
                rec["reason"] = llm_rec.get("reason", "")
                rec["target_price"] = llm_rec.get("target_price", stock_data["price"] * 1.1)
                rec["stop_loss"] = llm_rec.get("stop_loss", stock_data["price"] * 0.95)
            else:
                # 使用技术指标作为备选
                if stock_data.get("trend") == "UP" and stock_data.get("rsi", 50) < 70:
                    rec["action"] = "BUY"
                    rec["confidence"] = 60
                    rec["reason"] = "技术指标显示上升趋势"
                elif stock_data.get("trend") == "DOWN" and stock_data.get("rsi", 50) > 30:
                    rec["action"] = "SELL"
                    rec["confidence"] = 60
                    rec["reason"] = "技术指标显示下降趋势"
                else:
                    rec["reason"] = "信号不明确，建议观望"
            
            recommendations.append(rec)
        
        return recommendations
    
    def _risk_check(self, recommendations: list) -> list:
        """风控检查"""
        valid_trades = []
        
        for rec in recommendations:
            # 检查1: 置信度
            if rec["confidence"] < 50:
                print(f"  ⚠️ {rec['name']}: 置信度{rec['confidence']}%过低，转为HOLD")
                rec["action"] = "HOLD"
            
            # 检查2: 持仓数量
            if rec["action"] == "BUY" and len(self.portfolio["holdings"]) >= 3:
                if rec["code"] not in self.portfolio["holdings"]:
                    print(f"  ⚠️ {rec['name']}: 已达最大持仓数，无法新建仓位")
                    rec["action"] = "HOLD"
            
            # 检查3: 现金储备
            if rec["action"] == "BUY" and self.portfolio["cash"] < 20000:
                print(f"  ⚠️ {rec['name']}: 现金储备不足")
                rec["action"] = "HOLD"
            
            valid_trades.append(rec)
        
        return valid_trades
    
    def _simulate_execution(self, trades: list, market_data: Dict) -> list:
        """模拟交易执行"""
        executed = []
        trade_count = 0
        max_trades = 1  # 每日最多1笔交易（避免首日建仓过猛）
        
        for trade in trades:
            if trade_count >= max_trades:
                print(f"  ⏸️  {trade['name']}: 已达今日交易上限")
                continue
            if trade["action"] == "HOLD":
                continue
            
            price = trade["current_price"]
            
            if trade["action"] == "BUY":
                # 计算买入数量（固定金额策略）
                invest_amount = min(25000, self.portfolio["cash"] * 0.25)
                shares = int(invest_amount / price)
                cost = shares * price
                
                if cost > 0 and cost <= self.portfolio["cash"]:
                    trade["shares"] = shares
                    trade["cost"] = round(cost, 2)
                    trade["executed_price"] = price
                    print(f"  ✅ 买入 {trade['name']}: {shares}股 × {price} = {cost:.2f}")
                    executed.append(trade)
                else:
                    print(f"  ❌ 买入 {trade['name']}: 资金不足")
                    
            elif trade["action"] == "SELL":
                # 卖出当前持仓（支持部分卖出）
                if trade["code"] in self.portfolio["holdings"]:
                    holding = self.portfolio["holdings"][trade["code"]]
                    existing_shares = holding.get("shares", 0)
                    # 卖出50%
                    sell_shares = int(existing_shares * 0.5)
                    if sell_shares == 0:
                        sell_shares = existing_shares
                    
                    if sell_shares > 0:
                        revenue = sell_shares * price
                        fee = revenue * 0.0015
                        net_revenue = revenue - fee
                        
                        remaining_shares = existing_shares - sell_shares
                        
                        if remaining_shares > 0:
                            # 部分卖出，更新持仓
                            remaining_cost = holding["cost_basis"] * (remaining_shares / existing_shares)
                            self.portfolio["holdings"][trade["code"]]["shares"] = remaining_shares
                            self.portfolio["holdings"][trade["code"]]["cost_basis"] = remaining_cost
                        else:
                            # 全部卖出，删除持仓
                            del self.portfolio["holdings"][trade["code"]]
                        
                        self.portfolio["cash"] += net_revenue
                        trade["shares"] = sell_shares
                        trade["revenue"] = round(net_revenue, 2)
                        trade["fee"] = round(fee, 2)
                        trade["executed_price"] = price
                        print(f"  ✅ 卖出 {trade['name']}: {sell_shares}股 × {price} = {revenue:.2f} (手续费:{fee:.2f})")
                        executed.append(trade)
        
        return executed
    
    def _update_portfolio(self, trades: list, market_data: Dict):
        """更新投资组合"""
        for trade in trades:
            if trade["action"] == "BUY":
                # 买入
                self.portfolio["cash"] -= trade["cost"]
                self.portfolio["holdings"][trade["code"]] = {
                    "name": trade["name"],
                    "shares": trade["shares"],
                    "cost_price": trade["executed_price"],
                    "cost_basis": trade["cost"],
                    "buy_date": datetime.now().isoformat()
                }
                
            elif trade["action"] == "SELL":
                # 卖出
                self.portfolio["cash"] += trade["revenue"]
                if trade["code"] in self.portfolio["holdings"]:
                    del self.portfolio["holdings"][trade["code"]]
        
        # 计算当前总市值
        total_value = self.portfolio["cash"]
        for code, holding in self.portfolio["holdings"].items():
            if code in market_data:
                current_value = holding["shares"] * market_data[code]["price"]
                total_value += current_value
        
        self.portfolio["total_value"] = round(total_value, 2)
        
        # 记录历史
        self.portfolio["history"].append({
            "date": datetime.now().isoformat(),
            "cash": self.portfolio["cash"],
            "total_value": self.portfolio["total_value"],
            "trades": len(trades)
        })
        
        self._save_portfolio()
        
        print(f"\n  💼 组合更新:")
        print(f"     现金: {self.portfolio['cash']:.2f}")
        print(f"     总市值: {self.portfolio['total_value']:.2f}")
        print(f"     持仓: {len(self.portfolio['holdings'])}只股票")
    
    def _generate_report(self, market_data, analysis, trades) -> str:
        """生成日报"""
        report = f"""
{'='*60}
恒生科技Agent日报 - {datetime.now().strftime('%Y年%m月%d日')}
{'='*60}

📊 市场概况:
"""
        for code, data in market_data.items():
            report += f"  {data['name']}({code}): {data['price']} ({data['change_pct']:+.2f}%)\n"
        
        report += f"\n🧠 LLM分析:\n"
        if "note" in analysis:
            report += f"  {analysis['note']}\n"
        
        report += f"\n⚡ 今日交易:\n"
        if trades:
            for trade in trades:
                action = trade['action']
                if action == "BUY":
                    report += f"  买入 {trade['name']}: {trade['shares']}股 @ {trade['executed_price']} = {trade['cost']:.2f}\n"
                else:
                    report += f"  卖出 {trade['name']}: {trade['shares']}股 @ {trade['executed_price']} = {trade['revenue']:.2f}\n"
        else:
            report += "  无交易\n"
        
        report += f"\n💰 持仓状态:\n"
        report += f"  现金: {self.portfolio['cash']:.2f}\n"
        report += f"  总市值: {self.portfolio['total_value']:.2f}\n"
        
        if self.portfolio['holdings']:
            report += "  持仓明细:\n"
            for code, holding in self.portfolio['holdings'].items():
                current_price = market_data.get(code, {}).get('price', holding['cost_price'])
                current_value = holding['shares'] * current_price
                profit = current_value - holding['cost_basis']
                profit_pct = (profit / holding['cost_basis']) * 100
                report += f"    {holding['name']}: {holding['shares']}股 成本{holding['cost_price']} 现价{current_price} 盈亏{profit_pct:+.2f}%\n"
        
        report += f"\n{'='*60}\n"
        
        print(report)
        return report
    
    def _save_report(self, report: str):
        """保存报告"""
        report_file = f"/opt/hktech-agent/data/report_{datetime.now().strftime('%Y%m%d')}.txt"
        with open(report_file, "w", encoding="utf-8") as f:
            f.write(report)


def main():
    """主入口"""
    agent = HKTechAgent()
    agent.run_daily_analysis()


if __name__ == "__main__":
    main()
