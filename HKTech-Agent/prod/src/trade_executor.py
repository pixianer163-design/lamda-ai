#!/usr/bin/env python3
"""
模拟交易执行器

功能：
1. 接收 AI 决策信号（BUY/SELL/HOLD）
2. 执行模拟交易（计算滑点、手续费）
3. 更新持仓和现金
4. 记录交易历史
"""

import json
import os
from datetime import datetime
from typing import Dict, List, Optional, Tuple
import sys

# 添加路径
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))


class TradeExecutor:
    """模拟交易执行器"""
    
    def __init__(self, data_dir: str = "/opt/hktech-agent/data"):
        """初始化交易执行器"""
        self.data_dir = data_dir
        self.paper_account_file = os.path.join(data_dir, "paper_account.json")
        self.portfolio_file = os.path.join(data_dir, "portfolio.json")
        
        # 交易参数
        self.initial_cash = 1000000  # 初始资金 100 万
        self.commission_rate = 0.0003  # 佣金 万分之三
        self.min_commission = 5  # 最低佣金 5 元
        self.slippage_rate = 0.002  # 滑点 0.2%
        
        # 加载账户
        self.account = self._load_account()
        
    def _load_account(self) -> Dict:
        """加载模拟账户"""
        if os.path.exists(self.paper_account_file):
            try:
                with open(self.paper_account_file, 'r', encoding='utf-8') as f:
                    return json.load(f)
            except:
                pass
        
        # 创建新账户
        return {
            "cash": self.initial_cash,
            "positions": {},
            "trade_history": [],
            "updated_at": datetime.now().isoformat()
        }
    
    def _save_account(self):
        """保存模拟账户"""
        self.account["updated_at"] = datetime.now().isoformat()
        
        with open(self.paper_account_file, 'w', encoding='utf-8') as f:
            json.dump(self.account, f, indent=2, ensure_ascii=False)
    
    def execute_decision(self, code: str, action: str, confidence: float, 
                        current_price: float, market_data: Dict = None) -> Optional[Dict]:
        """
        执行交易决策
        
        Args:
            code: 股票代码
            action: BUY/SELL/HOLD
            confidence: 置信度
            current_price: 当前价格
            market_data: 市场数据（可选）
            
        Returns:
            交易记录，如果未执行则返回 None
        """
        if action == "hold" or confidence < 0.6:
            # 置信度低于 60% 不执行
            return None
        
        # 获取持仓信息
        position = self.account["positions"].get(code, {})
        shares_held = position.get("shares", 0)
        
        if action == "buy":
            return self._execute_buy(code, current_price, confidence)
        elif action == "sell":
            if shares_held > 0:
                return self._execute_sell(code, current_price, confidence)
            else:
                # 没有持仓无法卖出
                return None
        
        return None
    
    def _execute_buy(self, code: str, current_price: float, confidence: float) -> Dict:
        """执行买入交易"""
        # 计算买入金额（使用 10% 仓位）
        available_cash = self.account["cash"]
        target_amount = available_cash * 0.1  # 单次买入 10% 现金
        
        # 计算滑点
        slippage = current_price * self.slippage_rate
        executed_price = current_price + slippage
        
        # 计算股数（向下取整到 100 股的倍数，港股通常 100 股一手）
        shares = int(target_amount / executed_price / 100) * 100
        if shares < 100:
            shares = 100  # 至少买 1 手
        
        # 计算总金额和手续费
        amount = shares * executed_price
        commission = max(amount * self.commission_rate, self.min_commission)
        total_cost = amount + commission
        
        # 检查现金是否足够
        if total_cost > available_cash:
            # 现金不足，减少股数
            shares = int((available_cash - commission) / executed_price / 100) * 100
            if shares < 100:
                print(f"   ⚠️ 现金不足，跳过买入 {code}")
                return None
            amount = shares * executed_price
            commission = max(amount * self.commission_rate, self.min_commission)
            total_cost = amount + commission
        
        # 更新账户
        self.account["cash"] -= total_cost
        
        # 更新持仓
        if code not in self.account["positions"]:
            self.account["positions"][code] = {
                "code": code,
                "shares": 0,
                "avg_cost": 0,
                "total_cost": 0,
                "opened_at": datetime.now().isoformat(),
                "unrealized_pnl": 0,
                "realized_pnl": 0
            }
        
        pos = self.account["positions"][code]
        old_shares = pos["shares"]
        old_cost = pos["total_cost"]
        
        pos["shares"] += shares
        pos["total_cost"] += (amount + commission)
        pos["avg_cost"] = pos["total_cost"] / pos["shares"]
        
        # 记录交易
        trade = {
            "timestamp": datetime.now().isoformat(),
            "code": code,
            "action": "BUY",
            "shares": shares,
            "requested_price": current_price,
            "executed_price": round(executed_price, 2),
            "slippage": round(slippage / current_price, 4),
            "commission": round(commission, 2),
            "amount": round(amount, 2),
            "total_cost": round(total_cost, 2),
            "confidence": confidence,
            "status": "FILLED"
        }
        
        self.account["trade_history"].append(trade)
        self._save_account()
        
        return trade
    
    def _execute_sell(self, code: str, current_price: float, confidence: float) -> Dict:
        """执行卖出交易"""
        position = self.account["positions"].get(code, {})
        shares_held = position.get("shares", 0)
        
        if shares_held <= 0:
            return None
        
        # 全部卖出（简化策略）
        shares = shares_held
        
        # 计算滑点
        slippage = current_price * self.slippage_rate
        executed_price = current_price - slippage
        
        # 计算金额和手续费
        amount = shares * executed_price
        commission = max(amount * self.commission_rate, self.min_commission)
        net_proceeds = amount - commission
        
        # 计算盈亏
        avg_cost = position.get("avg_cost", 0)
        cost_basis = shares * avg_cost
        realized_pnl = net_proceeds - cost_basis
        
        # 更新账户
        self.account["cash"] += net_proceeds
        
        # 更新持仓
        position["realized_pnl"] = position.get("realized_pnl", 0) + realized_pnl
        position["shares"] = 0
        position["avg_cost"] = 0
        position["total_cost"] = 0
        
        # 记录交易
        trade = {
            "timestamp": datetime.now().isoformat(),
            "code": code,
            "action": "SELL",
            "shares": shares,
            "requested_price": current_price,
            "executed_price": round(executed_price, 2),
            "slippage": round(slippage / current_price, 4),
            "commission": round(commission, 2),
            "amount": round(amount, 2),
            "net_proceeds": round(net_proceeds, 2),
            "realized_pnl": round(realized_pnl, 2),
            "confidence": confidence,
            "status": "FILLED"
        }
        
        self.account["trade_history"].append(trade)
        self._save_account()
        
        return trade
    
    def get_account_summary(self) -> Dict:
        """获取账户摘要"""
        total_market_value = 0
        total_cost = 0
        
        for code, pos in self.account["positions"].items():
            if pos["shares"] > 0:
                total_market_value += pos["shares"] * pos.get("current_price", pos["avg_cost"])
                total_cost += pos["total_cost"]
        
        total_value = self.account["cash"] + total_market_value
        total_pnl = total_value - self.initial_cash
        total_pnl_pct = (total_pnl / self.initial_cash) * 100 if self.initial_cash > 0 else 0
        
        return {
            "cash": round(self.account["cash"], 2),
            "market_value": round(total_market_value, 2),
            "total_value": round(total_value, 2),
            "total_pnl": round(total_pnl, 2),
            "total_pnl_pct": round(total_pnl_pct, 2),
            "positions_count": sum(1 for p in self.account["positions"].values() if p["shares"] > 0),
            "trades_count": len(self.account["trade_history"])
        }
    
    def execute_all_decisions(self, decisions: Dict[str, Dict], market_data: Dict) -> List[Dict]:
        """
        批量执行所有决策
        
        Args:
            decisions: 决策字典 {code: {action, confidence, reason}}
            market_data: 市场数据 {code: {price, ...}}
            
        Returns:
            执行的交易列表
        """
        executed_trades = []
        
        print("\n" + "="*60)
        print("📈 开始执行交易决策")
        print("="*60)
        
        for code, decision in decisions.items():
            action = decision.get("action", "hold")
            confidence = decision.get("confidence", 0)
            price = market_data.get(code, {}).get("price", 0)
            
            if not price:
                print(f"   ⚠️ {code}: 无价格数据，跳过")
                continue
            
            print(f"\n   {code}: {action.upper()} (置信度：{confidence*100:.0f}%, 价格：{price})")
            
            trade = self.execute_decision(code, action, confidence, price, market_data)
            
            if trade:
                executed_trades.append(trade)
                if trade["action"] == "BUY":
                    print(f"      ✅ 买入 {trade['shares']}股 @ {trade['executed_price']:.2f} = {trade['total_cost']:.2f}元")
                else:
                    print(f"      ✅ 卖出 {trade['shares']}股 @ {trade['executed_price']:.2f} = {trade['net_proceeds']:.2f}元 (盈亏：{trade['realized_pnl']:.2f})")
            else:
                print(f"      ⚪ 未执行")
        
        # 打印账户摘要
        summary = self.get_account_summary()
        print("\n" + "="*60)
        print("📊 账户摘要")
        print("="*60)
        print(f"   现金：{summary['cash']:,.2f} 元")
        print(f"   市值：{summary['market_value']:,.2f} 元")
        print(f"   总值：{summary['total_value']:,.2f} 元")
        print(f"   盈亏：{summary['total_pnl']:,.2f} 元 ({summary['total_pnl_pct']:+.2f}%)")
        print(f"   持仓：{summary['positions_count']} 只")
        print(f"   交易：{summary['trades_count']} 笔")
        print("="*60 + "\n")
        
        return executed_trades


def main():
    """主函数 - 执行交易并保存记录"""
    import sys
    sys.path.insert(0, '/opt/hktech-agent/HKTech-Agent/prod/src')
    
    # 加载今日决策和市场数据
    today = datetime.now().strftime("%Y%m%d")
    decisions_file = f'/opt/hktech-agent/data/decisions_{today}.json'
    market_file = '/opt/hktech-agent/data/market_data_latest.json'
    
    if not os.path.exists(decisions_file):
        print(f"⚠️ 决策文件不存在：{decisions_file}")
        return
    
    with open(decisions_file, 'r') as f:
        decisions_data = json.load(f)
    
    with open(market_file, 'r') as f:
        market_data = json.load(f)
    
    decisions = decisions_data.get('decisions', {})
    
    print("="*60)
    print(f"📈 执行今日交易决策 ({today})")
    print("="*60)
    
    # 执行交易
    executor = TradeExecutor()
    trades = executor.execute_all_decisions(decisions, market_data)
    
    # 保存交易记录
    if trades:
        trade_file = f'/opt/hktech-agent/data/trades_{today}.json'
        with open(trade_file, 'w') as f:
            json.dump(trades, f, indent=2, ensure_ascii=False)
        print(f"✅ 交易记录已保存：{trade_file}")
    
    print(f"\n✅ 今日共执行 {len(trades)} 笔交易")


if __name__ == "__main__":
    main()
