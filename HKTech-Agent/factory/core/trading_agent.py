#!/usr/bin/env python3
"""
Agent Factory - TradingAgent（重构版）
基于现有SelfEvolvingAgent重构，支持配置驱动
"""

import os
import sys
import json
import asyncio
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any

sys.path.insert(0, '/opt/hktech-agent')
sys.path.insert(0, '/opt/hktech-agent/factory/core')

from shared_services import get_services


class TradingAgent:
    """
    交易Agent（重构版）
    
    改进：
    1. 配置驱动（而非硬编码）
    2. 共享服务（而非独立实例）
    3. 可克隆（支持A/B测试）
    4. 状态隔离（每个Agent独立状态）
    """
    
    def __init__(self, config: Dict[str, Any]):
        """
        初始化Agent
        
        Args:
            config: 完整配置字典
        """
        self.config = config
        self.agent_id = config["agent_id"]
        self.name = config["name"]
        
        # 从配置中提取
        self.stocks = [s["code"] for s in config["stocks"]]
        self.market = config["market"]
        self.risk_config = config.get("risk_management", {})
        self.strategy_config = config.get("strategy", {})
        self.schedule = config.get("schedule", {})
        
        # 注入共享服务
        services = get_services()
        self.memory = services.get_memory(self.agent_id)
        self.llm = services.get_llm(config.get("llm", {}).get("model", "deepseek-chat"))
        self.world_model = services.get_world_model()
        self.agent_memory = services.get_agent_memory(self.agent_id)
        self.evolution = services.get_evolution_engine(self.agent_id)
        self.data_collector = services.get_data_collector(self.market, self.stocks)
        self.risk_manager = services.get_risk_manager(self.agent_id, self.risk_config)
        self.notifier = services.get_notifier(self.agent_id, config.get("notification"))
        
        # 状态
        self.state = "idle"
        self.portfolio = self._load_portfolio()
        self.day_count = self._load_day_count()
        
        print(f"✅ Agent [{self.agent_id}] 初始化完成: {self.name}")
        print(f"   股票: {', '.join(self.stocks)}")
        print(f"   策略: {self.strategy_config.get('type', 'default')}")
    
    # ============== 生命周期 ==============
    
    async def run_cycle(self):
        """
        每日运行周期
        
        简化版核心流程：
        1. 数据采集
        2. 策略决策
        3. 风控检查
        4. 执行交易
        5. 学习总结
        6. 推送报告
        """
        print(f"\n{'='*70}")
        print(f"🤖 {self.name} - Day {self.day_count} | {datetime.now().strftime('%Y-%m-%d %H:%M')}")
        print(f"{'='*70}")
        
        self.state = "running"
        
        try:
            # 1. 数据采集
            print("\n👁️  Step 1: 数据采集...")
            market_data = self._collect_data()
            
            # 2. 策略决策
            print("\n🎯 Step 2: 策略决策...")
            decisions = self._make_decisions(market_data)
            
            # 3. 风控检查
            print("\n🛡️  Step 3: 风控检查...")
            approved = self._risk_check(decisions, market_data)
            
            # 4. 执行交易
            print("\n⚡ Step 4: 执行交易...")
            executed = self._execute_trades(approved)
            
            # 5. 学习总结
            print("\n📚 Step 5: 学习总结...")
            self._learn(market_data, decisions, executed)
            
            # 6. 策略进化（每周一次）
            if self.day_count % 7 == 0:
                print("\n🧬 Step 6: 策略进化...")
                self._evolve_strategies()
            
            # 7. 推送报告
            print("\n📄 Step 7: 生成报告...")
            await self._send_report(market_data, executed)
            
            # 更新状态
            self.day_count += 1
            self._save_state()
            
            self.state = "idle"
            print(f"\n✅ 周期完成")
            
        except Exception as e:
            self.state = "error"
            print(f"\n❌ 周期出错: {e}")
            raise
    
    def shutdown(self):
        """优雅关闭"""
        self._save_state()
        print(f"✅ Agent [{self.agent_id}] 已关闭")
    
    # ============== 核心方法 ==============
    
    def _collect_data(self) -> Dict:
        """采集市场数据"""
        # 使用共享数据采集器
        return self.data_collector.get_daily_data(days=5)
    
    def _make_decisions(self, market_data: Dict) -> List[Dict]:
        """策略决策（集成世界模型）"""
        decisions = []
        
        # 使用世界模型预测
        print("  🧠 世界模型预测中...")
        predictions = self.world_model.predict(market_data, days_ahead=3)
        scenarios = self.world_model.identify_scenarios(market_data)
        
        if scenarios:
            print(f"  📊 主要情景: {scenarios[0]['name']} ({scenarios[0]['probability']:.0%})")
        
        for code, data in market_data.items():
            if code not in self.stocks:
                continue
            
            # 基于策略类型生成信号
            strategy_type = self.strategy_config.get("type", "default")
            signals = self._generate_signals(data, strategy_type)
            
            # 添加世界模型信号
            if code in predictions:
                pred = predictions[code]
                if pred['direction_prob'] > 0.6:
                    signals.append(('world_model', 'BUY', pred['confidence'] * 0.8))
                    print(f"    🎯 {code}: 世界模型建议买入 (置信度{pred['confidence']:.0%})")
                elif pred['direction_prob'] < 0.4:
                    signals.append(('world_model', 'SELL', pred['confidence'] * 0.8))
                    print(f"    🎯 {code}: 世界模型建议卖出 (置信度{pred['confidence']:.0%})")
            
            # 投票决策
            decision = self._vote(signals, code, data, predictions.get(code))
            decisions.append(decision)
        
        return decisions
    
    def _generate_signals(self, data: Dict, strategy_type: str) -> List[tuple]:
        """生成交易信号"""
        signals = []
        price = data.get('price', 0)
        ma5 = data.get('ma5', price)
        ma20 = data.get('ma20', price)
        rsi = data.get('rsi', 50)
        change = data.get('change_pct', 0)
        
        factors = self.strategy_config.get("factors", {})
        
        # 技术分析
        if factors.get("technical", 0) > 0.2:
            if ma5 > ma20 * 1.02:
                signals.append(('technical', 'BUY', 0.6))
            elif ma5 < ma20 * 0.98:
                signals.append(('technical', 'SELL', 0.6))
            
            if rsi < 30:
                signals.append(('rsi', 'BUY', 0.5))
            elif rsi > 70:
                signals.append(('rsi', 'SELL', 0.5))
        
        # 动量策略
        if factors.get("sentiment", 0) > 0.2:
            if change > 2:
                signals.append(('momentum', 'BUY', 0.4))
            elif change < -2:
                signals.append(('momentum', 'SELL', 0.4))
        
        return signals
    
    def _vote(self, signals: List[tuple], code: str, data: Dict, prediction: Dict = None) -> Dict:
        """策略投票（包含世界模型预测）"""
        if not signals:
            return {
                "code": code,
                "name": data.get('name', code),
                "action": "HOLD",
                "confidence": 0.5,
                "reason": "无明确信号",
                "prediction": prediction
            }
        
        buy_strength = sum(s[2] for s in signals if s[1] == 'BUY')
        sell_strength = sum(s[2] for s in signals if s[1] == 'SELL')
        
        # 检查是否有世界模型信号
        wm_signal = next((s for s in signals if s[0] == 'world_model'), None)
        
        if buy_strength > sell_strength + 0.2:
            action = "BUY"
            confidence = min(0.9, 0.5 + buy_strength)
            if wm_signal:
                reason = f"买入信号强(含世界模型预测，强度{buy_strength:.2f})"
            else:
                reason = f"买入信号强(强度{buy_strength:.2f})"
        elif sell_strength > buy_strength + 0.2:
            action = "SELL"
            confidence = min(0.9, 0.5 + sell_strength)
            if wm_signal:
                reason = f"卖出信号强(含世界模型预测，强度{sell_strength:.2f})"
            else:
                reason = f"卖出信号强(强度{sell_strength:.2f})"
        else:
            action = "HOLD"
            confidence = 0.5
            reason = "信号冲突，观望"
        
        result = {
            "code": code,
            "name": data.get('name', code),
            "action": action,
            "confidence": round(confidence, 2),
            "signals": signals,
            "reason": reason,
            "current_price": data.get('price', 0)
        }
        
        if prediction:
            result["prediction"] = prediction
        
        return result
    
    def _risk_check(self, decisions: List[Dict], market_data: Dict) -> List[Dict]:
        """风控检查"""
        approved = []
        
        for decision in decisions:
            # 置信度检查
            if decision['confidence'] < 0.5:
                print(f"  ⛔ {decision['name']}: 置信度不足")
                continue
            
            # 仓位检查
            if decision['action'] == 'BUY':
                max_positions = self.risk_config.get("position_control", {}).get("max_positions", 3)
                if len(self.portfolio.get('holdings', {})) >= max_positions:
                    print(f"  ⛔ {decision['name']}: 持仓数量超限")
                    continue
                
                min_cash = self.risk_config.get("position_control", {}).get("min_cash_ratio", 0.2)
                if self.portfolio.get('cash', 0) < self.portfolio.get('total_value', 1) * min_cash:
                    print(f"  ⛔ {decision['name']}: 现金不足")
                    continue
            
            approved.append(decision)
        
        return approved
    
    def _execute_trades(self, decisions: List[Dict]) -> List[Dict]:
        """执行交易"""
        executed = []
        
        for decision in decisions:
            trade = self._simulate_trade(decision)
            if trade:
                executed.append(trade)
                self.portfolio.setdefault('trade_history', []).append(trade)
        
        return executed
    
    def _simulate_trade(self, decision: Dict) -> Optional[Dict]:
        """模拟交易"""
        action = decision['action']
        code = decision['code']
        price = decision['current_price']
        
        if action == 'BUY' and price > 0:
            allocation = 0.25 * decision['confidence']
            amount = min(self.portfolio['cash'] * allocation, 25000)
            shares = int(amount / price)
            cost = shares * price
            
            if cost > 0 and shares > 0:
                self.portfolio['cash'] -= cost
                self.portfolio.setdefault('holdings', {})[code] = {
                    "shares": shares,
                    "cost_price": price,
                    "cost_basis": cost
                }
                
                print(f"  ✅ 买入 {decision['name']}: {shares}股 @ {price}")
                
                return {
                    "timestamp": datetime.now().isoformat(),
                    "action": "BUY",
                    "code": code,
                    "name": decision['name'],
                    "shares": shares,
                    "price": price,
                    "cost": round(cost, 2)
                }
        
        elif action == 'SELL':
            holdings = self.portfolio.get('holdings', {})
            if code in holdings:
                holding = holdings[code]
                shares = holding['shares']
                revenue = shares * price
                pnl = revenue - holding['cost_basis']
                
                self.portfolio['cash'] += revenue
                del holdings[code]
                
                print(f"  ✅ 卖出 {decision['name']}: {shares}股 @ {price} (盈亏: {pnl:+.2f})")
                
                return {
                    "timestamp": datetime.now().isoformat(),
                    "action": "SELL",
                    "code": code,
                    "name": decision['name'],
                    "shares": shares,
                    "price": price,
                    "revenue": round(revenue, 2),
                    "pnl": round(pnl, 2)
                }
        
        return None
    
    def _learn(self, market_data: Dict, decisions: List[Dict], executed: List[Dict]):
        """学习总结（集成情景记忆）"""
        date = datetime.now().strftime('%Y-%m-%d')
        
        # 1. 基础记录（保持向后兼容）
        experience = {
            "date": date,
            "decisions": len(decisions),
            "executed": len(executed),
            "market_summary": {code: {"price": d["price"]} for code, d in market_data.items()}
        }
        
        # 保存到文件
        memory_file = f"/opt/hktech-agent/data/agent_{self.agent_id}_memory.json"
        memories = []
        if os.path.exists(memory_file):
            with open(memory_file, 'r') as f:
                memories = json.load(f)
        
        memories.append(experience)
        
        with open(memory_file, 'w') as f:
            json.dump(memories, f, indent=2)
        
        # 2. 情景记忆系统 - 存储详细经验
        try:
            # 分析市场状态
            market_desc = self._describe_market_condition(market_data)
            
            for trade in executed:
                # 提取经验教训
                lesson = self._extract_lesson(trade, market_data)
                
                # 存储到情景记忆
                self.agent_memory.store_lesson(
                    date=date,
                    market_condition=market_desc,
                    decision=f"{trade['action']} {trade['name']}",
                    result="success" if trade.get('pnl', 0) > 0 else "neutral" if trade.get('pnl', 0) == 0 else "failure",
                    lesson_text=lesson,
                    tags=[self.strategy_config.get('type', 'default'), trade['action']]
                )
            
            print(f"  ✅ 已存储 {len(executed)} 条经验到情景记忆")
            
        except Exception as e:
            print(f"  ⚠️ 情景记忆存储失败: {e}")
    
    def _evolve_strategies(self):
        """策略进化"""
        try:
            # 获取最近的交易记录
            recent_trades = self.portfolio.get('trade_history', [])[-20:]
            
            if len(recent_trades) < 5:
                print("  ℹ️  交易数据不足，跳过进化")
                return
            
            # 运行进化
            report = self.evolution.weekly_evolution_cycle(
                recent_trades=recent_trades,
                market_data={},
                llm_client=self.llm
            )
            
            print(f"  ✅ 策略进化完成")
            
            # 获取新的策略权重
            weights = self.evolution.get_strategy_weights()
            print(f"  📊 新策略权重:")
            for strategy, weight in weights.items():
                print(f"     {strategy}: {weight:.1%}")
            
            # 更新策略配置
            self.strategy_config['weights'] = weights
            
        except Exception as e:
            print(f"  ⚠️ 策略进化失败: {e}")
    
    def _describe_market_condition(self, market_data: Dict) -> str:
        """描述市场状态"""
        up_count = sum(1 for d in market_data.values() if d.get('change_pct', 0) > 0)
        down_count = len(market_data) - up_count
        
        if up_count > down_count:
            return f"市场普涨，{up_count}只股票上涨"
        elif down_count > up_count:
            return f"市场普跌，{down_count}只股票下跌"
        else:
            return "市场震荡，涨跌参半"
    
    def _extract_lesson(self, trade: Dict, market_data: Dict) -> str:
        """从交易中提取经验教训"""
        code = trade['code']
        action = trade['action']
        pnl = trade.get('pnl', 0)
        
        data = market_data.get(code, {})
        change = data.get('change_pct', 0)
        
        if action == 'BUY':
            if change > 0:
                return f"买入后市场上涨{change:.1f}%，时机判断正确"
            else:
                return f"买入后市场下跌{abs(change):.1f}%，需要更耐心等待"
        elif action == 'SELL':
            if pnl > 0:
                return f"卖出获利{pnl:.0f}元，止盈策略有效"
            elif pnl < 0:
                return f"卖出止损{abs(pnl):.0f}元，风控执行到位"
            else:
                return "平仓观望，等待更好机会"
        
        return "观望未操作"
    
    async def _send_report(self, market_data: Dict, executed: List[Dict]):
        """发送报告"""
        report = f"""
📊 {self.name} 日报
时间: {datetime.now().strftime('%Y-%m-%d')}

市场数据:
"""
        for code, data in market_data.items():
            report += f"  {data['name']}: {data['price']} ({data['change_pct']:+.2f}%)\n"
        
        report += f"\n交易: {len(executed)} 笔\n"
        for trade in executed:
            report += f"  {trade['action']} {trade['name']}: {trade['shares']}股\n"
        
        print(report)
        
        # 发送通知
        try:
            self.notifier.send_message(report)
        except:
            pass
    
    # ============== 状态管理 ==============
    
    def _load_portfolio(self) -> Dict:
        """加载投资组合"""
        portfolio_file = f"/opt/hktech-agent/data/agent_{self.agent_id}_portfolio.json"
        default = {
            "cash": 100000.0,
            "total_value": 100000.0,
            "holdings": {},
            "trade_history": []
        }
        
        if os.path.exists(portfolio_file):
            with open(portfolio_file, 'r') as f:
                loaded = json.load(f)
                default.update(loaded)
        
        return default
    
    def _load_day_count(self) -> int:
        """加载运行天数"""
        count_file = f"/opt/hktech-agent/data/agent_{self.agent_id}_count.json"
        if os.path.exists(count_file):
            with open(count_file, 'r') as f:
                return json.load(f).get('count', 1)
        return 1
    
    def _save_state(self):
        """保存状态"""
        # 保存组合
        portfolio_file = f"/opt/hktech-agent/data/agent_{self.agent_id}_portfolio.json"
        with open(portfolio_file, 'w') as f:
            json.dump(self.portfolio, f, indent=2, default=str)
        
        # 保存天数
        count_file = f"/opt/hktech-agent/data/agent_{self.agent_id}_count.json"
        with open(count_file, 'w') as f:
            json.dump({'count': self.day_count}, f)
    
    # ============== 查询接口 ==============
    
    def get_stats(self) -> Dict:
        """获取统计信息"""
        return {
            "agent_id": self.agent_id,
            "name": self.name,
            "state": self.state,
            "day_count": self.day_count,
            "stocks": self.stocks,
            "portfolio_value": self.portfolio.get('total_value', 0),
            "cash": self.portfolio.get('cash', 0),
            "holdings": len(self.portfolio.get('holdings', {}))
        }


if __name__ == "__main__":
    # 测试
    print("="*60)
    print("🤖 TradingAgent 测试")
    print("="*60)
    
    # 创建测试配置
    test_config = {
        "agent_id": "test_001",
        "name": "测试Agent",
        "market": "HK",
        "stocks": [
            {"code": "00700", "name": "腾讯控股"},
            {"code": "09988", "name": "阿里巴巴"}
        ],
        "risk_management": {
            "position_control": {
                "max_positions": 3,
                "min_cash_ratio": 0.2
            }
        },
        "strategy": {
            "type": "multi_factor",
            "factors": {"technical": 0.4, "fundamental": 0.3, "sentiment": 0.3}
        },
        "llm": {"model": "deepseek-chat"},
        "schedule": {},
        "notification": {}
    }
    
    agent = TradingAgent(test_config)
    print(f"\n统计: {agent.get_stats()}")
    
    print("\n✅ TradingAgent 测试完成！")
