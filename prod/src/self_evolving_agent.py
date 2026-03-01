#!/usr/bin/env python3
"""
Self-Evolving HK Tech Agent
自主演进的恒生科技交易Agent

核心特性：
1. 每日运行 - 数据采集、决策、执行
2. 情景记忆 - 记住什么策略在什么情况下有效
3. 世界模型 - 预测市场，形成预期
4. 进化引擎 - 每周自我改进策略参数
5. 元认知 - 反思决策，提取经验教训
"""

import os
import sys
import json
from datetime import datetime, timedelta
from typing import Dict, List

sys.path.insert(0, '/opt/hktech-agent')

from src.data_collector import HKStockDataCollector
from src.llm_client import DeepSeekClient
from src.evolution_engine import EvolutionEngine
from src.memory_system import EpisodicMemory
from src.world_model import WorldModel
from src.feishu_notifier import FeishuNotifier
from src.news_collector import NewsCollector
from src.stock_recommender import StockRecommender
from src.quarterly_report import QuarterlyReportGenerator


class SelfEvolvingAgent:
    """
    自主演进Agent - 真正会学习的交易AI
    """
    
    def __init__(self):
        """初始化所有组件"""
        print("🧬 初始化自主演进Agent...")
        
        self.data_collector = HKStockDataCollector()
        self.llm = DeepSeekClient()
        self.evolution = EvolutionEngine()
        self.memory = EpisodicMemory()
        self.world_model = WorldModel()
        self.notifier = FeishuNotifier()
        self.news_collector = NewsCollector()
        self.recommender = StockRecommender()
        self.report_generator = QuarterlyReportGenerator()
        
        self.portfolio = self._load_portfolio()
        self.day_count = self._load_day_count()
        self.start_date = self._load_start_date()
        
        print(f"✅ Agent已激活 | 第{self.day_count}天运行")
    
    def _load_portfolio(self) -> Dict:
        """加载投资组合"""
        portfolio_file = "/opt/hktech-agent/data/portfolio.json"
        default_portfolio = {
            "cash": 100000.0,
            "total_value": 100000.0,
            "holdings": {},
            "trade_history": [],
            "daily_returns": [],
            "created_at": datetime.now().isoformat()
        }
        
        if os.path.exists(portfolio_file):
            with open(portfolio_file, 'r') as f:
                loaded = json.load(f)
                # 确保所有必要字段存在
                for key, value in default_portfolio.items():
                    if key not in loaded:
                        loaded[key] = value
                return loaded
        
        return default_portfolio
    
    def _load_day_count(self) -> int:
        """加载运行天数"""
        count_file = "/opt/hktech-agent/data/day_count.json"
        if os.path.exists(count_file):
            with open(count_file, 'r') as f:
                return json.load(f).get('count', 1)
        return 1
    
    def _save_state(self):
        """保存状态"""
        # 保存组合
        with open("/opt/hktech-agent/data/portfolio.json", 'w') as f:
            json.dump(self.portfolio, f, indent=2, default=str)
        
        # 保存天数
        with open("/opt/hktech-agent/data/day_count.json", 'w') as f:
            json.dump({'count': self.day_count, 'last_run': datetime.now().isoformat()}, f)
    
    def run_daily_cycle(self):
        """
        每日运行周期
        
        完整的认知-决策-执行-学习循环
        """
        print("\n" + "="*70)
        print(f"🤖 自主演进Agent - Day {self.day_count} | {datetime.now().strftime('%Y-%m-%d %H:%M')}")
        print("="*70)
        
        # Step 1: 感知 - 采集数据
        print("\n👁️  Step 1: 感知环境...")
        market_data = self._perceive()
        
        # Step 2: 新闻 - 采集财经新闻
        print("\n📰 Step 2: 采集财经新闻...")
        news = self._collect_news()
        
        # Step 3: 认知 - 世界模型预测
        print("\n🧠 Step 3: 认知建模...")
        predictions, scenarios = self._cognize(market_data)
        
        # Step 4: 回忆 - 检索相似经验
        print("\n💭 Step 4: 情景回忆...")
        similar_episodes = self._recall(market_data)
        
        # Step 5: 决策 - LLM深度推理
        print("\n🎯 Step 5: 策略决策...")
        decisions = self._decide(market_data, predictions, scenarios, similar_episodes)
        
        # Step 6: 推荐 - 生成股票推荐
        print("\n📊 Step 6: 生成股票推荐...")
        recommendations = self._generate_recommendations(market_data, news, decisions)
        
        # Step 7: 执行 - 风控检查+交易
        print("\n⚡ Step 7: 执行交易...")
        executed = self._execute(decisions, market_data)
        
        # Step 8: 学习 - 存储经验
        print("\n📚 Step 8: 经验学习...")
        self._learn(market_data, decisions, executed)
        
        # Step 9: 每周进化
        if self.day_count % 7 == 0:
            print("\n🧬 Step 9: 自主进化...")
            self._evolve()
        
        # Step 10: 季度报告检查 (每90天)
        if self.day_count % 90 == 0:
            print("\n📈 Step 10: 生成本季度回溯报告...")
            self._generate_quarterly_report()
        
        # Step 11: 生成报告
        print("\n📄 Step 11: 生成日报...")
        report = self._report(market_data, predictions, executed, recommendations)
        
        # 更新天数
        self.day_count += 1
        self._save_state()
        
        print("\n" + "="*70)
        print(f"✅ Day {self.day_count-1} 完成 | 下次运行: 明天9:30")
        print("="*70)
        
        return report
    
    def _perceive(self) -> Dict:
        """感知：采集市场数据"""
        return self.data_collector.get_daily_data(days=30)
    
    def _cognize(self, market_data: Dict) -> tuple:
        """认知：世界模型预测"""
        # 预测价格
        predictions = self.world_model.predict(market_data, days_ahead=3)
        
        # 识别情景
        scenarios = self.world_model.identify_scenarios(market_data)
        
        # 验证之前的预测
        self.world_model.validate_predictions(market_data)
        
        if scenarios:
            print(f"  世界模型置信度: {self.world_model.get_model_confidence()}")
            print(f"  主要情景: {scenarios[0]['name']} ({scenarios[0]['probability']:.0%})")
        else:
            print(f"  世界模型置信度: {self.world_model.get_model_confidence()}")
            print(f"  主要情景: Unknown")
        
        return predictions, scenarios
    
    def _recall(self, market_data: Dict) -> List[Dict]:
        """回忆：检索相似经验"""
        episodes = self.memory.retrieve_similar_episodes(market_data, top_k=3)
        
        if episodes:
            print(f"  检索到 {len(episodes)} 个相似记忆:")
            for ep in episodes:
                outcome = ep.get('outcome', {})
                pnl = outcome.get('pnl', 0)
                print(f"    - {ep['decision'].get('action')} {ep['decision'].get('code')} "
                      f"| 结果: {pnl:+.0f} | 相似度: {ep.get('similarity', 0):.1%}")
        else:
            print("  无相似历史经验")
        
        return episodes
    
    def _decide(self, market_data, predictions, scenarios, episodes) -> List[Dict]:
        """决策：综合所有信息做出决策"""
        
        # 获取进化引擎的策略权重
        strategy_weights = self.evolution.get_strategy_weights()
        print(f"  当前策略权重: {', '.join(f'{k}={v:.1%}' for k, v in strategy_weights.items())}")
        
        # 构建提示
        expectations = self.world_model.generate_expectations(predictions, scenarios)
        
        # 生成决策
        decisions = []
        for code, data in market_data.items():
            # 多策略投票
            signals = self._generate_signals(data, strategy_weights)
            
            # 结合世界模型预期
            pred = predictions.get(code, {})
            if pred.get('direction_prob', 0.5) > 0.6:
                signals.append(('world_model', 'BUY', pred['confidence']))
            elif pred.get('direction_prob', 0.5) < 0.4:
                signals.append(('world_model', 'SELL', pred['confidence']))
            
            # 加权投票
            decision = self._vote(signals, code, data)
            decisions.append(decision)
        
        return decisions
    
    def _generate_signals(self, data: Dict, weights: Dict) -> List[tuple]:
        """基于各策略生成信号"""
        signals = []
        price = data.get('price', 0)
        ma5 = data.get('ma5', price)
        ma20 = data.get('ma20', price)
        rsi = data.get('rsi', 50)
        
        # 双均线策略
        if weights.get('dual_ma', 0) > 0.2:
            if ma5 > ma20 * 1.02:
                signals.append(('dual_ma', 'BUY', 0.6 * weights['dual_ma']))
            elif ma5 < ma20 * 0.98:
                signals.append(('dual_ma', 'SELL', 0.6 * weights['dual_ma']))
        
        # RSI策略
        if weights.get('rsi_reversal', 0) > 0.2:
            if rsi < 30:
                signals.append(('rsi_reversal', 'BUY', 0.5 * weights['rsi_reversal']))
            elif rsi > 70:
                signals.append(('rsi_reversal', 'SELL', 0.5 * weights['rsi_reversal']))
        
        # 动量策略
        if weights.get('momentum', 0) > 0.2:
            change = data.get('change_pct', 0)
            if change > 2:
                signals.append(('momentum', 'BUY', 0.4 * weights['momentum']))
            elif change < -2:
                signals.append(('momentum', 'SELL', 0.4 * weights['momentum']))
        
        return signals
    
    def _vote(self, signals: List[tuple], code: str, data: Dict) -> Dict:
        """策略投票，生成最终决策"""
        if not signals:
            return {
                "code": code,
                "name": data.get('name', code),
                "action": "HOLD",
                "confidence": 0.5,
                "reason": "无明确信号"
            }
        
        # 统计买入/卖出信号强度
        buy_strength = sum(s[2] for s in signals if s[1] == 'BUY')
        sell_strength = sum(s[2] for s in signals if s[1] == 'SELL')
        
        if buy_strength > sell_strength + 0.2:
            action = "BUY"
            confidence = min(0.9, 0.5 + buy_strength)
            reason = f"买入信号强(强度{buy_strength:.2f})"
        elif sell_strength > buy_strength + 0.2:
            action = "SELL"
            confidence = min(0.9, 0.5 + sell_strength)
            reason = f"卖出信号强(强度{sell_strength:.2f})"
        else:
            action = "HOLD"
            confidence = 0.5
            reason = "信号冲突，观望"
        
        return {
            "code": code,
            "name": data.get('name', code),
            "action": action,
            "confidence": round(confidence, 2),
            "signals": signals,
            "reason": reason,
            "current_price": data.get('price', 0)
        }
    
    def _execute(self, decisions: List[Dict], market_data: Dict) -> List[Dict]:
        """执行：风控+交易"""
        executed = []
        
        for decision in decisions:
            # 风控检查
            if not self._risk_check(decision):
                continue
            
            # 执行交易
            trade = self._simulate_trade(decision)
            if trade:
                executed.append(trade)
                self.portfolio['trade_history'].append(trade)
        
        # 更新组合市值
        self._update_portfolio_value(market_data)
        
        return executed
    
    def _risk_check(self, decision: Dict) -> bool:
        """风控检查 - 返回 False 时阻塞交易"""
        # 检查1: 置信度
        if decision['confidence'] < 0.5:
            print(f"  ⛔ {decision['name']}: 置信度{decision['confidence']:.0%}过低，阻塞交易")
            return False
        
        # 检查2: 仓位
        if decision['action'] == 'BUY':
            if len(self.portfolio['holdings']) >= 3 and decision['code'] not in self.portfolio['holdings']:
                print(f"  ⛔ {decision['name']}: 已达最大持仓数，阻塞交易")
                return False
            
            if self.portfolio['cash'] < 20000:
                print(f"  ⛔ {decision['name']}: 现金不足，阻塞交易")
                return False
        
        # 检查3: 持仓止损检查（持仓亏损超过15%时禁止加仓）
        if decision['action'] == 'BUY' and decision['code'] in self.portfolio['holdings']:
            holding = self.portfolio['holdings'][decision['code']]
            current_price = decision.get('current_price', 0)
            if current_price > 0:
                cost_price = holding['cost_price']
                loss_pct = (current_price - cost_price) / cost_price
                if loss_pct < -0.15:
                    print(f"  ⛔ {decision['name']}: 持仓亏损{loss_pct:.1%}，超过-15%止损线，阻塞加仓")
                    return False
        
        # 检查4: 卖出风控（可卖但记录）
        if decision['action'] == 'SELL' and decision['code'] not in self.portfolio['holdings']:
            print(f"  ⛔ {decision['name']}: 无持仓可卖，阻塞交易")
            return False
        
        return True
    
    def _simulate_trade(self, decision: Dict) -> Dict:
        """模拟交易执行"""
        action = decision['action']
        code = decision['code']
        price = decision['current_price']
        
        if action == 'BUY':
            # 根据置信度决定仓位
            allocation = 0.25 * decision['confidence']  # 最高25%
            amount = min(self.portfolio['cash'] * allocation, 25000)
            # 预留手续费(0.15%)和滑点(0.1%)
            fee_rate = 0.0015
            slippage_rate = 0.001
            available_amount = amount / (1 + fee_rate + slippage_rate)
            shares = int(available_amount / price)
            cost = shares * price
            fee = cost * fee_rate
            slippage = cost * slippage_rate
            total_cost = cost + fee + slippage
            
            if cost > 0 and total_cost <= self.portfolio['cash']:
                self.portfolio['cash'] -= total_cost
                
                # 累加持仓，计算加权平均成本
                if code in self.portfolio['holdings']:
                    existing = self.portfolio['holdings'][code]
                    old_shares = existing['shares']
                    old_cost = existing['cost_basis']
                    new_shares = old_shares + shares
                    new_cost = old_cost + cost
                    avg_price = new_cost / new_shares
                    self.portfolio['holdings'][code] = {
                        "shares": new_shares,
                        "cost_price": avg_price,
                        "cost_basis": new_cost
                    }
                else:
                    self.portfolio['holdings'][code] = {
                        "shares": shares,
                        "cost_price": price,
                        "cost_basis": cost
                    }
                
                trade = {
                    "timestamp": datetime.now().isoformat(),
                    "action": "BUY",
                    "code": code,
                    "name": decision['name'],
                    "shares": shares,
                    "price": price,
                    "cost": round(cost, 2),
                    "fee": round(fee, 2),
                    "slippage": round(slippage, 2),
                    "total_cost": round(total_cost, 2),
                    "confidence": decision['confidence']
                }
                print(f"  ✅ 买入 {decision['name']}: {shares}股 @ {price} = {cost:.2f} (手续费:{fee:.2f} 滑点:{slippage:.2f})")
                return trade
        
        elif action == 'SELL':
            if code in self.portfolio['holdings']:
                holding = self.portfolio['holdings'][code]
                existing_shares = holding['shares']
                # 卖出50%（或根据置信度）
                sell_ratio = min(1.0, 0.5 + decision.get('confidence', 0) * 0.5)
                sell_shares = int(existing_shares * sell_ratio)
                if sell_shares == 0:
                    sell_shares = existing_shares
                
                revenue = sell_shares * price
                fee = revenue * 0.0015
                net_revenue = revenue - fee
                
                remaining_shares = existing_shares - sell_shares
                cost_basis_sold = holding['cost_basis'] * (sell_shares / existing_shares)
                
                self.portfolio['cash'] += net_revenue
                
                # 只在全部卖出时删除持仓
                if remaining_shares > 0:
                    remaining_cost = holding['cost_basis'] * (remaining_shares / existing_shares)
                    self.portfolio['holdings'][code] = {
                        "shares": remaining_shares,
                        "cost_price": holding['cost_price'],
                        "cost_basis": remaining_cost
                    }
                else:
                    del self.portfolio['holdings'][code]
                
                # 计算盈亏
                pnl = net_revenue - cost_basis_sold
                
                trade = {
                    "timestamp": datetime.now().isoformat(),
                    "action": "SELL",
                    "code": code,
                    "name": decision['name'],
                    "shares": sell_shares,
                    "price": price,
                    "revenue": round(net_revenue, 2),
                    "fee": round(fee, 2),
                    "pnl": round(pnl, 2)
                }
                print(f"  ✅ 卖出 {decision['name']}: {sell_shares}股 @ {price} = {revenue:.2f} (手续费:{fee:.2f} 盈亏: {pnl:+.2f})")
                return trade
        
        return None
    
    def _update_portfolio_value(self, market_data: Dict):
        """更新组合市值"""
        total_value = self.portfolio['cash']
        
        for code, holding in self.portfolio['holdings'].items():
            if code in market_data:
                current_value = holding['shares'] * market_data[code]['price']
                total_value += current_value
        
        self.portfolio['total_value'] = round(total_value, 2)
        
        # 计算日收益
        if len(self.portfolio['daily_returns']) > 0:
            last_value = self.portfolio['daily_returns'][-1]['value']
            daily_return = (total_value - last_value) / last_value
        else:
            daily_return = 0
        
        self.portfolio['daily_returns'].append({
            "date": datetime.now().strftime('%Y-%m-%d'),
            "value": total_value,
            "return": round(daily_return * 100, 2)
        })
    
    def _learn(self, market_data: Dict, decisions: List[Dict], executed: List[Dict]):
        """学习：存储经验到情景记忆"""
        for trade in executed:
            # 找到对应的决策
            decision = next((d for d in decisions if d['code'] == trade['code']), None)
            if decision:
                self.memory.store_episode(
                    market_state=market_data,
                    decision=decision,
                    outcome={"pnl": trade.get('pnl', 0), "revenue": trade.get('revenue', 0)},
                    reflection=""
                )
        
        # 存储每日市场状态（无交易时也要学习）
        if not executed:
            self.memory.store_episode(
                market_state=market_data,
                decision={"action": "HOLD", "code": "ALL", "reason": "观望"},
                outcome={"pnl": 0},
                reflection="市场信号不明确，选择观望"
            )
    
    def _evolve(self):
        """进化：每周自我改进"""
        # 获取最近7天的交易
        recent_trades = [t for t in self.portfolio['trade_history'] 
                        if datetime.fromisoformat(t['timestamp']) > 
                        datetime.now() - timedelta(days=7)]
        
        # 获取市场数据用于检测市场状态
        market_data = self.data_collector.load_historical_data()
        if not market_data:
            market_data = {}
        
        # 运行进化周期
        report = self.evolution.weekly_evolution_cycle(
            recent_trades=recent_trades,
            market_data=market_data,
            llm_client=self.llm
        )
        
        print(report)
        
        # 保存进化报告
        report_file = f"/opt/hktech-agent/data/evolution_report_{datetime.now().strftime('%Y%m%d')}.txt"
        with open(report_file, 'w') as f:
            f.write(report)
    
    def _report(self, market_data, predictions, executed, recommendations=None) -> str:
        """生成日报"""
        stats = self.memory.get_memory_stats()
        
        report = f"""
🤖 自主演进Agent日报 - Day {self.day_count} | {datetime.now().strftime('%Y-%m-%d')}
{'='*70}

📊 市场表现:
"""
        for code, data in market_data.items():
            emoji = "📈" if data.get('change_pct', 0) > 0 else "📉"
            report += f"{emoji} {data['name']}: {data['price']} ({data['change_pct']:+.2f}%)\n"
        
        # 添加推荐
        if recommendations:
            report += f"\n⭐ 今日推荐:\n"
            for rec in recommendations[:3]:  # 只显示前3
                emoji = "🟢" if rec['total_score'] > 0.3 else "🔴" if rec['total_score'] < -0.3 else "⚪"
                report += f"  {emoji} {rec['name']}: {rec['action']} (评分: {rec['total_score']:+.2f})\n"
                report += f"     目标价: {rec['target_price']} | 止损: {rec['stop_loss']}\n"
        
        report += f"\n🎯 今日交易 ({len(executed)}笔):\n"
        if executed:
            for trade in executed:
                if trade['action'] == 'BUY':
                    report += f"  🟢 买入 {trade['name']}: {trade['shares']}股 @ {trade['price']}\n"
                else:
                    pnl = trade.get('pnl', 0)
                    emoji = "✅" if pnl > 0 else "❌"
                    report += f"  🔴 卖出 {trade['name']}: {trade['shares']}股 @ {trade['price']} {emoji} {pnl:+.2f}\n"
        else:
            report += "  无交易\n"
        
        report += f"""
💰 组合状态:
  总市值: {self.portfolio['total_value']:,.2f}
  现金: {self.portfolio['cash']:,.2f} ({self.portfolio['cash']/self.portfolio['total_value']:.1%})
  持仓: {len(self.portfolio['holdings'])}只股票

🧠 认知状态:
  世界模型置信度: {self.world_model.get_model_confidence():.0%}
  情景记忆数量: {stats.get('total', 0)}
  策略胜率: {stats.get('win_rate', 0):.1%}

{'='*70}
"""
        
        # 保存报告
        report_file = f"/opt/hktech-agent/data/report_{datetime.now().strftime('%Y%m%d')}.txt"
        with open(report_file, 'w') as f:
            f.write(report)
        
        print(report)
        
        # 飞书通知
        self.notifier.send_market_update(market_data, executed)
        
        return report
    
    def _load_start_date(self) -> str:
        """加载开始日期"""
        count_file = "/opt/hktech-agent/data/day_count.json"
        if os.path.exists(count_file):
            with open(count_file, 'r') as f:
                data = json.load(f)
                return data.get('start_date', datetime.now().strftime('%Y-%m-%d'))
        return datetime.now().strftime('%Y-%m-%d')
    
    def _collect_news(self) -> List[Dict]:
        """采集新闻"""
        return self.news_collector.get_daily_news()
    
    def _generate_recommendations(self, market_data: Dict, news: List[Dict], decisions: List[Dict]) -> List[Dict]:
        """生成股票推荐"""
        # 获取新闻情绪
        sentiment = {}
        for code in market_data.keys():
            sentiment[code] = self.news_collector.get_stock_sentiment(code, days=3)
        
        # 转换决策为策略信号
        strategy_signals = {d['code']: d for d in decisions}
        
        # 生成推荐
        recommendations = self.recommender.generate_recommendations(
            market_data, sentiment, strategy_signals
        )
        
        return recommendations
    
    def _generate_quarterly_report(self):
        """生成本季度回溯报告"""
        report = self.report_generator.generate_quarterly_report(llm_client=self.llm)
        print(f"\n📊 本季度报告已生成！")
        print(f"查看: /opt/hktech-agent/data/reports/quarterly_report_*.md")


def main():
    """主入口"""
    agent = SelfEvolvingAgent()
    agent.run_daily_cycle()


if __name__ == "__main__":
    main()
