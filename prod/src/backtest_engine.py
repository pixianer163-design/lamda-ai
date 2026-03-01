#!/usr/bin/env python3
"""
回测框架 (Backtesting Framework)
用历史数据验证策略有效性
"""

import json
import os
import sys
from datetime import datetime, timedelta
from typing import Dict, List, Tuple
from dataclasses import dataclass, asdict
import numpy as np

sys.path.insert(0, '/opt/hktech-agent/prod/src')

from rssm_world_model import WorldModelTrainer
from llm_signal_extractor import LLMSignalExtractor


@dataclass
class Trade:
    """交易记录"""
    date: str
    code: str
    action: str  # BUY/SELL
    shares: int
    price: float
    amount: float
    reason: str


@dataclass
class DailyState:
    """每日状态"""
    date: str
    cash: float
    portfolio_value: float
    total_value: float
    holdings: Dict
    trades: List[Trade]
    signals: Dict


class BacktestEngine:
    """
    回测引擎
    
    功能:
    1. 加载历史数据
    2. 模拟交易执行
    3. 计算收益指标
    4. 对比不同策略
    """
    
    def __init__(self, data_dir="/opt/hktech-agent/data", 
                 start_date=None, end_date=None,
                 initial_capital=100000):
        self.data_dir = data_dir
        self.start_date = start_date or "2024-01-01"
        self.end_date = end_date or "2024-12-31"
        self.initial_capital = initial_capital
        
        # 加载历史数据
        self.price_data = self._load_price_data()
        
        # 初始化组件
        try:
            self.world_model = WorldModelIntegration()
            self.use_world_model = self.world_model.enabled
        except:
            self.use_world_model = False
            
        try:
            self.signal_extractor = LLMSignalExtractor()
            self.use_llm = True
        except:
            self.use_llm = False
        
        print(f"📊 回测引擎初始化")
        print(f"   时间范围: {self.start_date} ~ {self.end_date}")
        print(f"   初始资金: ¥{initial_capital:,.0f}")
        print(f"   世界模型: {'✅' if self.use_world_model else '❌'}")
        print(f"   LLM信号: {'✅' if self.use_llm else '❌'}")
    
    def _load_price_data(self) -> Dict:
        """加载历史价格数据"""
        data = {}
        
        for code in ['00700', '09988', '03690', '09618', '01810', '01024']:
            filepath = f"{self.data_dir}/historical/{code}_5y.json"
            if os.path.exists(filepath):
                with open(filepath, 'r') as f:
                    stock_data = json.load(f)
                    # 过滤日期范围
                    filtered = [
                        d for d in stock_data 
                        if self.start_date <= d['date'] <= self.end_date
                    ]
                    data[code] = filtered
                    print(f"   {code}: 加载 {len(filtered)} 天数据")
        
        return data
    
    def run_backtest(self, strategy_name="base", strategy_params=None) -> Dict:
        """
        运行回测
        
        strategy_name: 
            - "base": 基础策略（仅技术指标）
            - "rssm": 加入世界模型
            - "llm": 加入LLM信号
            - "full": 完整策略（所有模块）
        """
        print(f"\n🚀 开始回测: {strategy_name}")
        print("="*50)
        
        # 获取共同交易日
        dates = self._get_trading_dates()
        
        # 初始化状态
        state = {
            'cash': self.initial_capital,
            'holdings': {},
            'trades': [],
            'daily_states': []
        }
        
        # 逐日回测
        for i, date in enumerate(dates):
            # 获取当日市场数据
            market_data = self._get_market_data(date)
            if not market_data:
                continue
            
            # 生成交易信号
            signals = self._generate_signals(
                date, market_data, state, strategy_name
            )
            
            # 执行交易
            trades = self._execute_trades(date, state, signals, market_data)
            state['trades'].extend(trades)
            
            # 记录每日状态
            portfolio_value = self._calc_portfolio_value(state, market_data)
            daily_state = DailyState(
                date=date,
                cash=state['cash'],
                portfolio_value=portfolio_value,
                total_value=state['cash'] + portfolio_value,
                holdings=state['holdings'].copy(),
                trades=trades,
                signals=signals
            )
            state['daily_states'].append(asdict(daily_state))
            
            # 打印进度
            if (i + 1) % 50 == 0:
                print(f"   进度: {i+1}/{len(dates)}  {date}  净值: {daily_state.total_value/self.initial_capital:.2f}")
        
        # 计算回测结果
        results = self._calculate_metrics(state)
        results['strategy'] = strategy_name
        results['params'] = strategy_params or {}
        
        print(f"\n✅ 回测完成: {strategy_name}")
        
        return results
    
    def _get_trading_dates(self) -> List[str]:
        """获取所有交易日（取三只股票的交集）"""
        dates = set()
        for code, data in self.price_data.items():
            dates.update([d['date'] for d in data])
        return sorted(list(dates))
    
    def _get_market_data(self, date: str) -> Dict:
        """获取某日的市场数据"""
        data = {}
        for code in ['00700', '09988', '03690', '09618', '01810', '01024']:
            day_data = next(
                (d for d in self.price_data.get(code, []) if d['date'] == date),
                None
            )
            if day_data:
                data[code] = day_data
        return data
    
    def _generate_signals(self, date: str, market_data: Dict, 
                         state: Dict, strategy_name: str) -> Dict:
        """生成交易信号"""
        signals = {}
        
        for code in ['00700', '09988', '03690', '09618', '01810', '01024']:
            if code not in market_data:
                continue
            
            data = market_data[code]
            rsi = data.get('rsi', 50)
            change = data.get('change_pct', 0)
            
            # 基础策略：均线交叉 + RSI
            price = data.get('close', 0)
            ma5 = data.get('ma5', price)
            ma20 = data.get('ma20', price)
            
            # 均线金叉 + RSI支持
            if price > ma5 > ma20 and rsi < 70:
                action = "BUY"
                confidence = 0.6
            # 均线死叉 + RSI支持
            elif price < ma5 < ma20 and rsi > 30:
                action = "SELL"
                confidence = 0.6
            else:
                action = "HOLD"
                confidence = 0.5
            
            # 加入世界模型（如果启用）
            if strategy_name in ['rssm', 'full'] and self.use_world_model:
                # 简化：根据历史趋势微调
                if 'ma20' in data and 'close' in data:
                    trend = (data['close'] - data['ma20']) / data['ma20']
                    if trend > 0.05 and action == "HOLD":
                        action = "BUY"
                        confidence = 0.55
                    elif trend < -0.05 and action == "HOLD":
                        action = "SELL"
                        confidence = 0.55
            
            # 加入LLM信号（如果启用）
            if strategy_name in ['llm', 'full'] and self.use_llm:
                # 使用技术指标作为信号基础，不使用随机生成
                # 如果技术指标已经有明确信号，保持不变
                # 如果技术指标是HOLD，可以参考RSI/涨跌作为辅助
                rsi = data.get('rsi', 50)
                change = data.get('change_pct', 0)
                
                # 基于RSI和市场情绪的技术信号
                if rsi < 30 and change > 0:
                    sentiment = 0.7  # 超卖反弹
                elif rsi > 70 and change < 0:
                    sentiment = 0.3  # 超买回调
                else:
                    sentiment = 0.5  # 中性
                
                # 只有在技术指标不确定时使用情绪辅助
                if action == "HOLD":
                    if sentiment > 0.6:
                        action = "BUY"
                        confidence = min(0.7, confidence + 0.1)
                    elif sentiment < 0.4:
                        action = "SELL"
                        confidence = min(0.7, confidence + 0.1)
            
            signals[code] = {
                'action': action,
                'confidence': confidence,
                'price': data.get('close', 0)
            }
        
        return signals
    
    def _execute_trades(self, date: str, state: Dict, 
                       signals: Dict, market_data: Dict) -> List[Trade]:
        """执行交易"""
        trades = []
        
        for code, signal in signals.items():
            action = signal['action']
            price = signal['price']
            confidence = signal['confidence']
            
            # 只执行高置信度交易
            if confidence < 0.50:
                continue
            
            # 检查当前持仓
            current_shares = state['holdings'].get(code, {}).get('shares', 0)
            
            if action == "BUY" and current_shares == 0:
                # 买入 - 只在无持仓时买入
                buy_amount = state['cash'] * 0.3  # 30%资金
                if buy_amount > 10000:
                    shares = int(buy_amount / price)
                    if shares > 0:
                        cost = shares * price
                        state['cash'] -= cost
                        
                        state['holdings'][code] = {'shares': shares, 'cost': cost}
                        
                        trades.append(Trade(
                            date=date, code=code, action="BUY",
                            shares=shares, price=price,
                            amount=cost, reason=f"MA+RSI signal"
                        ))
            
            elif action == "SELL" and current_shares > 0:
                # 卖出 - 只在有持仓时卖出
                shares = current_shares
                revenue = shares * price
                state['cash'] += revenue
                
                del state['holdings'][code]
                
                trades.append(Trade(
                    date=date, code=code, action="SELL",
                    shares=shares, price=price,
                    amount=revenue, reason="MA+RSI signal"
                ))
        
        return trades
    
    def _calc_portfolio_value(self, state: Dict, market_data: Dict) -> float:
        """计算组合市值"""
        value = 0
        for code, holding in state['holdings'].items():
            if code in market_data:
                price = market_data[code].get('close', 0)
                value += holding['shares'] * price
        return value
    
    def _calculate_metrics(self, state: Dict) -> Dict:
        """计算回测指标"""
        daily_states = state['daily_states']
        if not daily_states:
            return {}
        
        # 提取净值序列
        values = [d['total_value'] for d in daily_states]
        dates = [d['date'] for d in daily_states]
        
        # 计算收益率
        total_return = (values[-1] - self.initial_capital) / self.initial_capital
        
        # 计算年化收益率
        days = len(daily_states)
        annual_return = (1 + total_return) ** (252 / days) - 1 if days > 0 else 0
        
        # 计算最大回撤
        peak = values[0]
        max_drawdown = 0
        for v in values:
            if v > peak:
                peak = v
            drawdown = (peak - v) / peak
            max_drawdown = max(max_drawdown, drawdown)
        
        # 计算波动率（日收益率的标准差）
        daily_returns = []
        for i in range(1, len(values)):
            ret = (values[i] - values[i-1]) / values[i-1]
            daily_returns.append(ret)
        
        volatility = np.std(daily_returns) * np.sqrt(252) if daily_returns else 0
        
        # 计算夏普比率（假设无风险利率2%）
        risk_free_rate = 0.02
        sharpe_ratio = (annual_return - risk_free_rate) / volatility if volatility > 0 else 0
        
        # 交易统计
        all_trades = []
        for d in daily_states:
            all_trades.extend([Trade(**t) for t in d['trades']])
        
        buy_trades = [t for t in all_trades if t.action == "BUY"]
        sell_trades = [t for t in all_trades if t.action == "SELL"]
        
        return {
            'initial_capital': self.initial_capital,
            'final_value': values[-1],
            'total_return': round(total_return * 100, 2),  # %
            'annual_return': round(annual_return * 100, 2),  # %
            'max_drawdown': round(max_drawdown * 100, 2),  # %
            'volatility': round(volatility * 100, 2),  # %
            'sharpe_ratio': round(sharpe_ratio, 2),
            'total_trades': len(all_trades),
            'buy_trades': len(buy_trades),
            'sell_trades': len(sell_trades),
            'trading_days': days,
            'daily_states': daily_states,
            'equity_curve': [(d, v) for d, v in zip(dates, values)]
        }
    
    def compare_strategies(self) -> Dict:
        """对比多个策略"""
        strategies = ['base', 'rssm', 'llm', 'full']
        results = {}
        
        print("\n" + "="*60)
        print("📊 多策略对比回测")
        print("="*60)
        
        for strategy in strategies:
            results[strategy] = self.run_backtest(strategy)
        
        # 打印对比表
        print("\n" + "="*60)
        print("📈 策略对比结果")
        print("="*60)
        print(f"{'策略':<10} {'总收益':<10} {'年化':<10} {'最大回撤':<10} {'夏普':<10} {'交易次数':<10}")
        print("-"*60)
        
        for name, result in results.items():
            print(f"{name:<10} {result['total_return']:>8.2f}% {result['annual_return']:>8.2f}% "
                  f"{result['max_drawdown']:>8.2f}% {result['sharpe_ratio']:>8.2f} "
                  f"{result['total_trades']:>8}")
        
        # 找出最佳策略
        best_sharpe = max(results.items(), key=lambda x: x[1]['sharpe_ratio'])
        best_return = max(results.items(), key=lambda x: x[1]['total_return'])
        
        print("\n🏆 最佳策略:")
        print(f"   夏普比率最高: {best_sharpe[0]} ({best_sharpe[1]['sharpe_ratio']:.2f})")
        print(f"   总收益最高: {best_return[0]} ({best_return[1]['total_return']:.2f}%)")
        
        return results
    
    def save_results(self, results: Dict, filename=None):
        """保存回测结果"""
        if filename is None:
            filename = f"backtest_{results['strategy']}_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        
        filepath = f"{self.data_dir}/backtests/{filename}"
        os.makedirs(os.path.dirname(filepath), exist_ok=True)
        
        with open(filepath, 'w') as f:
            json.dump(results, f, indent=2, default=str)
        
        print(f"\n💾 结果已保存: {filepath}")


def main():
    """主函数"""
    print("="*60)
    print("🧪 恒生科技Agent - 回测框架")
    print("="*60)
    
    # 创建回测引擎
    engine = BacktestEngine(
        start_date="2024-01-01",
        end_date="2024-06-30",
        initial_capital=100000
    )
    
    # 运行多策略对比
    results = engine.compare_strategies()
    
    # 保存最佳策略结果
    best = max(results.items(), key=lambda x: x[1]['sharpe_ratio'])
    engine.save_results(best[1], f"best_strategy_{best[0]}.json")
    
    print("\n✅ 回测完成!")
    print(f"📁 结果保存于: /opt/hktech-agent/data/backtests/")


if __name__ == "__main__":
    main()
