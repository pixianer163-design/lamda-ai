"""
VectorBT 与恒生Agent集成模块
提供向量化回测能力，用于快速策略筛选
"""

import vectorbt as vbt
import pandas as pd
import numpy as np
from typing import Dict, List, Tuple, Optional
from datetime import datetime, timedelta
import warnings
warnings.filterwarnings('ignore')


class VectorBTBacktester:
    """
    VectorBT回测器 - 快速策略筛选
    
    与现有恒生Agent的集成点:
    1. 使用相同的数据源 (Yahoo Finance)
    2. 使用相同的技术指标
    3. 使用相同的信号生成逻辑
    4. 输出格式与现有回测兼容
    """
    
    def __init__(self, initial_cash: float = 100000, fees: float = 0.001):
        """
        初始化回测器
        
        Args:
            initial_cash: 初始资金
            fees: 手续费率 (默认0.1%)
        """
        self.initial_cash = initial_cash
        self.fees = fees
        self.portfolio = None
        
    def fetch_data(self, symbols: List[str], 
                   start: str = None, 
                   end: str = None,
                   period: str = "1y") -> pd.DataFrame:
        """
        获取股票数据 (与恒生Agent使用相同数据源)
        
        Args:
            symbols: 股票代码列表 (如 ['00700.HK', '09988.HK'])
            start: 开始日期 (YYYY-MM-DD)
            end: 结束日期 (YYYY-MM-DD)
            period: 默认1年
            
        Returns:
            DataFrame: 价格数据 (MultiIndex: date, symbol)
        """
        print(f"📊 下载数据: {symbols}")
        
        # 转换港股代码格式
        # 恒生Agent格式: 00700 -> Yahoo格式: 0700.HK
        yahoo_symbols = []
        for s in symbols:
            if s.isdigit() and len(s) == 5:  # 港股代码
                yahoo_symbols.append(f"{s[1:]}.HK")  # 去掉前导0
            else:
                yahoo_symbols.append(s)
        
        # 使用VectorBT下载数据
        data = vbt.YFData.download(
            yahoo_symbols,
            start=start,
            end=end,
            period=period,
            missing_index="drop"
        )
        
        return data.get("Close")
    
    def calculate_indicators(self, price: pd.DataFrame,
                            fast_window: int = 10,
                            slow_window: int = 50,
                            rsi_period: int = 14) -> Dict:
        """
        计算技术指标 (与恒生Agent相同)
        
        Args:
            price: 价格DataFrame
            fast_window: 快速均线窗口
            slow_window: 慢速均线窗口
            rsi_period: RSI周期
            
        Returns:
            Dict: 包含各种指标的字典
        """
        print(f"📈 计算指标: MA({fast_window},{slow_window}), RSI({rsi_period})")
        
        # 移动平均线
        fast_ma = vbt.MA.run(price, fast_window, short_name='fast')
        slow_ma = vbt.MA.run(price, slow_window, short_name='slow')
        
        # RSI
        rsi = vbt.RSI.run(price, rsi_period)
        
        # 布林带
        bbands = vbt.BollingerBands.run(price, window=20, alpha=2)
        
        return {
            'fast_ma': fast_ma,
            'slow_ma': slow_ma,
            'rsi': rsi,
            'bbands': bbands
        }
    
    def generate_signals(self, indicators: Dict,
                        strategy_type: str = "ma_cross") -> Tuple[pd.DataFrame, pd.DataFrame]:
        """
        生成交易信号 (与恒生Agent兼容)
        
        Args:
            indicators: 技术指标字典
            strategy_type: 策略类型 (ma_cross, rsi, bollinger)
            
        Returns:
            Tuple: (entries, exits) 信号DataFrame
        """
        price = indicators['fast_ma'].close
        
        if strategy_type == "ma_cross":
            # 双均线交叉策略
            entries = indicators['fast_ma'].ma_crossed_above(indicators['slow_ma'])
            exits = indicators['fast_ma'].ma_crossed_below(indicators['slow_ma'])
            
        elif strategy_type == "rsi":
            # RSI策略: <30买入, >70卖出
            rsi_values = indicators['rsi'].rsi
            entries = rsi_values < 30
            exits = rsi_values > 70
            
        elif strategy_type == "bollinger":
            # 布林带策略: 触及下轨买入, 触及上轨卖出
            entries = price < indicators['bbands'].lower
            exits = price > indicators['bbands'].upper
            
        else:
            raise ValueError(f"未知策略类型: {strategy_type}")
        
        return entries, exits
    
    def run_backtest(self, price: pd.DataFrame,
                    entries: pd.DataFrame,
                    exits: pd.DataFrame,
                    slippage: float = 0.001) -> vbt.Portfolio:
        """
        执行回测
        
        Args:
            price: 价格数据
            entries: 入场信号
            exits: 出场信号
            slippage: 滑点
            
        Returns:
            Portfolio: 回测结果
        """
        print(f"🚀 执行回测...")
        
        self.portfolio = vbt.Portfolio.from_signals(
            price,
            entries,
            exits,
            init_cash=self.initial_cash,
            fees=self.fees,
            slippage=slippage,
            freq='1d'  # 日频
        )
        
        return self.portfolio
    
    def get_metrics(self) -> Dict:
        """
        获取回测指标 (与恒生Agent回测格式兼容)
        
        Returns:
            Dict: 绩效指标
        """
        if self.portfolio is None:
            raise ValueError("请先执行回测")
        
        pf = self.portfolio
        
        return {
            'total_return': pf.total_return(),
            'total_profit': pf.total_profit(),
            'sharpe_ratio': pf.sharpe_ratio(),
            'max_drawdown': pf.max_drawdown(),
            'win_rate': pf.trades.win_rate(),
            'avg_winning_trade': pf.trades.returns_avg_winning(),
            'avg_losing_trade': pf.trades.returns_avg_losing(),
            'profit_factor': pf.trades.profit_factor(),
            'expectancy': pf.trades.expectancy(),
            'total_trades': pf.trades.count(),
            'avg_trade_duration': pf.trades.duration.mean()
        }
    
    def optimize_parameters(self, price: pd.DataFrame,
                           strategy_type: str = "ma_cross",
                           fast_range: range = range(5, 51, 5),
                           slow_range: range = range(50, 201, 10)) -> pd.DataFrame:
        """
        超参数优化 - VectorBT核心优势
        
        Args:
            price: 价格数据
            strategy_type: 策略类型
            fast_range: 快速参数范围
            slow_range: 慢速参数范围
            
        Returns:
            DataFrame: 各参数组合的回测结果
        """
        print(f"🔍 参数优化: 测试 {len(fast_range) * len(slow_range)} 种组合...")
        
        if strategy_type == "ma_cross":
            # 批量计算所有均线组合
            fast_ma = vbt.MA.run(price, list(fast_range), short_name='fast')
            slow_ma = vbt.MA.run(price, list(slow_range), short_name='slow')
            
            # 生成所有组合的信号
            entries = fast_ma.ma_crossed_above(slow_ma)
            exits = fast_ma.ma_crossed_below(slow_ma)
            
            # 批量回测
            pf = vbt.Portfolio.from_signals(
                price,
                entries,
                exits,
                init_cash=self.initial_cash,
                fees=self.fees
            )
            
            # 提取结果
            returns = pf.total_return()
            
            # 转换为DataFrame
            results = []
            for (fast, slow), ret in returns.items():
                results.append({
                    'fast_window': fast,
                    'slow_window': slow,
                    'total_return': ret,
                    'sharpe_ratio': pf.sharpe_ratio()[(fast, slow)],
                    'max_drawdown': pf.max_drawdown()[(fast, slow)],
                    'win_rate': pf.trades.win_rate()[(fast, slow)]
                })
            
            return pd.DataFrame(results).sort_values('total_return', ascending=False)
        
        else:
            raise NotImplementedError(f"暂不支持策略类型: {strategy_type}")
    
    def plot_results(self):
        """绘制回测结果"""
        if self.portfolio is None:
            raise ValueError("请先执行回测")
        
        # 返回图表对象
        fig = self.portfolio.plot()
        return fig
    
    def compare_with_hktech(self, hktech_results: Dict) -> Dict:
        """
        与恒生Agent现有回测结果对比
        
        Args:
            hktech_results: 恒生Agent回测结果
            
        Returns:
            Dict: 对比结果
        """
        vbt_metrics = self.get_metrics()
        
        comparison = {
            'metric': [],
            'vectorbt': [],
            'hktech': [],
            'difference': []
        }
        
        for metric in ['total_return', 'sharpe_ratio', 'max_drawdown', 'win_rate']:
            comparison['metric'].append(metric)
            comparison['vectorbt'].append(vbt_metrics.get(metric, 'N/A'))
            comparison['hktech'].append(hktech_results.get(metric, 'N/A'))
            
            if metric in vbt_metrics and metric in hktech_results:
                diff = vbt_metrics[metric] - hktech_results[metric]
                comparison['difference'].append(f"{diff:+.2%}")
            else:
                comparison['difference'].append('N/A')
        
        return pd.DataFrame(comparison)


# 使用示例
if __name__ == '__main__':
    # 初始化回测器
    backtester = VectorBTBacktester(initial_cash=100000, fees=0.001)
    
    # 1. 获取数据
    symbols = ['00700', '09988', '03690']  # 腾讯、阿里、美团
    price = backtester.fetch_data(symbols, period="1y")
    print(f"📊 数据 shape: {price.shape}")
    
    # 2. 计算指标
    indicators = backtester.calculate_indicators(price, fast_window=10, slow_window=50)
    
    # 3. 生成信号
    entries, exits = backtester.generate_signals(indicators, strategy_type="ma_cross")
    
    # 4. 执行回测
    portfolio = backtester.run_backtest(price, entries, exits)
    
    # 5. 获取结果
    metrics = backtester.get_metrics()
    print("\n📈 回测结果:")
    for key, value in metrics.items():
        if isinstance(value, float):
            print(f"  {key}: {value:.4f}")
        else:
            print(f"  {key}: {value}")
    
    # 6. 参数优化示例
    print("\n🔍 开始参数优化...")
    optimization_results = backtester.optimize_parameters(
        price,
        fast_range=range(5, 31, 5),
        slow_range=range(30, 101, 10)
    )
    print("\n🏆 Top 5 参数组合:")
    print(optimization_results.head())
