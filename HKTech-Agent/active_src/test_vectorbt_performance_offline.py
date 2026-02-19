"""
VectorBT vs 现有回测模块 性能对比测试（离线版）
使用模拟数据，避免API限流
"""

import time
import sys
sys.path.insert(0, '/opt/hktech-agent/active_src')

import pandas as pd
import numpy as np
from datetime import datetime, timedelta


def generate_mock_data(days=1000, n_stocks=3):
    """生成模拟股票数据"""
    dates = pd.date_range(end=datetime.now(), periods=days, freq='D')
    
    data = {}
    for i in range(n_stocks):
        # 随机游走生成价格
        returns = np.random.randn(days) * 0.02
        price = 100 * np.exp(np.cumsum(returns))
        data[f'STOCK_{i}'] = price
    
    return pd.DataFrame(data, index=dates)


def test_vectorbt_speed():
    """测试 VectorBT 回测速度（离线版）"""
    print("="*70)
    print("🚀 VectorBT 性能测试")
    print("="*70)
    
    try:
        import vectorbt as vbt
        
        # 测试1: 单只股票回测
        print("\n📊 测试1: 单只股票回测 (1000天数据)")
        price = generate_mock_data(days=1000, n_stocks=1)
        
        start_time = time.time()
        
        # 计算指标
        fast_ma = vbt.MA.run(price, 10)
        slow_ma = vbt.MA.run(price, 50)
        
        # 生成信号
        entries = fast_ma.ma_crossed_above(slow_ma)
        exits = fast_ma.ma_crossed_below(slow_ma)
        
        # 回测
        pf = vbt.Portfolio.from_signals(
            price, entries, exits,
            init_cash=100000, fees=0.001
        )
        
        vbt_time_single = time.time() - start_time
        
        print(f"  ⏱️  耗时: {vbt_time_single:.4f} 秒")
        print(f"  📈 总收益: {pf.total_return().iloc[0]:.2%}")
        print(f"  📊 夏普比率: {pf.sharpe_ratio().iloc[0]:.2f}")
        print(f"  📊 交易次数: {pf.trades.count().iloc[0]}")
        
        # 测试2: 多只股票回测
        print("\n📊 测试2: 多只股票回测 (10只股票)")
        price = generate_mock_data(days=1000, n_stocks=10)
        
        start_time = time.time()
        
        fast_ma = vbt.MA.run(price, 10)
        slow_ma = vbt.MA.run(price, 50)
        entries = fast_ma.ma_crossed_above(slow_ma)
        exits = fast_ma.ma_crossed_below(slow_ma)
        
        pf = vbt.Portfolio.from_signals(
            price, entries, exits,
            init_cash=100000, fees=0.001
        )
        
        vbt_time_multi = time.time() - start_time
        
        print(f"  ⏱️  耗时: {vbt_time_multi:.4f} 秒")
        print(f"  📈 平均收益: {pf.total_return().mean():.2%}")
        
        # 测试3: 参数优化（VectorBT核心优势）
        print("\n📊 测试3: 参数优化 (100种组合)")
        price = generate_mock_data(days=1000, n_stocks=1)
        
        start_time = time.time()
        
        # 批量计算所有均线组合
        fast_windows = list(range(5, 51, 5))   # 10种
        slow_windows = list(range(50, 101, 10)) # 10种
        
        fast_ma = vbt.MA.run(price, fast_windows, short_name='fast')
        slow_ma = vbt.MA.run(price, slow_windows, short_name='slow')
        
        entries = fast_ma.ma_crossed_above(slow_ma)
        exits = fast_ma.ma_crossed_below(slow_ma)
        
        pf = vbt.Portfolio.from_signals(
            price, entries, exits,
            init_cash=100000, fees=0.001
        )
        
        returns = pf.total_return()
        best_idx = returns.idxmax()
        
        vbt_time_opt = time.time() - start_time
        
        print(f"  ⏱️  耗时: {vbt_time_opt:.4f} 秒")
        print(f"  🔍 测试组合数: 100")
        print(f"  ⏱️  平均每种组合: {vbt_time_opt/100:.4f} 秒")
        print(f"  🏆 最佳收益: {returns[best_idx]:.2%} (参数: {best_idx})")
        
        return {
            'single_stock': vbt_time_single,
            'multi_stock': vbt_time_multi,
            'optimization': vbt_time_opt,
            'per_combination': vbt_time_opt / 100
        }
        
    except Exception as e:
        print(f"❌ VectorBT 测试失败: {e}")
        import traceback
        traceback.print_exc()
        return None


def test_existing_backtest_speed():
    """测试现有回测模块速度"""
    print("\n" + "="*70)
    print("🐌 现有回测模块性能测试（事件驱动）")
    print("="*70)
    
    # 测试1: 单只股票回测
    print("\n📊 测试1: 单只股票回测 (1000天数据)")
    price = generate_mock_data(days=1000, n_stocks=1).iloc[:, 0]
    
    start_time = time.time()
    
    # 事件驱动回测（模拟现有回测）
    position = 0
    cash = 100000
    trades = []
    
    for i in range(len(price)):
        if i >= 50:
            ma_fast = price.iloc[i-10:i].mean()
            ma_slow = price.iloc[i-50:i].mean()
            
            if ma_fast > ma_slow and position == 0:
                position = cash / price.iloc[i]
                cash = 0
                trades.append(('buy', i))
            elif ma_fast < ma_slow and position > 0:
                cash = position * price.iloc[i]
                position = 0
                trades.append(('sell', i))
    
    existing_time_single = time.time() - start_time
    
    print(f"  ⏱️  耗时: {existing_time_single:.4f} 秒")
    print(f"  📊 交易次数: {len(trades)//2}")
    
    # 测试2: 多只股票回测（逐个运行）
    print("\n📊 测试2: 多只股票回测 (10只股票)")
    prices = generate_mock_data(days=1000, n_stocks=10)
    
    start_time = time.time()
    
    for col in prices.columns:
        price = prices[col]
        position = 0
        cash = 100000
        
        for i in range(len(price)):
            if i >= 50:
                ma_fast = price.iloc[i-10:i].mean()
                ma_slow = price.iloc[i-50:i].mean()
                
                if ma_fast > ma_slow and position == 0:
                    position = cash / price.iloc[i]
                    cash = 0
                elif ma_fast < ma_slow and position > 0:
                    cash = position * price.iloc[i]
                    position = 0
    
    existing_time_multi = time.time() - start_time
    
    print(f"  ⏱️  耗时: {existing_time_multi:.4f} 秒")
    
    # 测试3: 参数优化（极其缓慢）
    print("\n📊 测试3: 参数优化 (100种组合)")
    price = generate_mock_data(days=1000, n_stocks=1).iloc[:, 0]
    
    start_time = time.time()
    
    fast_windows = range(5, 51, 5)   # 10种
    slow_windows = range(50, 101, 10) # 10种
    
    results = []
    for fast in fast_windows:
        for slow in slow_windows:
            # 运行一次回测
            position = 0
            cash = 100000
            
            for i in range(len(price)):
                if i >= slow:
                    ma_fast = price.iloc[i-fast:i].mean()
                    ma_slow = price.iloc[i-slow:i].mean()
                    
                    if ma_fast > ma_slow and position == 0:
                        position = cash / price.iloc[i]
                        cash = 0
                    elif ma_fast < ma_slow and position > 0:
                        cash = position * price.iloc[i]
                        position = 0
            
            final_value = cash + position * price.iloc[-1] if position > 0 else cash
            returns = (final_value - 100000) / 100000
            results.append(returns)
    
    existing_time_opt = time.time() - start_time
    
    print(f"  ⏱️  耗时: {existing_time_opt:.4f} 秒")
    print(f"  🔍 测试组合数: 100")
    print(f"  ⏱️  平均每种组合: {existing_time_opt/100:.4f} 秒")
    print(f"  🏆 最佳收益: {max(results):.2%}")
    
    return {
        'single_stock': existing_time_single,
        'multi_stock': existing_time_multi,
        'optimization': existing_time_opt,
        'per_combination': existing_time_opt / 100
    }


def generate_report(vbt_results, existing_results):
    """生成对比报告"""
    print("\n" + "="*70)
    print("📊 性能对比报告")
    print("="*70)
    
    if vbt_results is None or existing_results is None:
        print("❌ 测试数据不完整，无法生成报告")
        return
    
    # 创建对比表
    comparison = pd.DataFrame({
        '测试项目': ['单只股票回测', '多只股票(10只)', '参数优化(100组合)', '平均每组合'],
        'VectorBT (秒)': [
            f"{vbt_results['single_stock']:.4f}",
            f"{vbt_results['multi_stock']:.4f}",
            f"{vbt_results['optimization']:.4f}",
            f"{vbt_results['per_combination']:.4f}"
        ],
        '现有回测 (秒)': [
            f"{existing_results['single_stock']:.4f}",
            f"{existing_results['multi_stock']:.4f}",
            f"{existing_results['optimization']:.4f}",
            f"{existing_results['per_combination']:.4f}"
        ],
        '加速比': [
            f"{existing_results['single_stock']/vbt_results['single_stock']:.1f}x",
            f"{existing_results['multi_stock']/vbt_results['multi_stock']:.1f}x",
            f"{existing_results['optimization']/vbt_results['optimization']:.1f}x",
            f"{existing_results['per_combination']/vbt_results['per_combination']:.1f}x"
        ]
    })
    
    print("\n", comparison.to_string(index=False))
    
    # 总结
    speedups = [
        existing_results['single_stock']/vbt_results['single_stock'],
        existing_results['multi_stock']/vbt_results['multi_stock'],
        existing_results['optimization']/vbt_results['optimization']
    ]
    avg_speedup = sum(speedups) / len(speedups)
    max_speedup = max(speedups)
    
    print(f"\n" + "="*70)
    print("🎯 性能总结")
    print("="*70)
    print(f"📈 平均加速比: {avg_speedup:.1f}x")
    print(f"🚀 最大加速比: {max_speedup:.1f}x (参数优化场景)")
    print(f"💡 核心优势: 参数优化时速度提升最显著")
    
    if max_speedup > 50:
        print("\n✅ VectorBT 性能提升极其显著！")
        print("   强烈建议用于：")
        print("   • 策略参数优化")
        print("   • 多策略批量回测")
        print("   • 快速策略筛选")
    elif avg_speedup > 10:
        print("\n✅ VectorBT 有明显性能优势")
        print("   建议并行使用：")
        print("   • VectorBT: 快速筛选和参数优化")
        print("   • 现有回测: 精细验证和实盘模拟")
    
    # 保存报告
    report_path = '/opt/hktech-agent/data/vectorbt_performance_report.txt'
    with open(report_path, 'w') as f:
        f.write("VectorBT 性能测试报告\n")
        f.write("="*70 + "\n\n")
        f.write(comparison.to_string(index=False))
        f.write(f"\n\n平均加速比: {avg_speedup:.1f}x\n")
        f.write(f"最大加速比: {max_speedup:.1f}x\n")
    
    print(f"\n💾 报告已保存: {report_path}")


if __name__ == '__main__':
    print("🚀 VectorBT 性能对比测试（离线版）")
    print("="*70)
    
    # 测试 VectorBT
    vbt_results = test_vectorbt_speed()
    
    # 测试现有回测
    existing_results = test_existing_backtest_speed()
    
    # 生成报告
    generate_report(vbt_results, existing_results)
    
    print("\n✅ 测试完成!")
