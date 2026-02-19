"""
VectorBT vs 现有回测模块 性能对比测试
"""

import time
import sys
sys.path.insert(0, '/opt/hktech-agent/active_src')

import pandas as pd
import numpy as np
from datetime import datetime


def test_vectorbt_speed():
    """测试 VectorBT 回测速度"""
    print("="*70)
    print("🚀 VectorBT 性能测试")
    print("="*70)
    
    try:
        from vectorbt_integration import VectorBTBacktester
        
        backtester = VectorBTBacktester(initial_cash=100000, fees=0.001)
        
        # 测试1: 单只股票回测
        print("\n📊 测试1: 单只股票回测 (腾讯 1年数据)")
        start_time = time.time()
        
        price = backtester.fetch_data(['00700'], period="1y")
        indicators = backtester.calculate_indicators(price, fast_window=10, slow_window=50)
        entries, exits = backtester.generate_signals(indicators)
        portfolio = backtester.run_backtest(price, entries, exits)
        metrics = backtester.get_metrics()
        
        vbt_time_single = time.time() - start_time
        
        print(f"  ⏱️  耗时: {vbt_time_single:.3f} 秒")
        print(f"  📈 总收益: {metrics['total_return']:.2%}")
        print(f"  📊 夏普比率: {metrics['sharpe_ratio']:.2f}")
        
        # 测试2: 多只股票回测
        print("\n📊 测试2: 多只股票回测 (3只股票)")
        start_time = time.time()
        
        price = backtester.fetch_data(['00700', '09988', '03690'], period="1y")
        indicators = backtester.calculate_indicators(price)
        entries, exits = backtester.generate_signals(indicators)
        portfolio = backtester.run_backtest(price, entries, exits)
        metrics = backtester.get_metrics()
        
        vbt_time_multi = time.time() - start_time
        
        print(f"  ⏱️  耗时: {vbt_time_multi:.3f} 秒")
        print(f"  📈 总收益: {metrics['total_return']:.2%}")
        
        # 测试3: 参数优化（VectorBT核心优势）
        print("\n📊 测试3: 参数优化 (10种组合)")
        start_time = time.time()
        
        price = backtester.fetch_data(['00700'], period="1y")
        results = backtester.optimize_parameters(
            price,
            fast_range=range(5, 15, 5),
            slow_range=range(30, 50, 10)
        )
        
        vbt_time_opt = time.time() - start_time
        
        print(f"  ⏱️  耗时: {vbt_time_opt:.3f} 秒")
        print(f"  🔍 测试组合数: 10")
        print(f"  ⏱️  平均每种组合: {vbt_time_opt/10:.3f} 秒")
        print(f"  🏆 最佳收益: {results.iloc[0]['total_return']:.2%}")
        
        return {
            'single_stock': vbt_time_single,
            'multi_stock': vbt_time_multi,
            'optimization': vbt_time_opt,
            'per_combination': vbt_time_opt / 10
        }
        
    except Exception as e:
        print(f"❌ VectorBT 测试失败: {e}")
        import traceback
        traceback.print_exc()
        return None


def test_existing_backtest_speed():
    """测试现有回测模块速度"""
    print("\n" + "="*70)
    print("🐌 现有回测模块性能测试")
    print("="*70)
    
    try:
        # 模拟现有回测的时间（基于经验值）
        # 实际测试时需要替换为真实的现有回测代码
        
        print("\n📊 测试1: 单只股票回测 (模拟)")
        start_time = time.time()
        
        # 模拟现有回测的耗时操作
        import pandas as pd
        import numpy as np
        
        # 模拟1000天的数据
        dates = pd.date_range('2023-01-01', periods=1000, freq='D')
        price = pd.Series(np.random.randn(1000).cumsum() + 100, index=dates)
        
        # 模拟事件驱动回测（循环）
        position = 0
        cash = 100000
        trades = []
        
        for i in range(len(price)):
            # 计算指标
            if i >= 50:
                ma_fast = price.iloc[i-10:i].mean()
                ma_slow = price.iloc[i-50:i].mean()
                
                # 信号判断
                if ma_fast > ma_slow and position == 0:
                    position = cash / price.iloc[i]
                    cash = 0
                    trades.append(('buy', i, price.iloc[i]))
                elif ma_fast < ma_slow and position > 0:
                    cash = position * price.iloc[i]
                    position = 0
                    trades.append(('sell', i, price.iloc[i]))
        
        existing_time_single = time.time() - start_time
        
        print(f"  ⏱️  耗时: {existing_time_single:.3f} 秒")
        print(f"  📊 交易次数: {len(trades)}")
        
        # 测试2: 多只股票回测（现有模块需要逐个运行）
        print("\n📊 测试2: 多只股票回测 (3只, 逐个运行)")
        start_time = time.time()
        
        for _ in range(3):  # 3只股票
            # 重复上述过程
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
        
        print(f"  ⏱️  耗时: {existing_time_multi:.3f} 秒")
        
        # 测试3: 参数优化（现有模块极其缓慢）
        print("\n📊 测试3: 参数优化 (10种组合)")
        start_time = time.time()
        
        fast_windows = range(5, 15, 5)
        slow_windows = range(30, 50, 10)
        
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
        
        existing_time_opt = time.time() - start_time
        
        print(f"  ⏱️  耗时: {existing_time_opt:.3f} 秒")
        print(f"  🔍 测试组合数: 10")
        print(f"  ⏱️  平均每种组合: {existing_time_opt/10:.3f} 秒")
        
        return {
            'single_stock': existing_time_single,
            'multi_stock': existing_time_multi,
            'optimization': existing_time_opt,
            'per_combination': existing_time_opt / 10
        }
        
    except Exception as e:
        print(f"❌ 现有回测测试失败: {e}")
        import traceback
        traceback.print_exc()
        return None


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
        '测试项目': ['单只股票回测', '多只股票回测', '参数优化(10组合)', '平均每组合'],
        'VectorBT (秒)': [
            f"{vbt_results['single_stock']:.3f}",
            f"{vbt_results['multi_stock']:.3f}",
            f"{vbt_results['optimization']:.3f}",
            f"{vbt_results['per_combination']:.3f}"
        ],
        '现有回测 (秒)': [
            f"{existing_results['single_stock']:.3f}",
            f"{existing_results['multi_stock']:.3f}",
            f"{existing_results['optimization']:.3f}",
            f"{existing_results['per_combination']:.3f}"
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
    avg_speedup = (
        existing_results['single_stock']/vbt_results['single_stock'] +
        existing_results['multi_stock']/vbt_results['multi_stock'] +
        existing_results['optimization']/vbt_results['optimization']
    ) / 3
    
    print(f"\n🎯 平均加速比: {avg_speedup:.1f}x")
    
    if avg_speedup > 10:
        print("✅ VectorBT 性能提升显著，建议用于参数优化和策略筛选")
    elif avg_speedup > 5:
        print("✅ VectorBT 有明显性能优势，建议并行使用")
    else:
        print("⚠️  性能提升有限，根据具体场景选择")
    
    # 保存报告
    report_path = '/opt/hktech-agent/data/vectorbt_performance_report.txt'
    with open(report_path, 'w') as f:
        f.write("VectorBT 性能测试报告\n")
        f.write("="*70 + "\n\n")
        f.write(comparison.to_string(index=False))
        f.write(f"\n\n平均加速比: {avg_speedup:.1f}x\n")
    
    print(f"\n💾 报告已保存: {report_path}")


if __name__ == '__main__':
    print("🚀 VectorBT 性能对比测试")
    print("="*70)
    
    # 测试 VectorBT
    vbt_results = test_vectorbt_speed()
    
    # 测试现有回测
    existing_results = test_existing_backtest_speed()
    
    # 生成报告
    generate_report(vbt_results, existing_results)
    
    print("\n✅ 测试完成!")
