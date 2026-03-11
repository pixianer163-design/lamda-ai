#!/usr/bin/env python3
"""
生成完整的日报数据
包含市场数据、持仓信息、策略表现等
"""

import json
import os
import sys
from datetime import datetime

# 添加src目录到Python路径
sys.path.insert(0, '/opt/hktech-agent/src')
from data_collector_sina import HKStockDataCollectorSina

def load_portfolio_data():
    """加载投资组合数据"""
    data_dir = "/opt/hktech-agent/data"
    
    data = {}
    
    try:
        with open(f"{data_dir}/portfolio.json", 'r', encoding='utf-8') as f:
            data["portfolio"] = json.load(f)
    except Exception as e:
        print(f"⚠️ 无法读取portfolio.json: {e}")
        data["portfolio"] = {
            "total_value": 0,
            "cash": 0,
            "holdings": {}
        }
    
    try:
        with open(f"{data_dir}/strategy_pool.json", 'r', encoding='utf-8') as f:
            data["strategy_pool"] = json.load(f)
    except Exception as e:
        print(f"⚠️ 无法读取strategy_pool.json: {e}")
        data["strategy_pool"] = {
            "strategies": [],
            "active_strategy": "base"
        }
    
    try:
        with open(f"{data_dir}/day_count.json", 'r', encoding='utf-8') as f:
            data["day_count"] = json.load(f)
    except Exception as e:
        print(f"⚠️ 无法读取day_count.json: {e}")
        data["day_count"] = {
            "day": 1,
            "total_trades": 0
        }
    
    return data

def get_market_data():
    """获取市场数据"""
    print("📈 正在获取市场数据...")
    collector = HKStockDataCollectorSina()
    
    # 获取所有跟踪股票的实时价格
    market_data = {}
    
    for code, info in collector.stocks.items():
        try:
            # 获取实时价格
            price_data = collector.get_daily_data(days=1)
            if code in price_data:
                price_info = price_data[code]
                market_data[code] = {
                    "name": info["name"],
                    "sector": info["sector"],
                    "price": price_info.get("close", 0),
                    "change": price_info.get("change", 0),
                    "change_percent": price_info.get("change_percent", 0),
                    "volume": price_info.get("volume", 0),
                    "timestamp": datetime.now().isoformat()
                }
                print(f"  ✅ {info['name']}({code}): ¥{price_info.get('close', 0):.2f}")
            else:
                # 使用模拟数据
                market_data[code] = {
                    "name": info["name"],
                    "sector": info["sector"],
                    "price": 100.0,
                    "change": 0,
                    "change_percent": 0,
                    "volume": 1000000,
                    "timestamp": datetime.now().isoformat(),
                    "note": "模拟数据"
                }
                print(f"  ⚠️ {info['name']}({code}): 使用模拟数据")
        except Exception as e:
            print(f"  ❌ {info['name']}({code}): 获取失败 - {e}")
            # 使用模拟数据
            market_data[code] = {
                "name": info["name"],
                "sector": info["sector"],
                "price": 100.0,
                "change": 0,
                "change_percent": 0,
                "volume": 1000000,
                "timestamp": datetime.now().isoformat(),
                "note": "模拟数据（获取失败）"
            }
    
    return market_data

def calculate_performance(portfolio, market_data):
    """计算策略表现"""
    total_value = portfolio.get("total_value", 0)
    cash = portfolio.get("cash", 0)
    holdings = portfolio.get("holdings", {})
    
    # 计算持仓市值
    holdings_value = 0
    for code, holding in holdings.items():
        if code in market_data:
            current_price = market_data[code]["price"]
            holdings_value += current_price * holding.get("quantity", 0)
    
    # 计算总市值
    total_market_value = cash + holdings_value
    
    # 计算盈亏
    initial_investment = 1000000  # 初始投资100万
    profit = total_market_value - initial_investment
    profit_percent = (profit / initial_investment) * 100 if initial_investment > 0 else 0
    
    return {
        "total_market_value": total_market_value,
        "cash_balance": cash,
        "holdings_value": holdings_value,
        "profit_amount": profit,
        "profit_percent": profit_percent,
        "initial_investment": initial_investment
    }

def generate_daily_report():
    """生成日报数据"""
    print(f"📊 生成完整日报数据: {datetime.now().isoformat()}")
    
    # 加载投资组合数据
    portfolio_data = load_portfolio_data()
    
    # 获取市场数据
    market_data = get_market_data()
    
    # 计算表现
    performance = calculate_performance(portfolio_data["portfolio"], market_data)
    
    # 构建日报数据
    daily_report = {
        "date": datetime.now().strftime("%Y-%m-%d"),
        "timestamp": datetime.now().isoformat(),
        "total_market_value": performance["total_market_value"],
        "cash_balance": performance["cash_balance"],
        "holdings_value": performance["holdings_value"],
        "profit_amount": performance["profit_amount"],
        "profit_percent": performance["profit_percent"],
        "initial_investment": performance["initial_investment"],
        "holdings": portfolio_data["portfolio"].get("holdings", {}),
        "market_data": market_data,
        "strategy_info": portfolio_data["strategy_pool"],
        "day_count": portfolio_data["day_count"],
        "portfolio_summary": {
            "total_value": portfolio_data["portfolio"].get("total_value", 0),
            "cash": portfolio_data["portfolio"].get("cash", 0),
            "holdings_count": len(portfolio_data["portfolio"].get("holdings", {}))
        }
    }
    
    # 保存日报数据
    report_file = "/opt/hktech-agent/data/daily_report.json"
    try:
        with open(report_file, 'w', encoding='utf-8') as f:
            json.dump(daily_report, f, ensure_ascii=False, indent=2)
        print(f"✅ 完整日报数据已保存到: {report_file}")
        
        # 同时保存到dashboard_data.json供Web面板使用
        dashboard_file = "/opt/hktech-agent/data/dashboard_data.json"
        with open(dashboard_file, 'w', encoding='utf-8') as f:
            json.dump(daily_report, f, ensure_ascii=False, indent=2)
        print(f"✅ Web面板数据已更新: {dashboard_file}")
        
        return True
    except Exception as e:
        print(f"❌ 保存日报数据失败: {e}")
        return False

def main():
    """主函数"""
    success = generate_daily_report()
    
    if success:
        print("✅ 完整日报数据生成完成")
        print("📤 推送将由OpenClaw负责执行")
    else:
        print("❌ 日报数据生成失败")
    
    print("✅ 恒生Agent数据生成流程完成")

if __name__ == "__main__":
    main()