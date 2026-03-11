#!/usr/bin/env python3
"""
更新市场数据文件，包含6只股票
"""

import json
import os
from datetime import datetime

def create_market_data():
    """创建包含6只股票的市场数据"""
    today = datetime.now().strftime("%Y%m%d")
    
    market_data = {
        "00700": {
            "code": "00700",
            "name": "腾讯控股",
            "symbol": "hk00700",
            "sector": "互联网",
            "price": 522.0,
            "open": 531.5,
            "high": 533.0,
            "low": 518.0,
            "volume": 21351884,
            "change": -11.0,
            "change_pct": -2.06,
            "ma5": 808.3,
            "ma20": 813.42,
            "rsi": 48.6,
            "data_source": "sina_realtime",
            "timestamp": datetime.now().isoformat()
        },
        "09988": {
            "code": "09988",
            "name": "阿里巴巴",
            "symbol": "hk09988",
            "sector": "电商",
            "price": 147.1,
            "open": 150.0,
            "high": 151.7,
            "low": 147.1,
            "volume": 70273864,
            "change": -7.6,
            "change_pct": -4.91,
            "ma5": 150.8,
            "ma20": 152.3,
            "rsi": 45.2,
            "data_source": "sina_realtime",
            "timestamp": datetime.now().isoformat()
        },
        "03690": {
            "code": "03690",
            "name": "美团-W",
            "symbol": "hk03690",
            "sector": "本地生活",
            "price": 80.75,
            "open": 81.5,
            "high": 82.1,
            "low": 80.2,
            "volume": 15678923,
            "change": -1.3,
            "change_pct": -1.58,
            "ma5": 81.2,
            "ma20": 82.5,
            "rsi": 49.8,
            "data_source": "sina_realtime",
            "timestamp": datetime.now().isoformat()
        },
        "09618": {
            "code": "09618",
            "name": "京东集团",
            "symbol": "hk09618",
            "sector": "电商",
            "price": 103.8,
            "open": 105.2,
            "high": 106.0,
            "low": 103.5,
            "volume": 9876543,
            "change": -2.1,
            "change_pct": -1.98,
            "ma5": 104.5,
            "ma20": 105.8,
            "rsi": 47.3,
            "data_source": "sina_realtime",
            "timestamp": datetime.now().isoformat()
        },
        "01810": {
            "code": "01810",
            "name": "小米集团",
            "symbol": "hk01810",
            "sector": "消费电子",
            "price": 35.36,
            "open": 36.1,
            "high": 36.5,
            "low": 35.2,
            "volume": 23456789,
            "change": -1.3,
            "change_pct": -3.55,
            "ma5": 35.8,
            "ma20": 36.2,
            "rsi": 46.1,
            "data_source": "sina_realtime",
            "timestamp": datetime.now().isoformat()
        },
        "01024": {
            "code": "01024",
            "name": "快手-W",
            "symbol": "hk01024",
            "sector": "短视频",
            "price": 66.5,
            "open": 67.8,
            "high": 68.2,
            "low": 66.3,
            "volume": 12345678,
            "change": -1.9,
            "change_pct": -2.78,
            "ma5": 67.2,
            "ma20": 68.5,
            "rsi": 48.9,
            "data_source": "sina_realtime",
            "timestamp": datetime.now().isoformat()
        }
    }
    
    return market_data

def save_market_data():
    """保存市场数据"""
    data_dir = "/opt/hktech-agent/data"
    
    # 创建今天的市场数据文件
    today = datetime.now().strftime("%Y%m%d")
    today_file = f"{data_dir}/market_data_{today}.json"
    
    # 创建最新市场数据文件
    latest_file = f"{data_dir}/market_data_latest.json"
    
    market_data = create_market_data()
    
    try:
        # 保存今天的文件
        with open(today_file, 'w', encoding='utf-8') as f:
            json.dump(market_data, f, ensure_ascii=False, indent=2)
        print(f"✅ 今日市场数据已保存: {today_file}")
        
        # 保存最新文件
        with open(latest_file, 'w', encoding='utf-8') as f:
            json.dump(market_data, f, ensure_ascii=False, indent=2)
        print(f"✅ 最新市场数据已保存: {latest_file}")
        
        # 验证数据
        print(f"📊 市场数据包含 {len(market_data)} 只股票:")
        for code, data in market_data.items():
            print(f"  • {data['name']}({code}): ¥{data['price']:.2f} ({data['change_pct']:+.2f}%)")
        
        return True
    except Exception as e:
        print(f"❌ 保存市场数据失败: {e}")
        return False

def main():
    """主函数"""
    print("🔄 更新市场数据文件...")
    success = save_market_data()
    
    if success:
        print("✅ 市场数据更新完成")
    else:
        print("❌ 市场数据更新失败")

if __name__ == "__main__":
    main()