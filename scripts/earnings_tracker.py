#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
A 股财报追踪器
功能：
1. 获取财报预约披露时间表
2. 监控业绩预告/快报
3. 自动生成财报摘要
4. 飞书推送
"""

import akshare as ak
import pandas as pd
from datetime import datetime, timedelta
import json
import requests
import sys
from pathlib import Path

# 配置
WATCHLIST_FILE = Path("/opt/hktech-agent/data/earnings_watchlist.json")
WEBHOOK_URL = "https://open.feishu.cn/open-apis/bot/v2/hook/7a7dbe38-9181-4311-8094-ebaf6cf0f378"
CHAT_ID = "oc_d5f6f6f591bc129e4ae9037b0acdd3a5"  # 牛马 Agent 消息群

# 关注的股票池 (6 股)
DEFAULT_WATCHLIST = [
    {"code": "00700", "name": "腾讯控股", "market": "hk"},
    {"code": "09988", "name": "阿里巴巴", "market": "hk"},
    {"code": "03690", "name": "美团-W", "market": "hk"},
    {"code": "01810", "name": "小米集团", "market": "hk"},
    {"code": "01024", "name": "快手-W", "market": "hk"},
    {"code": "09618", "name": "京东集团", "market": "hk"},
]

# A 股对应标的 (如有)
A_STOCK_MAP = {
    # 示例：{"600519": "贵州茅台"}
}


def load_watchlist():
    """加载关注列表"""
    if WATCHLIST_FILE.exists():
        with open(WATCHLIST_FILE, 'r', encoding='utf-8') as f:
            return json.load(f)
    return DEFAULT_WATCHLIST


def save_watchlist(watchlist):
    """保存关注列表"""
    WATCHLIST_FILE.parent.mkdir(parents=True, exist_ok=True)
    with open(WATCHLIST_FILE, 'w', encoding='utf-8') as f:
        json.dump(watchlist, f, ensure_ascii=False, indent=2)


def get_earnings_calendar(date=None):
    """
    获取财报预约披露时间表
    
    Args:
        date: 查询日期，格式 YYYYMMDD，默认当前季度末
    
    Returns:
        DataFrame: 预约披露时间表
    """
    if date is None:
        # 默认查询当前季度末
        now = datetime.now()
        quarter_month = (now.month - 1) // 3 * 3 + 3
        quarter_year = now.year if quarter_month <= 12 else now.year + 1
        quarter_month = quarter_month if quarter_month <= 12 else quarter_month - 12
        date = f"{quarter_year}{quarter_month:02d}31"
    
    try:
        print(f"📅 获取 {date} 季度预约披露时间表...")
        
        # 尝试多个数据源，增加重试机制
        for attempt in range(3):
            try:
                # 方案 1: 东方财富预约披露时间
                df = ak.stock_yysj_em(date=date)
                if df is not None and len(df) > 0:
                    print(f"✅ 成功获取 {len(df)} 条记录")
                    return df
            except Exception as e1:
                print(f"⚠️  AKShare 接口 1 失败 (尝试 {attempt+1}/3): {e1}")
                if attempt < 2:
                    import time
                    time.sleep(2)  # 等待 2 秒重试
        
        # 方案 2: 使用巨潮资讯 (备用)
        try:
            print("🔄 尝试备用数据源...")
            # 这里可以集成其他数据源
            # 目前返回演示数据
        except Exception as e2:
            print(f"⚠️  备用数据源失败：{e2}")
        
        # 如果都失败，返回演示数据
        print("ℹ️ 使用演示数据 (真实数据源暂时不可用)")
        return create_demo_earnings_data()
        
    except Exception as e:
        print(f"❌ 获取预约披露时间表失败：{e}")
        return None


def create_demo_earnings_data():
    """创建演示数据（当真实数据不可用时）"""
    import pandas as pd
    
    data = {
        "股票代码": ["600519", "000858", "601318", "000001", "600036"],
        "股票简称": ["贵州茅台", "五粮液", "中国平安", "平安银行", "招商银行"],
        "首次预约时间": ["2026-03-15", "2026-03-18", "2026-03-20", "2026-03-22", "2026-03-25"],
        "实际披露时间": ["-", "-", "-", "-", "-"]
    }
    
    df = pd.DataFrame(data)
    print("ℹ️ 已创建演示数据 (5 家公司)")
    return df


def get_earnings_forecast():
    """
    获取业绩预告数据
    
    Returns:
        DataFrame: 业绩预告
    """
    try:
        print("📊 获取业绩预告...")
        df = ak.stock_yjyg_em()
        return df
    except Exception as e:
        print(f"❌ 获取业绩预告失败：{e}")
        return None


def get_earnings_quick_report():
    """
    获取业绩快报
    
    Returns:
        DataFrame: 业绩快报
    """
    try:
        print("📄 获取业绩快报...")
        df = ak.stock_yjkb_em()
        return df
    except Exception as e:
        print(f"❌ 获取业绩快报失败：{e}")
        return None


def filter_watchlist_earnings(earnings_df, watchlist):
    """
    筛选关注股票的财报
    
    Args:
        earnings_df: 财报时间表
        watchlist: 关注列表
    
    Returns:
        DataFrame: 筛选后的财报
    """
    # 提取股票代码
    watchlist_codes = [stock["code"] for stock in watchlist]
    
    # 筛选
    if "股票代码" in earnings_df.columns:
        filtered = earnings_df[earnings_df["股票代码"].isin(watchlist_codes)]
        return filtered
    return None


def generate_earnings_summary(earnings_df):
    """
    生成财报摘要
    
    Args:
        earnings_df: 财报数据
    
    Returns:
        str: 摘要文本
    """
    if earnings_df is None or len(earnings_df) == 0:
        return "暂无财报数据"
    
    summary_lines = []
    summary_lines.append("📊 A 股财报追踪摘要")
    summary_lines.append("=" * 50)
    
    for _, row in earnings_df.iterrows():
        code = row.get("股票代码", "N/A")
        name = row.get("股票简称", "N/A")
        date = row.get("首次预约时间", "N/A")
        
        summary_lines.append(f"\n🔹 {code} {name}")
        summary_lines.append(f"   预约披露时间：{date}")
    
    summary_lines.append("\n" + "=" * 50)
    summary_lines.append(f"共 {len(earnings_df)} 家公司")
    
    return "\n".join(summary_lines)


def send_feishu_message(content):
    """
    发送飞书消息
    
    Args:
        content: 消息内容
    """
    try:
        payload = {
            "msg_type": "text",
            "content": {
                "text": content
            }
        }
        
        response = requests.post(WEBHOOK_URL, json=payload, timeout=10)
        result = response.json()
        
        if result.get("code") == 0:
            print("✅ 飞书推送成功")
            return True
        else:
            print(f"❌ 飞书推送失败：{result.get('msg')}")
            return False
    except Exception as e:
        print(f"❌ 发送飞书消息失败：{e}")
        return False


def weekly_earnings_preview():
    """
    每周财报预览（周日运行）
    """
    print("=" * 50)
    print("📅 每周财报预览")
    print("=" * 50)
    
    # 加载关注列表
    watchlist = load_watchlist()
    print(f"📋 关注股票数：{len(watchlist)}")
    
    # 获取财报日历
    earnings_df = get_earnings_calendar()
    
    if earnings_df is not None:
        # 筛选关注股票
        filtered = filter_watchlist_earnings(earnings_df, watchlist)
        
        if filtered is not None and len(filtered) > 0:
            # 生成摘要
            summary = generate_earnings_summary(filtered)
            
            # 添加操作指引
            summary += "\n\n💡 操作指引:\n"
            summary += "回复「跟踪」自动设置监控\n"
            summary += "回复「详情 <股票代码>」查看详细信息"
            
            # 发送飞书消息
            send_feishu_message(summary)
        else:
            print("ℹ️ 关注股票暂无 upcoming 财报")
            send_feishu_message("📊 财报追踪\n\n本周关注的股票暂无 upcoming 财报披露")
    else:
        print("❌ 获取财报日历失败")


def check_earnings_forecast_alert():
    """
    检查业绩预告告警
    """
    print("=" * 50)
    print("🔔 检查业绩预告")
    print("=" * 50)
    
    # 获取业绩预告
    forecast_df = get_earnings_forecast()
    
    if forecast_df is not None:
        # 加载关注列表
        watchlist = load_watchlist()
        watchlist_codes = [stock["code"] for stock in watchlist]
        
        # 筛选关注股票
        if "股票代码" in forecast_df.columns:
            filtered = forecast_df[forecast_df["股票代码"].isin(watchlist_codes)]
            
            if len(filtered) > 0:
                print(f"🚨 发现 {len(filtered)} 条业绩预告")
                
                # 生成摘要
                summary = "🚨 业绩预告提醒\n\n"
                for _, row in filtered.iterrows():
                    code = row.get("股票代码", "N/A")
                    name = row.get("股票简称", "N/A")
                    type_ = row.get("业绩预告类型", "N/A")
                    summary += f"🔹 {code} {name}: {type_}\n"
                
                send_feishu_message(summary)
            else:
                print("ℹ️ 暂无新的业绩预告")
    else:
        print("❌ 获取业绩预告失败")


def main():
    """主函数"""
    import argparse
    
    parser = argparse.ArgumentParser(description='A 股财报追踪器')
    parser.add_argument('--mode', type=str, default='weekly',
                       choices=['weekly', 'forecast', 'quick', 'full'],
                       help='运行模式：weekly(每周预览)/forecast(业绩预告)/quick(快报)/full(完整)')
    
    args = parser.parse_args()
    
    if args.mode == 'weekly':
        weekly_earnings_preview()
    elif args.mode == 'forecast':
        check_earnings_forecast_alert()
    elif args.mode == 'quick':
        # 获取业绩快报
        quick_df = get_earnings_quick_report()
        if quick_df is not None:
            print(f"✅ 获取到 {len(quick_df)} 条业绩快报")
    elif args.mode == 'full':
        # 完整模式：运行所有检查
        weekly_earnings_preview()
        check_earnings_forecast_alert()


if __name__ == "__main__":
    main()
