#!/usr/bin/env python3
"""
恒生科技Agent - 共享常量模块
集中管理股票信息、配置路径、默认值等，消除代码重复
"""

import os
from typing import Dict, List, Any

# 项目根目录
PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

# ============================================================================
# 股票信息定义
# ============================================================================

# 完整的股票元数据映射
STOCKS: Dict[str, Dict[str, Any]] = {
    "00700": {
        "name": "腾讯控股",
        "name_en": "Tencent Holdings",
        "sector": "互联网",
        "industry": "互联网服务",
        "yf_symbol": "0700.HK",      # Yahoo Finance 代码
        "sina_symbol": "hk00700",    # 新浪财经 代码
        "exchange": "HKEX",
        "currency": "HKD",
        "market_cap_rank": 1,
        "description": "中国领先的互联网科技公司"
    },
    "09988": {
        "name": "阿里巴巴",
        "name_en": "Alibaba Group",
        "sector": "电商",
        "industry": "电子商务",
        "yf_symbol": "9988.HK",
        "sina_symbol": "hk09988",
        "exchange": "HKEX",
        "currency": "HKD",
        "market_cap_rank": 2,
        "description": "中国最大的电子商务平台"
    },
    "03690": {
        "name": "美团-W",
        "name_en": "Meituan",
        "sector": "本地生活",
        "industry": "生活服务",
        "yf_symbol": "3690.HK",
        "sina_symbol": "hk03690",
        "exchange": "HKEX",
        "currency": "HKD",
        "market_cap_rank": 3,
        "description": "中国领先的生活服务电子商务平台"
    },
    "01810": {
        "name": "小米集团-W",
        "name_en": "Xiaomi Corporation",
        "sector": "硬件",
        "industry": "消费电子",
        "yf_symbol": "1810.HK",
        "sina_symbol": "hk01810",
        "exchange": "HKEX",
        "currency": "HKD",
        "market_cap_rank": 4,
        "description": "中国领先的智能手机和IoT平台公司"
    },
    "09618": {
        "name": "京东集团-SW",
        "name_en": "JD.com",
        "sector": "电商",
        "industry": "电子商务",
        "yf_symbol": "9618.HK",
        "sina_symbol": "hk09618",
        "exchange": "HKEX",
        "currency": "HKD",
        "market_cap_rank": 5,
        "description": "中国领先的技术驱动型电商和零售基础设施服务商"
    },
    "09999": {
        "name": "网易-S",
        "name_en": "NetEase",
        "sector": "互联网",
        "industry": "网络游戏",
        "yf_symbol": "9999.HK",
        "sina_symbol": "hk09999",
        "exchange": "HKEX",
        "currency": "HKD",
        "market_cap_rank": 6,
        "description": "中国领先的互联网和在线游戏服务提供商"
    }
}

# 股票名称的简化映射（兼容现有代码）
STOCK_NAMES = {code: info["name"] for code, info in STOCKS.items()}

# 默认股票列表（核心三只）
DEFAULT_STOCKS = ["00700", "09988", "03690"]

# 扩展股票列表（全部可用股票）
ALL_STOCKS = list(STOCKS.keys())

# 股票代码转换函数
def get_yf_symbol(hk_code: str) -> str:
    """获取港股代码对应的Yahoo Finance代码"""
    return STOCKS.get(hk_code, {}).get("yf_symbol", f"{hk_code[:4]}.HK")

def get_sina_symbol(hk_code: str) -> str:
    """获取港股代码对应的新浪财经代码"""
    return STOCKS.get(hk_code, {}).get("sina_symbol", f"hk{hk_code}")

def get_stock_name(hk_code: str) -> str:
    """获取股票中文名称"""
    return STOCKS.get(hk_code, {}).get("name", hk_code)

# ============================================================================
# 路径配置
# ============================================================================

# 数据目录
DATA_DIR = os.path.join(PROJECT_ROOT, "data")

# 配置文件目录
CONFIG_DIR = os.path.join(PROJECT_ROOT, "config")
FACTORY_CONFIG_DIR = os.path.join(PROJECT_ROOT, "factory", "configs")
LOCAL_CONFIG_DIR = os.path.join(PROJECT_ROOT, "local_config")

# 生产代码目录
PROD_SRC_DIR = os.path.join(PROJECT_ROOT, "prod", "src")

# 活跃开发代码目录
ACTIVE_SRC_DIR = os.path.join(PROJECT_ROOT, "active_src")

# 日志目录
LOG_DIR = os.path.join(PROJECT_ROOT, "prod", "logs")

# 确保关键目录存在
for directory in [DATA_DIR, LOG_DIR, LOCAL_CONFIG_DIR]:
    os.makedirs(directory, exist_ok=True)

# ============================================================================
# 默认配置值
# ============================================================================

# 默认风险控制参数
DEFAULT_RISK_CONFIG = {
    "position_control": {
        "max_single_stock_weight": 0.40,
        "max_total_position": 0.80,
        "min_cash_ratio": 0.20,
        "max_positions": 5
    },
    "stop_loss_take_profit": {
        "stop_loss_pct": -0.08,
        "take_profit_pct": 0.15
    }
}

# 默认策略配置
DEFAULT_STRATEGY_CONFIG = {
    "type": "multi_factor",
    "factors": {
        "technical": 0.4,
        "fundamental": 0.3,
        "sentiment": 0.3
    }
}

# 默认LLM配置
DEFAULT_LLM_CONFIG = {
    "model": "deepseek-chat",
    "temperature": 0.7,
    "max_tokens": 2000
}

# 默认调度配置
DEFAULT_SCHEDULE_CONFIG = {
    "pre_market": "09:00",
    "midday": "12:30",
    "post_market": "16:30"
}

# ============================================================================
# 环境配置
# ============================================================================

# 环境变量名称
ENV_AGENT_ENV = "AGENT_ENV"
ENV_LOG_DIR = "AGENT_LOG_DIR"
ENV_DATA_DIR = "AGENT_DATA_DIR"
ENV_FEISHU_WEBHOOK = "FEISHU_WEBHOOK_URL"
ENV_DEEPSEEK_API_KEY = "DEEPSEEK_API_KEY"

# 环境类型
ENV_LOCAL = "local"
ENV_DEV = "dev"
ENV_PROD = "prod"

# ============================================================================
# 工具函数
# ============================================================================

def get_data_dir() -> str:
    """获取数据目录（优先使用环境变量）"""
    return os.environ.get(ENV_DATA_DIR, DATA_DIR)

def get_log_dir() -> str:
    """获取日志目录（优先使用环境变量）"""
    return os.environ.get(ENV_LOG_DIR, LOG_DIR)

def get_agent_env() -> str:
    """获取当前环境"""
    return os.environ.get(ENV_AGENT_ENV, ENV_LOCAL)

def is_production() -> bool:
    """检查是否为生产环境"""
    return get_agent_env() == ENV_PROD

def is_local() -> bool:
    """检查是否为本地环境"""
    return get_agent_env() == ENV_LOCAL

def get_stock_list(stock_codes: List[str] = None) -> List[str]:
    """获取股票列表，如果未指定则返回默认列表"""
    if stock_codes is None:
        return DEFAULT_STOCKS.copy()
    return stock_codes.copy()

def get_stock_info(hk_code: str, field: str = None) -> Any:
    """获取股票信息，可指定特定字段"""
    info = STOCKS.get(hk_code, {})
    if field is None:
        return info.copy()
    return info.get(field)

# ============================================================================
# 模块导入检查
# ============================================================================

if __name__ == "__main__":
    print("✅ 共享常量模块加载成功")
    print(f"项目根目录: {PROJECT_ROOT}")
    print(f"默认股票: {DEFAULT_STOCKS}")
    print(f"总股票数: {len(ALL_STOCKS)}")
    print(f"数据目录: {get_data_dir()}")
    print(f"当前环境: {get_agent_env()}")
    print(f"是否为生产环境: {is_production()}")
    
    # 测试股票信息获取
    for code in DEFAULT_STOCKS:
        name = get_stock_name(code)
        yf_symbol = get_yf_symbol(code)
        print(f"{code}: {name} (YF: {yf_symbol})")