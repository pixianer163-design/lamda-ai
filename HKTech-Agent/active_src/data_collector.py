#!/usr/bin/env python3
"""
数据采集模块 - 港股数据采集（雅虎财经版本）
免费、稳定、延迟15分钟（模拟交易足够）
"""

try:
    import yfinance as yf
    YFINANCE_AVAILABLE = True
except ImportError:
    yf = None
    YFINANCE_AVAILABLE = False
    print("⚠️ yfinance未安装，使用替代数据源")

try:
    import pandas as pd
    PANDAS_AVAILABLE = True
except ImportError:
    pd = None
    PANDAS_AVAILABLE = False
    print("⚠️ pandas未安装，使用简化数据处理")
import json
import sys
from datetime import datetime, timedelta
from typing import Dict, Optional
import os

# 导入共享常量
SHARED_CONSTANTS_AVAILABLE = False
constants = None  # 默认值
try:
    import constants
    SHARED_CONSTANTS_AVAILABLE = True
except ImportError:
    print("⚠️ 共享常量模块不可用，使用本地定义")
import time
import random
import requests
from requests.adapters import HTTPAdapter
from urllib.parse import urlencode


class HKStockDataCollector:
    """港股数据采集器 - 多源备份版本"""
    
    def __init__(self, data_dir=None):
        """初始化采集器"""
        # 股票信息（使用共享常量或本地定义）
        if SHARED_CONSTANTS_AVAILABLE and constants is not None:
            # 从共享常量获取股票信息
            self.stocks = {}
            for code, info in constants.STOCKS.items():
                self.stocks[code] = {
                    "name": info.get("name", code),
                    "yf_symbol": info.get("yf_symbol", f"{code[:4]}.HK"),
                    "sector": info.get("sector", "未知"),
                    "sina_symbol": info.get("sina_symbol", f"hk{code}")
                }
            # 限制为默认股票（如果需要）
            # 保持原有行为：只使用三只核心股票
            default_codes = constants.DEFAULT_STOCKS
            self.stocks = {code: self.stocks.get(code) for code in default_codes if code in self.stocks}
        else:
            self.stocks = {
                "00700": {"name": "腾讯控股", "yf_symbol": "0700.HK", "sector": "互联网", "sina_symbol": "hk00700"},
                "09988": {"name": "阿里巴巴", "yf_symbol": "9988.HK", "sector": "电商", "sina_symbol": "hk09988"},
                "03690": {"name": "美团", "yf_symbol": "3690.HK", "sector": "本地生活", "sina_symbol": "hk03690"}
            }
        # 设置数据目录
        if data_dir:
            self.data_dir = data_dir
        else:
            # 默认使用项目相对路径
            current_dir = os.path.dirname(os.path.abspath(__file__))
            self.data_dir = os.path.join(current_dir, '../data')
        
        os.makedirs(self.data_dir, exist_ok=True)
        
        # 配置请求session（带重试）
        self.session = requests.Session()
        self.session.headers.update({
            'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.0'
        })
        adapter = HTTPAdapter(max_retries=3)
        self.session.mount('http://', adapter)
        self.session.mount('https://', adapter)
    
    def get_daily_data(self, days: int = 30) -> Dict:
        """获取日线数据（带多源备份）"""
        data = {}
        
        for code, info in self.stocks.items():
            stock_data = None
            
            # 尝试1: Yahoo Finance
            try:
                print(f"📊 正在获取 {info['name']}({code}) 的数据...")
                stock_data = self._get_yahoo_data(code, info, days)
                if stock_data:
                    self._write_cache(code, stock_data)
                    data[code] = stock_data
                    print(f"✅ {info['name']}: ¥{stock_data['price']} ({stock_data.get('change_pct', 0):+.2f}%) [Yahoo]")
                    time.sleep(3)  # Yahoo限流更严格
                    continue
            except Exception as e:
                print(f"⚠️ Yahoo数据源失败: {e}")
            
            # 尝试2: 新浪财经
            try:
                print(f"🔄 尝试新浪财经...")
                stock_data = self._get_sina_data(code, info)
                if stock_data:
                    self._write_cache(code, stock_data)
                    data[code] = stock_data
                    print(f"✅ {info['name']}: ¥{stock_data['price']} ({stock_data.get('change_pct', 0):+.2f}%) [Sina]")
                    time.sleep(1)
                    continue
            except Exception as e:
                print(f"⚠️ 新浪数据源失败: {e}")
            
            # 尝试3: 磁盘缓存（12h TTL）
            cached = self._read_cache(code)
            if cached:
                data[code] = cached
                print(f"✅ {info['name']}: ¥{cached['price']} ({cached.get('change_pct', 0):+.2f}%) [Cache]")
                continue

            # 备用: 模拟数据
            print(f"⚠️ 使用备用模拟数据")
            data[code] = self._mock_data(code, info)
            print(f"✅ {info['name']}: ¥{data[code]['price']} ({data[code]['change_pct']:+.2f}%) [Mock]")
        
        # 保存到本地
        self._save_data(data)
        
        return data

    def _write_cache(self, code: str, data: dict):
        """将成功获取的股票数据写入磁盘缓存"""
        cache_dir = os.path.join(self.data_dir, "cache")
        os.makedirs(cache_dir, exist_ok=True)
        today = datetime.now().strftime("%Y%m%d")
        cache_path = os.path.join(cache_dir, f"{code}_{today}.json")
        try:
            with open(cache_path, "w", encoding="utf-8") as f:
                json.dump({**data, "_cached_at": datetime.now().isoformat()}, f, ensure_ascii=False)
        except Exception:
            pass  # 缓存写失败不影响主流程

    def _read_cache(self, code: str):
        """读取最新缓存文件（12h TTL），返回 dict 或 None"""
        from pathlib import Path
        cache_dir = os.path.join(self.data_dir, "cache")
        if not os.path.exists(cache_dir):
            return None
        files = sorted(Path(cache_dir).glob(f"{code}_*.json"), reverse=True)
        for cache_file in files[:1]:
            try:
                with open(cache_file, encoding="utf-8") as f:
                    data = json.load(f)
                cached_at_str = data.get("_cached_at")
                if cached_at_str:
                    cached_at = datetime.fromisoformat(cached_at_str)
                    if datetime.now() - cached_at > timedelta(hours=12):
                        print(f"⚠️ {code} 缓存已过期（>12h），但仍使用")
                data["data_source"] = "cache"
                return data
            except Exception:
                continue
        return None

    def _get_yahoo_data(self, code: str, info: dict, days: int) -> Optional[Dict]:
        """从Yahoo Finance获取数据"""
        max_retries = 3
        for attempt in range(max_retries):
            try:
                ticker = yf.Ticker(info["yf_symbol"])
                hist = ticker.history(period=f"{days}d")
                
                if hist is None or len(hist) == 0:
                    raise ValueError("无数据返回")
                
                # 计算技术指标
                hist = self._calculate_indicators(hist)
                
                # 获取最新数据
                latest = hist.iloc[-1]
                prev = hist.iloc[-2] if len(hist) > 1 else latest
                
                return {
                    "code": code,
                    "name": info["name"],
                    "symbol": info["yf_symbol"],
                    "sector": info["sector"],
                    "price": round(float(latest["Close"]), 2),
                    "open": round(float(latest["Open"]), 2),
                    "high": round(float(latest["High"]), 2),
                    "low": round(float(latest["Low"]), 2),
                    "volume": int(latest["Volume"]),
                    "change": round(float(latest["Close"] - prev["Close"]), 2),
                    "change_pct": round(float((latest["Close"] / prev["Close"] - 1) * 100), 2),
                    "ma5": round(float(latest["MA5"]), 2) if not pd.isna(latest["MA5"]) else None,
                    "ma20": round(float(latest["MA20"]), 2) if not pd.isna(latest["MA20"]) else None,
                    "ma60": round(float(latest["MA60"]), 2) if not pd.isna(latest["MA60"]) else None,
                    "rsi": round(float(latest["RSI"]), 2) if not pd.isna(latest["RSI"]) else None,
                    "trend": "UP" if latest.get("MA5", 0) > latest.get("MA20", 0) else "DOWN",
                    "updated_at": datetime.now().isoformat(),
                    "data_source": "yahoo_finance"
                }
                
            except Exception as e:
                if "Rate limited" in str(e) or "Too Many Requests" in str(e):
                    wait_time = (attempt + 1) * 5 + random.uniform(1, 3)
                    print(f"    ⏳ 限流，等待 {wait_time:.1f} 秒后重试...")
                    time.sleep(wait_time)
                else:
                    raise
        
        return None
    
    def _get_sina_data(self, code: str, info: dict) -> Optional[Dict]:
        """从新浪财经获取实时数据"""
        try:
            # 新浪财经港股API
            url = f"https://hq.sinajs.cn/list={info['sina_symbol']}"
            response = self.session.get(url, timeout=10)
            response.encoding = 'gb2312'
            
            # 解析返回数据
            text = response.text
            if not text or 'var hq_str_' not in text:
                return None
            
            # 提取数据部分
            data_str = text.split('"')[1]
            fields = data_str.split(',')
            
            if len(fields) < 10:
                return None
            
            # 新浪港股数据格式: 英文名,今日开盘价,昨日收盘价,最新价,最高价,最低价...
            name_en = fields[0]
            open_price = float(fields[2])
            prev_close = float(fields[3])
            current_price = float(fields[6])
            high = float(fields[4])
            low = float(fields[5])
            volume = int(float(fields[12])) if len(fields) > 12 else 0
            
            change = current_price - prev_close
            change_pct = (change / prev_close) * 100
            
            # 读取本地历史数据计算技术指标（如果没有则估算）
            ma5, ma20, rsi = self._calc_indicators_from_history(code, current_price)
            
            return {
                "code": code,
                "name": info["name"],
                "symbol": info["yf_symbol"],
                "sector": info["sector"],
                "price": round(current_price, 2),
                "open": round(open_price, 2),
                "high": round(high, 2),
                "low": round(low, 2),
                "volume": volume,
                "change": round(change, 2),
                "change_pct": round(change_pct, 2),
                "ma5": ma5,
                "ma20": ma20,
                "rsi": rsi,
                "trend": "UP" if ma5 and ma20 and ma5 > ma20 else "DOWN",
                "updated_at": datetime.now().isoformat(),
                "data_source": "sina_finance"
            }
            
        except Exception as e:
            print(f"    ❌ 新浪数据解析失败: {e}")
            return None
    
    def _calc_indicators_from_history(self, code: str, current_price: float):
        """从历史数据计算指标（备用）"""
        history_file = f"{self.data_dir}/market_data_{datetime.now().strftime('%Y%m%d')}.json"
        
        if os.path.exists(history_file):
            try:
                with open(history_file, 'r') as f:
                    data = json.load(f)
                    if code in data:
                        return data[code].get('ma5'), data[code].get('ma20'), data[code].get('rsi')
            except:
                pass
        
        # 估算值
        return round(current_price * 0.98, 2), round(current_price * 0.95, 2), 50.0
    
    def _calculate_indicators(self, df: pd.DataFrame) -> pd.DataFrame:
        """计算技术指标"""
        # 收盘价序列
        close = df["Close"]
        
        # 移动平均线
        df["MA5"] = close.rolling(window=5, min_periods=1).mean()
        df["MA20"] = close.rolling(window=20, min_periods=1).mean()
        df["MA60"] = close.rolling(window=60, min_periods=1).mean()
        
        # RSI
        df["RSI"] = self._calculate_rsi(close)
        
        # MACD
        exp1 = close.ewm(span=12, adjust=False).mean()
        exp2 = close.ewm(span=26, adjust=False).mean()
        df["MACD"] = exp1 - exp2
        df["MACD_Signal"] = df["MACD"].ewm(span=9, adjust=False).mean()
        
        return df
    
    def _calculate_rsi(self, prices: pd.Series, period: int = 14) -> pd.Series:
        """计算RSI指标"""
        delta = prices.diff()
        gain = delta.where(delta > 0, 0).rolling(window=period, min_periods=1).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=period, min_periods=1).mean()
        rs = gain / loss
        rsi = 100 - (100 / (1 + rs))
        return rsi
    
    def _save_data(self, data: Dict):
        """保存数据到本地"""
        filename = f"{self.data_dir}/market_data_{datetime.now().strftime('%Y%m%d')}.json"
        with open(filename, "w", encoding="utf-8") as f:
            json.dump(data, f, ensure_ascii=False, indent=2, default=str)
        print(f"💾 数据已保存到 {filename}")
    
    def _mock_data(self, code: str, info: dict) -> Dict:
        """生成模拟数据（备用）"""
        import random
        base_price = {"00700": 385.0, "09988": 85.0, "03690": 130.0}
        price = base_price.get(code, 100.0) + random.uniform(-5, 5)
        
        return {
            "code": code,
            "name": info["name"],
            "sector": info["sector"],
            "price": round(price, 2),
            "open": round(price * 0.99, 2),
            "high": round(price * 1.02, 2),
            "low": round(price * 0.98, 2),
            "volume": random.randint(1000000, 10000000),
            "change": round(random.uniform(-5, 5), 2),
            "change_pct": round(random.uniform(-2, 2), 2),
            "ma5": round(price * 0.98, 2),
            "ma20": round(price * 0.95, 2),
            "rsi": round(random.uniform(30, 70), 2),
            "trend": random.choice(["UP", "DOWN"]),
            "updated_at": datetime.now().isoformat(),
            "data_source": "mock"
        }


if __name__ == "__main__":
    # 测试数据采集
    collector = HKStockDataCollector()
    data = collector.get_daily_data(days=30)
    
    print("\n" + "="*50)
    print("数据采集完成")
    print("="*50)
    
    for code, stock_data in data.items():
        source = stock_data.get('data_source', 'unknown')
        print(f"\n{stock_data['name']} ({code}) [来源: {source}]:")
        print(f"  价格: {stock_data['price']}")
        print(f"  涨跌: {stock_data['change_pct']:+.2f}%")
        print(f"  MA5: {stock_data['ma5']}")
        print(f"  MA20: {stock_data['ma20']}")
        print(f"  RSI: {stock_data['rsi']}")
