#!/usr/bin/env python3
"""
数据采集模块 - 港股数据采集（优化版）
改进点：
1. 修复死代码和异常处理问题
2. 添加数据验证
3. 提取配置到外部
4. 使用日志替代print
5. 优化代码结构
"""

import json
import logging
import os
import random
import time
from datetime import datetime
from typing import Dict, List, Optional, Tuple
from enum import Enum

import pandas as pd
import requests
import yfinance as yf
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry


class CircuitState(Enum):
    CLOSED = "closed"
    OPEN = "open"
    HALF_OPEN = "half_open"


class CircuitBreaker:
    """熔断器 - 防止级联故障"""
    
    def __init__(self, failure_threshold: int = 5, recovery_timeout: int = 60):
        self.failure_threshold = failure_threshold
        self.recovery_timeout = recovery_timeout
        self.failure_count = 0
        self.last_failure_time = None
        self.state = CircuitState.CLOSED
    
    def call(self, func, *args, **kwargs):
        """带熔断的函数调用"""
        if self.state == CircuitState.OPEN:
            if time.time() - self.last_failure_time >= self.recovery_timeout:
                self.state = CircuitState.HALF_OPEN
                logger.info("🔄 熔断器进入半开状态")
            else:
                raise Exception(f"Circuit breaker OPEN, retry after {self.recovery_timeout}s")
        
        try:
            result = func(*args, **kwargs)
            self._on_success()
            return result
        except Exception as e:
            self._on_failure()
            raise
    
    def _on_success(self):
        self.failure_count = 0
        if self.state == CircuitState.HALF_OPEN:
            self.state = CircuitState.CLOSED
            logger.info("✅ 熔断器已关闭")
    
    def _on_failure(self):
        self.failure_count += 1
        self.last_failure_time = time.time()
        if self.failure_count >= self.failure_threshold:
            self.state = CircuitState.OPEN
            logger.warning(f"⚠️ 熔断器已打开 (失败{self.failure_count}次)")

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class DataSourceConfig:
    """数据源配置"""
    
    STOCKS = {
        "00700": {
            "name": "腾讯控股",
            "yf_symbol": "0700.HK",
            "sector": "互联网",
            "sina_symbol": "hk00700",
            "base_price": 385.0
        },
        "09988": {
            "name": "阿里巴巴",
            "yf_symbol": "9988.HK",
            "sector": "电商",
            "sina_symbol": "hk09988",
            "base_price": 85.0
        },
        "03690": {
            "name": "美团",
            "yf_symbol": "3690.HK",
            "sector": "本地生活",
            "sina_symbol": "hk03690",
            "base_price": 130.0
        }
    }
    
    # API配置
    YAHOO_MAX_RETRIES = 3
    YAHOO_RETRY_DELAY = 5
    SINA_TIMEOUT = 10
    
    # 限流配置
    YAHOO_RATE_LIMIT_DELAY = 3
    SINA_RATE_LIMIT_DELAY = 1


class DataValidationError(Exception):
    """数据验证错误"""
    pass


class HKStockDataCollector:
    """港股数据采集器 - 优化版本"""
    
    def __init__(self, data_dir: str = "/opt/hktech-agent/data"):
        """初始化采集器"""
        self.config = DataSourceConfig()
        self.data_dir = data_dir
        os.makedirs(self.data_dir, exist_ok=True)
        
        # 配置请求session（带重试）
        self.session = self._create_session()
        
        # 熔断器
        self.circuit_breaker = CircuitBreaker(failure_threshold=3, recovery_timeout=30)
        
        logger.info("✅ 数据采集器初始化完成")
    
    def _create_session(self) -> requests.Session:
        """创建配置好的HTTP Session"""
        session = requests.Session()
        
        # 随机User-Agent
        user_agents = [
            'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36',
            'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36',
            'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36'
        ]
        session.headers.update({
            'User-Agent': random.choice(user_agents)
        })
        
        # 配置重试策略
        retry_strategy = Retry(
            total=3,
            backoff_factor=1,
            status_forcelist=[429, 500, 502, 503, 504]
        )
        adapter = HTTPAdapter(max_retries=retry_strategy)
        session.mount('http://', adapter)
        session.mount('https://', adapter)
        
        return session
    
    def get_daily_data(self, days: int = 30, max_retries: int = 3) -> Dict:
        """
        获取日线数据（带多源备份 + 熔断 + 重试）
        
        Args:
            days: 历史数据天数
            max_retries: 最大重试次数
            
        Returns:
            股票数据字典
        """
        data = {}
        
        for code, info in self.config.STOCKS.items():
            retry_count = 0
            last_error = None
            
            while retry_count < max_retries:
                try:
                    logger.info(f"📊 正在获取 {info['name']}({code}) 的数据... (尝试 {retry_count + 1}/{max_retries})")
                    stock_data = self.circuit_breaker.call(self._fetch_stock_data, code, info, days)
                    
                    if stock_data:
                        # 验证数据完整性
                        self._validate_stock_data(stock_data)
                        data[code] = stock_data
                        logger.info(
                            f"✅ {info['name']}: ¥{stock_data['price']} "
                            f"({stock_data['change_pct']:+.2f}%) "
                            f"[{stock_data['data_source']}]"
                        )
                        break  # 成功，跳出重试循环
                    else:
                        raise DataValidationError("未能获取有效数据")
                        
                except Exception as e:
                    last_error = e
                    retry_count += 1
                    if retry_count < max_retries:
                        wait_time = 2 ** retry_count  # 指数退避
                        logger.warning(f"⚠️ 获取 {code} 数据失败: {e}, {wait_time}秒后重试...")
                        time.sleep(wait_time)
                    else:
                        logger.error(f"❌ 获取 {code} 数据失败 (已重试{max_retries}次): {e}")
                        # 使用模拟数据作为fallback
                        data[code] = self._mock_data(code, info)
        
        # 保存到本地
        self._save_data(data)
        
        return data
    
    def _fetch_stock_data(self, code: str, info: dict, days: int) -> Optional[Dict]:
        """
        获取单只股票数据（多源策略）
        
        优先级：Yahoo Finance -> 新浪财经 -> 模拟数据
        """
        # 尝试1: Yahoo Finance
        try:
            data = self._get_yahoo_data(code, info, days)
            if data:
                return data
        except (requests.RequestException, Exception) as e:
            logger.warning(f"Yahoo数据源失败: {e}")
        
        time.sleep(self.config.YAHOO_RATE_LIMIT_DELAY)
        
        # 尝试2: 新浪财经
        try:
            data = self._get_sina_data(code, info)
            if data:
                return data
        except (requests.RequestException, ValueError, KeyError) as e:
            logger.warning(f"新浪数据源失败: {e}")
        
        time.sleep(self.config.SINA_RATE_LIMIT_DELAY)
        
        # 备用: 模拟数据
        logger.warning(f"使用备用模拟数据: {code}")
        return self._mock_data(code, info)
    
    def _get_yahoo_data(self, code: str, info: dict, days: int) -> Optional[Dict]:
        """从Yahoo Finance获取数据"""
        for attempt in range(self.config.YAHOO_MAX_RETRIES):
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
                    "price": self._safe_round(latest["Close"]),
                    "open": self._safe_round(latest["Open"]),
                    "high": self._safe_round(latest["High"]),
                    "low": self._safe_round(latest["Low"]),
                    "volume": int(latest["Volume"]),
                    "change": self._safe_round(latest["Close"] - prev["Close"]),
                    "change_pct": self._safe_round(
                        (latest["Close"] / prev["Close"] - 1) * 100
                    ),
                    "ma5": self._safe_round(latest.get("MA5")),
                    "ma20": self._safe_round(latest.get("MA20")),
                    "ma60": self._safe_round(latest.get("MA60")),
                    "rsi": self._safe_round(latest.get("RSI")),
                    "trend": "UP" if latest.get("MA5", 0) > latest.get("MA20", 0) else "DOWN",
                    "updated_at": datetime.now().isoformat(),
                    "data_source": "yahoo_finance"
                }
                
            except (requests.RequestException, Exception) as e:
                if "Rate limited" in str(e) or "Too Many Requests" in str(e):
                    wait_time = (attempt + 1) * self.config.YAHOO_RETRY_DELAY
                    logger.info(f"⏳ 限流，等待 {wait_time} 秒后重试...")
                    time.sleep(wait_time)
                else:
                    raise
        
        return None
    
    def _get_sina_data(self, code: str, info: dict) -> Optional[Dict]:
        """从新浪财经获取实时数据"""
        url = f"https://hq.sinajs.cn/list={info['sina_symbol']}"
        
        try:
            response = self.session.get(url, timeout=self.config.SINA_TIMEOUT)
            response.encoding = 'gb2312'
            
            # 验证响应
            if response.status_code != 200:
                raise requests.RequestException(f"HTTP {response.status_code}")
            
            text = response.text
            if not text or 'var hq_str_' not in text:
                raise ValueError("响应格式错误")
            
            # 解析数据
            data_parts = text.split('"')
            if len(data_parts) < 2:
                raise ValueError("数据解析失败")
            
            fields = data_parts[1].split(',')
            if len(fields) < 10:
                raise ValueError(f"字段不足: {len(fields)}")
            
            # 提取字段
            open_price = float(fields[2])
            prev_close = float(fields[3])
            current_price = float(fields[6])
            high = float(fields[4])
            low = float(fields[5])
            volume = int(float(fields[12])) if len(fields) > 12 else 0
            
            change = current_price - prev_close
            change_pct = (change / prev_close) * 100 if prev_close != 0 else 0
            
            # 计算指标
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
            logger.error(f"新浪数据获取失败: {e}")
            return None
    
    def _calc_indicators_from_history(
        self, code: str, current_price: float
    ) -> Tuple[Optional[float], Optional[float], Optional[float]]:
        """从历史数据计算指标（备用）"""
        history_file = f"{self.data_dir}/market_data_{datetime.now().strftime('%Y%m%d')}.json"
        
        if os.path.exists(history_file):
            try:
                with open(history_file, 'r', encoding='utf-8') as f:
                    data = json.load(f)
                    if code in data and isinstance(data[code], dict):
                        stock_data = data[code]
                        return (
                            stock_data.get('ma5'),
                            stock_data.get('ma20'),
                            stock_data.get('rsi')
                        )
            except (json.JSONDecodeError, IOError, KeyError) as e:
                logger.warning(f"读取历史数据失败: {e}")
        
        # 估算值（基于当前价格）
        return (
            round(current_price * 0.98, 2),
            round(current_price * 0.95, 2),
            50.0
        )
    
    def _calculate_indicators(self, df: pd.DataFrame) -> pd.DataFrame:
        """计算技术指标"""
        close = df["Close"]
        
        # 移动平均线
        df["MA5"] = close.rolling(window=5, min_periods=1).mean()
        df["MA20"] = close.rolling(window=20, min_periods=1).mean()
        df["MA60"] = close.rolling(window=60, min_periods=1).mean()
        
        # RSI
        df["RSI"] = self._calculate_rsi(close)
        
        return df
    
    def _calculate_rsi(self, prices: pd.Series, period: int = 14) -> pd.Series:
        """计算RSI指标（带除零保护）"""
        delta = prices.diff()
        gain = delta.where(delta > 0, 0).rolling(window=period, min_periods=1).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=period, min_periods=1).mean()
        
        # 避免除零
        rs = gain / loss.replace(0, 1e-10)
        rsi = 100 - (100 / (1 + rs))
        return rsi
    
    def _validate_stock_data(self, data: Dict) -> None:
        """验证股票数据完整性"""
        required_fields = ["code", "name", "price", "change_pct", "data_source"]
        
        for field in required_fields:
            if field not in data:
                raise DataValidationError(f"缺少必需字段: {field}")
        
        if not isinstance(data["price"], (int, float)) or data["price"] <= 0:
            raise DataValidationError(f"无效价格: {data.get('price')}")
    
    def _save_data(self, data: Dict) -> None:
        """保存数据到本地（原子写入）"""
        filename = f"{self.data_dir}/market_data_{datetime.now().strftime('%Y%m%d')}.json"
        temp_file = f"{filename}.tmp"
        
        try:
            # 1. 先写入临时文件
            with open(temp_file, "w", encoding="utf-8") as f:
                json.dump(data, f, ensure_ascii=False, indent=2, default=str)
            
            # 2. 原子重命名（确保文件完整性）
            os.replace(temp_file, filename)
            logger.info(f"💾 数据已保存到 {filename}")
            
        except IOError as e:
            logger.error(f"保存数据失败: {e}")
            # 清理临时文件
            if os.path.exists(temp_file):
                try:
                    os.remove(temp_file)
                except OSError:
                    pass
    
    def _mock_data(self, code: str, info: dict) -> Dict:
        """生成模拟数据（备用）"""
        base_price = info.get("base_price", 100.0)
        price = base_price + random.uniform(-5, 5)
        
        # 从配置中获取symbol，否则使用默认格式
        yf_symbol = info.get("yf_symbol", f"{code}.HK")
        
        return {
            "code": code,
            "name": info.get("name", f"Stock-{code}"),
            "symbol": yf_symbol,
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
            "data_source": "mock",
            "is_mock": True  # 标记为模拟数据
        }
    
    @staticmethod
    def _safe_round(value, decimals: int = 2) -> Optional[float]:
        """安全地四舍五入（处理None和NaN）"""
        if value is None or (isinstance(value, float) and pd.isna(value)):
            return None
        return round(float(value), decimals)


if __name__ == "__main__":
    # 测试
    collector = HKStockDataCollector()
    data = collector.get_daily_data(days=5)
    
    print("\n" + "="*50)
    print("数据采集完成")
    print("="*50)
    
    for code, stock_data in data.items():
        source = stock_data.get('data_source', 'unknown')
        is_mock = stock_data.get('is_mock', False)
        mock_flag = " [模拟]" if is_mock else ""
        
        print(f"\n{stock_data['name']} ({code}) [{source}]{mock_flag}:")
        print(f"  价格: ¥{stock_data['price']}")
        print(f"  涨跌: {stock_data['change_pct']:+.2f}%")
        print(f"  MA5: {stock_data.get('ma5', 'N/A')}")
        print(f"  RSI: {stock_data.get('rsi', 'N/A')}")
