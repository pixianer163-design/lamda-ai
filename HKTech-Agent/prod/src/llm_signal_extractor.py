#!/usr/bin/env python3
"""
LLM信号提取模块 (模拟版本)
由于真实LLM API可能不可用，提供模拟实现
"""

import json
import os
import random
import sys
from datetime import datetime
from typing import Dict, List

# 导入共享常量
SHARED_CONSTANTS_AVAILABLE = False
constants = None  # 默认值
try:
    import constants
    SHARED_CONSTANTS_AVAILABLE = True
except ImportError:
    print("⚠️ 共享常量模块不可用，使用本地定义")


class LLMSignalExtractor:
    """
    LLM信号提取器 - 模拟版本
    真实部署时需要替换为实际LLM API调用
    """
    
    def __init__(self, data_dir=None):
        if data_dir is None:
            try:
                import sys as _sys, os as _os
                _sys.path.insert(0, _os.path.join(_os.path.dirname(_os.path.abspath(__file__)), '../../shared'))
                from config import get_config
                data_dir = str(get_config().data_dir)
            except Exception:
                import os as _os
                data_dir = _os.path.join(_os.path.dirname(_os.path.abspath(__file__)), '../../data')
        self.data_dir = data_dir
        os.makedirs(data_dir, exist_ok=True)
        
        # 股票代码映射（使用共享常量或本地定义）
        if SHARED_CONSTANTS_AVAILABLE and constants is not None:
            self.stocks = constants.DEFAULT_STOCKS
            self.stock_names = constants.STOCK_NAMES
        else:
            self.stocks = ["00700", "09988", "03690"]
            self.stock_names = {
                "00700": "腾讯控股",
                "09988": "阿里巴巴", 
                "03690": "美团-W"
            }
        
        # 信号缓存文件
        self.signals_file = f"{data_dir}/llm_signals.json"
        
        print(f"📡 LLM信号提取器初始化 (模拟模式)")
    
    def analyze_news(self, news_items: List[Dict]) -> Dict[str, float]:
        """
        分析新闻，提取情绪信号
        
        Args:
            news_items: 新闻列表，每个元素包含title, content, source等
            
        Returns:
            Dict[str, float]: 股票情绪信号字典，值在0-1之间
        """
        print(f"   📰 分析 {len(news_items)} 条新闻...")
        
        # 模拟LLM分析 - 实际应调用LLM API
        signals = {}
        
        for stock_code in self.stocks:
            # 基于新闻内容生成模拟情绪信号
            base_sentiment = 0.5  # 中性
            
            # 简单关键词匹配（模拟）
            for news in news_items:
                content = f"{news.get('title', '')} {news.get('content', '')}".lower()
                
                if stock_code == "00700":  # 腾讯
                    if any(word in content for word in ["腾讯", "游戏", "微信", "增长", "超预期"]):
                        base_sentiment += 0.15
                    if any(word in content for word in ["监管", "下滑", "亏损", "放缓"]):
                        base_sentiment -= 0.10
                
                elif stock_code == "09988":  # 阿里
                    if any(word in content for word in ["阿里", "电商", "云", "复苏", "反弹"]):
                        base_sentiment += 0.12
                    if any(word in content for word in ["竞争", "压力", "下滑", "放缓"]):
                        base_sentiment -= 0.12
                
                elif stock_code == "03690":  # 美团
                    if any(word in content for word in ["美团", "外卖", "增长", "扩张", "盈利"]):
                        base_sentiment += 0.10
                    if any(word in content for word in ["竞争", "监管", "成本", "压力"]):
                        base_sentiment -= 0.08
            
            # 添加随机波动和归一化
            sentiment = base_sentiment + random.uniform(-0.05, 0.05)
            sentiment = max(0.1, min(0.9, sentiment))  # 限制在0.1-0.9之间
            
            signals[f"{stock_code}_sentiment"] = round(sentiment, 2)
        
        # 保存信号
        self._save_signals(signals)
        
        print(f"   ✅ 情绪信号生成完成:")
        for code, sentiment in signals.items():
            stock_name = self.stock_names.get(code.replace("_sentiment", ""), code)
            print(f"     {stock_name}: {sentiment:.2f}")
        
        return signals
    
    def get_latest_signals(self) -> Dict[str, float]:
        """
        获取最新的LLM信号（从缓存或生成模拟信号）
        
        Returns:
            Dict[str, float]: 股票情绪信号
        """
        # 尝试从文件加载
        if os.path.exists(self.signals_file):
            try:
                with open(self.signals_file, 'r') as f:
                    signals = json.load(f)
                
                # 检查是否过期（超过24小时）
                if "timestamp" in signals:
                    import time
                    file_age = time.time() - signals["timestamp"]
                    if file_age < 24 * 3600:  # 24小时内
                        print(f"   📄 使用缓存信号 (生成于 {datetime.fromtimestamp(signals['timestamp']):%H:%M})")
                        # 移除时间戳返回
                        return {k: v for k, v in signals.items() if k != "timestamp"}
            except Exception as e:
                print(f"   ⚠️ 读取缓存信号失败: {e}")
        
        # 生成模拟信号
        print(f"   🔄 生成模拟信号...")
        signals = {}
        for stock_code in self.stocks:
            # 随机生成信号（模拟）
            sentiment = 0.5 + random.uniform(-0.2, 0.2)
            sentiment = max(0.2, min(0.8, sentiment))
            signals[f"{stock_code}_sentiment"] = round(sentiment, 2)
        
        # 保存
        self._save_signals(signals)
        
        return signals
    
    def _save_signals(self, signals: Dict[str, float]):
        """保存信号到文件"""
        try:
            signals_with_ts = signals.copy()
            signals_with_ts["timestamp"] = datetime.now().timestamp()
            
            with open(self.signals_file, 'w') as f:
                json.dump(signals_with_ts, f, indent=2)
        except Exception as e:
            print(f"   ⚠️ 保存信号失败: {e}")
    
    def save_signals(self, signals: Dict[str, float]):
        """保存信号到文件（公共方法）"""
        self._save_signals(signals)
    
    def load_signals(self) -> Dict[str, float]:
        """从文件加载信号"""
        if os.path.exists(self.signals_file):
            try:
                with open(self.signals_file, 'r') as f:
                    signals = json.load(f)
                return signals
            except Exception as e:
                print(f"   ⚠️ 加载信号失败: {e}")
        return {}
    
    def extract_signals(self, news_items: List[Dict]) -> Dict[str, float]:
        """提取信号（analyze_news的别名）"""
        return self.analyze_news(news_items)
    
    def clear_cache(self):
        """清除缓存信号"""
        if os.path.exists(self.signals_file):
            os.remove(self.signals_file)
            print("🗑️  信号缓存已清除")


if __name__ == "__main__":
    # 测试模块
    extractor = LLMSignalExtractor()
    
    # 测试新闻分析
    test_news = [
        {
            "title": "腾讯Q4财报超预期，游戏收入增长20%",
            "content": "腾讯发布2024年Q4财报，游戏业务收入同比增长20%，微信月活突破13亿。",
            "source": "财联社"
        },
        {
            "title": "阿里巴巴云计算业务增速放缓",
            "content": "阿里Q4云业务增速降至10%，低于市场预期。",
            "source": "华尔街见闻"
        }
    ]
    
    print("🧪 测试LLM信号提取...")
    signals = extractor.analyze_news(test_news)
    print(f"📊 结果: {signals}")
    
    # 测试获取最新信号
    latest = extractor.get_latest_signals()
    print(f"📄 最新信号: {latest}")