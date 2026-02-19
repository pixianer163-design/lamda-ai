#!/usr/bin/env python3
"""
LLM信号提取模块
支持 DeepSeek API 调用，无 API Key 或调用失败时 fallback 到关键词匹配
"""

import json
import os
import random
import requests
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
    LLM信号提取器
    优先调用 DeepSeek API，无 API Key 或调用失败时 fallback 到关键词匹配
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

        api_key_available = bool(os.environ.get("DEEPSEEK_API_KEY", ""))
        mode = "DeepSeek API 模式" if api_key_available else "关键词 fallback 模式"
        print(f"📡 LLM信号提取器初始化 ({mode})")

    # ------------------------------------------------------------------
    # 核心方法：关键词 fallback 与 DeepSeek API 调用
    # ------------------------------------------------------------------

    def _keyword_fallback(self, stock_code: str, news_items: list) -> dict:
        """关键词匹配 fallback"""
        positive_keywords = ["增长", "超预期", "盈利", "利好", "上涨", "突破", "合作", "扩张"]
        negative_keywords = ["下滑", "亏损", "利空", "下跌", "监管", "罚款", "裁员", "收缩"]
        score = 0.5
        text = " ".join([str(n) for n in news_items])
        for kw in positive_keywords:
            if kw in text:
                score = min(score + 0.05, 0.9)
        for kw in negative_keywords:
            if kw in text:
                score = max(score - 0.05, 0.1)
        return {"sentiment": round(score, 2), "key_factors": [], "confidence": 0.5}

    def _call_llm_api(self, stock_code: str, news_items: list) -> dict:
        """调用 DeepSeek API 提取情感信号，失败时 fallback 到关键词匹配"""
        api_key = os.environ.get("DEEPSEEK_API_KEY", "")
        if not api_key:
            return self._keyword_fallback(stock_code, news_items)

        stock_name = self.stock_names.get(stock_code, stock_code)
        news_text = "\n".join([f"- {n}" for n in news_items[:5]])
        prompt = (
            f"你是一位专业的港股分析师。请分析以下关于{stock_name}({stock_code})的新闻，"
            f"返回JSON格式的情感分析结果。\n\n新闻内容：\n{news_text}\n\n"
            "要求返回格式（只返回JSON，不要其他文字）：\n"
            '{"sentiment": 0到1之间的浮点数, "key_factors": ["因素1","因素2"], '
            '"confidence": 0到1之间的浮点数}\n'
            "其中sentiment含义：0=极度悲观, 0.5=中性, 1=极度乐观"
        )
        try:
            resp = requests.post(
                "https://api.deepseek.com/v1/chat/completions",
                headers={"Authorization": f"Bearer {api_key}",
                         "Content-Type": "application/json"},
                json={
                    "model": "deepseek-chat",
                    "messages": [{"role": "user", "content": prompt}],
                    "temperature": 0.1,
                    "max_tokens": 200,
                },
                timeout=30,
            )
            resp.raise_for_status()
            content = resp.json()["choices"][0]["message"]["content"].strip()
            if "{" in content:
                content = content[content.index("{"):content.rindex("}") + 1]
            result = json.loads(content)
            result.setdefault("confidence", 0.7)
            result["sentiment"] = max(0.0, min(1.0, float(result["sentiment"])))
            return result
        except Exception as e:
            print(f"⚠️ DeepSeek API 调用失败 ({stock_code}): {e}，fallback 到关键词匹配")
            return self._keyword_fallback(stock_code, news_items)

    # ------------------------------------------------------------------
    # 公共接口
    # ------------------------------------------------------------------

    def analyze_news(self, news_items: List[Dict]) -> Dict[str, float]:
        """
        分析新闻，提取情绪信号

        Args:
            news_items: 新闻列表，每个元素包含 title, content, source 等

        Returns:
            Dict[str, float]: 股票情绪信号字典，key 为股票代码，值在 0-1 之间
        """
        print(f"   📰 分析 {len(news_items)} 条新闻...")

        signals = {}

        for stock_code in self.stocks:
            # 收集与该股票相关的新闻文本
            relevant_texts = []
            stock_name = self.stock_names.get(stock_code, stock_code)
            for news in news_items:
                title = news.get("title", "")
                content = news.get("content", "")
                combined = f"{title} {content}"
                relevant_texts.append(combined)

            result = self._call_llm_api(stock_code, relevant_texts)
            signals[stock_code] = round(float(result["sentiment"]), 2)

        # 保存信号
        self._save_signals(signals)

        print(f"   ✅ 情绪信号生成完成:")
        for code, sentiment in signals.items():
            stock_name = self.stock_names.get(code, code)
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

        # 生成 fallback 信号
        print(f"   🔄 生成 fallback 信号...")
        signals = {}
        for stock_code in self.stocks:
            sentiment = 0.5 + random.uniform(-0.2, 0.2)
            sentiment = max(0.2, min(0.8, sentiment))
            signals[stock_code] = round(sentiment, 2)

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
        """提取信号（analyze_news 的别名）"""
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
