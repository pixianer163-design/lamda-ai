#!/usr/bin/env python3
"""
LLM增强版恒生科技Agent
集成 RSSM世界模型 + LLM信号提取 + LLM决策增强
"""

import sys
import os
# 添加当前目录到Python路径
current_dir = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, current_dir)

# 尝试添加shared目录路径 (用于导入策略引擎)
shared_dir = os.path.join(current_dir, '../../shared')
if os.path.exists(shared_dir) and shared_dir not in sys.path:
    sys.path.insert(0, shared_dir)

import glob
import json
import os
import time
from datetime import datetime
from typing import Dict, Optional

# 导入统一常量
from shared.base import get_constants
constants = get_constants()
print(f"✅ 统一常量模块: 可用={constants.available}")

# 导入统一策略引擎
STRATEGY_ENGINE_AVAILABLE = False
StrategyEngine = None
get_strategy_engine = None
try:
    import strategy_engine
    StrategyEngine = strategy_engine.StrategyEngine
    get_strategy_engine = strategy_engine.get_strategy_engine
    STRATEGY_ENGINE_AVAILABLE = True
    print("✅ 统一策略引擎可用")
except ImportError as e:
    print(f"⚠️ 统一策略引擎不可用: {e}")

# 导入VectorBT回测器（可选）- 保持向后兼容
VECTORBT_AVAILABLE = False
VectorBTBacktester = None
try:
    from vectorbt_integration import VectorBTBacktester
    VECTORBT_AVAILABLE = True
    print("✅ VectorBT回测器可用 (旧版)")
except ImportError as e:
    print(f"⚠️ VectorBT回测器不可用: {e}")

# 导入日志模块
LOGGER_AVAILABLE = False
_logger = None
_info = _warning = _error = _debug = _critical = _get_logger = _setup_logging = None

try:
    import logger
    _get_logger = logger.get_logger
    _setup_logging = logger.setup_logging
    _info = logger.info
    _warning = logger.warning  
    _error = logger.error
    _debug = logger.debug
    _critical = logger.critical
    LOGGER_AVAILABLE = True
    # 配置日志（默认配置）
    _setup_logging(log_level="INFO", structured_output=False)
    _logger = _get_logger()
except ImportError as e:
    print(f"⚠️ 日志模块不可用: {e}")

# 导入所有模块
from rssm_world_model import WorldModelTrainer
from world_model_integration import WorldModelIntegration
from llm_signal_extractor import LLMSignalExtractor
from llm_decision_enhancer import LLMDecisionEnhancer


# 日志辅助函数
def _log_message(level: str, message: str, context: dict = None):
    """
    统一日志记录函数
    
    Args:
        level: 日志级别 (info, warning, error, debug, critical)
        message: 日志消息
        context: 上下文信息
    """
    # 总是打印到控制台（保持向后兼容）
    print(message)
    
    # 如果日志模块可用，记录结构化日志
    if LOGGER_AVAILABLE and _logger is not None:
        try:
            if level == "info" and _info:
                _info(message, context=context)
            elif level == "warning" and _warning:
                _warning(message, context=context)
            elif level == "error" and _error:
                _error(message, context=context)
            elif level == "debug" and _debug:
                _debug(message, context=context)
            elif level == "critical" and _critical:
                _critical(message, context=context)
        except Exception as e:
            print(f"⚠️ 日志记录失败: {e}")


def _log_performance(operation: str, duration: float, details: dict = None):
    """
    性能日志记录函数
    
    Args:
        operation: 操作名称
        duration: 耗时（秒）
        details: 额外详情
    """
    # 打印到控制台
    print(f"⏱️  {operation} 耗时 {duration:.3f}秒")
    
    # 如果日志模块可用，记录结构化性能日志
    if LOGGER_AVAILABLE and _logger is not None:
        try:
            _logger.log_performance(operation, duration, details)
        except Exception as e:
            print(f"⚠️ 性能日志记录失败: {e}")


def _log_decision(stock_code: str, action: str, confidence: float, reason: str, engine: str = "unknown"):
    """
    决策日志记录函数
    
    Args:
        stock_code: 股票代码
        action: 操作 (buy/sell/hold)
        confidence: 置信度 (0-1)
        reason: 理由
        engine: 决策引擎
    """
    # 打印到控制台
    print(f"🎯 {stock_code}: {action} (置信度: {confidence:.0%})")
    
    # 如果日志模块可用，记录结构化决策日志
    if LOGGER_AVAILABLE and _logger is not None:
        try:
            _logger.log_decision(stock_code, action, confidence, reason, engine)
        except Exception as e:
            print(f"⚠️ 决策日志记录失败: {e}")


class LLMEnhancedAgent:
    """
    LLM增强版Agent
    
    架构:
    1. 数据收集 -> 2. LLM信号提取 -> 3. RSSM世界模型预测 -> 4. LLM决策增强 -> 5. 执行
    """
    
    def __init__(self, data_dir=None):
        # 设置默认数据目录
        if data_dir is None:
            import os
            current_dir = os.path.dirname(os.path.abspath(__file__))
            # 使用统一数据目录（云端部署兼容）
            data_dir = os.environ.get('DATA_DIR', '/opt/hktech-agent/data')
            if not os.path.exists(data_dir):
                data_dir = os.path.join(current_dir, '../../data')
        self.data_dir = data_dir
        
        print("🚀 初始化 LLM增强版Agent...")
        
        # 1. 世界模型
        self.wm_integration = WorldModelIntegration(data_dir)
        if self.wm_integration.enabled:
            print("✅ 世界模型: 已加载")
        else:
            print("⚠️  世界模型: 未启用")
        
        # 2. LLM信号提取
        self.llm_extractor = LLMSignalExtractor(data_dir)
        print("✅ LLM信号提取: 已加载")
        
        # 3. LLM决策增强
        self.llm_enhancer = LLMDecisionEnhancer(data_dir)
        print("✅ LLM决策增强: 已加载")
        
        # 4. 统一策略引擎
        self.strategy_engine = None
        if STRATEGY_ENGINE_AVAILABLE:
            try:
                self.strategy_engine = get_strategy_engine()
                capabilities = self.strategy_engine.get_capabilities()
                print(f"✅ 策略引擎: {capabilities['engine_type']} (VectorBT可用: {capabilities['vectorbt_available']})")
            except Exception as e:
                print(f"⚠️ 策略引擎初始化失败: {e}")
                self.strategy_engine = None
        else:
            print("⚠️ 策略引擎: 不可用，使用传统策略")
        
        # 加载当前组合
        self.portfolio = self._load_portfolio()
        
        print("✅ Agent初始化完成\n")
    
    def _load_portfolio(self) -> Dict:
        """加载当前组合"""
        portfolio_file = f"{self.data_dir}/portfolio.json"
        if os.path.exists(portfolio_file):
            with open(portfolio_file, 'r') as f:
                return json.load(f)
        return {
            "cash": 19000,
            "holdings": {},
            "total_value": 19000
        }
    
    def _load_market_data(self) -> Dict:
        """加载市场数据 - 使用真实数据源"""
        import sys
        import os
        import logging
        
        # 计算正确的路径
        current_dir = os.path.dirname(os.path.abspath(__file__))
        active_src_path = os.path.join(current_dir, '../../active_src')
        hktech_src_path = os.path.join(current_dir, '../../../src')
        
        # 确保路径在 sys.path 中
        for p in [active_src_path, hktech_src_path]:
            if p not in sys.path:
                sys.path.insert(0, p)
        
        market_data = {}
        data_source_used = None
        
        # 尝试1: 使用 CN 数据采集器 (腾讯财经API)
        try:
            from data_collector_cn import HKStockDataCollectorCN
            
            print("   🌐 正在从腾讯财经API获取...")
            collector = HKStockDataCollectorCN()
            data = collector.get_daily_data(days=30)
            
            # 检查是否使用了 mock 数据
            if collector._mock_data_used:
                print("   ⚠️ 警告: 数据采集使用了 fallback mock 数据!")
                logging.warning("数据采集 fallback 到 mock 数据，决策生成被拒绝!")
                
            # 转换为内部格式
            for code, stock_info in data.items():
                market_data[code] = {
                    "price": stock_info.get("price", 0),
                    "ma5": stock_info.get("ma5", stock_info.get("price", 0)),
                    "ma20": stock_info.get("ma20", stock_info.get("price", 0)),
                    "rsi": stock_info.get("rsi", 50),
                    "change_pct": stock_info.get("change_pct", 0),
                    "volume": stock_info.get("volume", 0),
                    "data_source": stock_info.get("data_source", "unknown")
                }
                data_source_used = stock_info.get("data_source", "unknown")
            
            # 验证数据质量
            if self._validate_market_data(market_data):
                print(f"   ✅ 成功获取 {len(market_data)} 只股票实时数据 [数据源: {data_source_used}]")
                return market_data
            else:
                print("   ⚠️ 数据验证失败，尝试备用数据源...")
                
        except Exception as e:
            print(f"   ⚠️ CN数据源失败: {e}")
        
        # 尝试2: 使用 yfinance 数据采集器
        try:
            from data_collector import HKStockDataCollector
            
            print("   🌐 正在从Yahoo Finance获取...")
            collector = HKStockDataCollector(self.data_dir)
            data = collector.get_daily_data(days=30)
            
            # 转换为内部格式
            for code, stock_info in data.items():
                market_data[code] = {
                    "price": stock_info.get("price", 0),
                    "ma5": stock_info.get("ma5", stock_info.get("price", 0)),
                    "ma20": stock_info.get("ma20", stock_info.get("price", 0)),
                    "rsi": stock_info.get("rsi", 50),
                    "change_pct": stock_info.get("change_pct", 0),
                    "volume": stock_info.get("volume", 0),
                    "data_source": stock_info.get("data_source", "unknown")
                }
                data_source_used = stock_info.get("data_source", "unknown")
            
            if self._validate_market_data(market_data):
                print(f"   ✅ 成功获取 {len(market_data)} 只股票实时数据 [数据源: {data_source_used}]")
                return market_data
                
        except Exception as e:
            print(f"   ⚠️ Yahoo Finance数据源失败: {e}")
        
        # 尝试3: 从最新的历史数据文件加载
        print("   🔄 尝试从缓存加载...")
        pattern = f"{self.data_dir}/market_data_*.json"
        files = glob.glob(pattern)
        
        if files:
            latest_file = max(files, key=os.path.getmtime)
            try:
                with open(latest_file, 'r') as f:
                    data = json.load(f)
                # 检查缓存数据源
                sample = next(iter(data.values())) if data else {}
                cache_source = sample.get('data_source', 'unknown')
                if cache_source == 'mock':
                    print(f"   ⚠️ 缓存数据源为 mock，拒绝使用!")
                else:
                    print(f"   ✅ 使用缓存数据: {os.path.basename(latest_file)}")
                    return data
            except Exception as e2:
                print(f"   ⚠️ 缓存数据也失败: {e2}")
        
        # 拒绝使用 mock 数据生成决策
        print("   ❌ 错误: 无法获取真实数据，拒绝生成决策!")
        logging.error("数据采集完全失败，无法生成决策 - 需要真实数据!")
        return {}
    
    def _validate_market_data(self, market_data: Dict) -> bool:
        """验证市场数据质量"""
        if not market_data:
            return False
        
        # 检查是否有 mock 数据
        for code, data in market_data.items():
            source = data.get('data_source', 'unknown')
            if source in ['mock', 'fallback_mock']:
                print(f"   ⚠️ {code} 数据源为 mock，拒绝使用!")
                return False
            
            # 验证价格合理性
            price = data.get('price', 0)
            if price <= 0:
                print(f"   ⚠️ {code} 价格无效: {price}")
                return False
                
            # 验证涨跌幅合理性 (-20% ~ +20%)
            change_pct = data.get('change_pct', 0)
            if change_pct < -20 or change_pct > 20:
                print(f"   ⚠️ {code} 涨跌幅异常: {change_pct}%")
                return False
        
        return True

    def _load_market_data_safe(self) -> dict:
        """安全的数据加载：读缓存 → mock"""
        result = {}
        cache_dir = os.path.join(self.data_dir, "cache")

        # 确定目标股票列表
        try:
            from constants import DEFAULT_STOCKS
            stocks = DEFAULT_STOCKS
        except Exception:
            stocks = ["00700", "09988", "03690"]

        for code in stocks:
            pattern = os.path.join(cache_dir, f"{code}_*.json")
            files = sorted(glob.glob(pattern), reverse=True)
            if files:
                try:
                    with open(files[0], encoding="utf-8") as f:
                        data = json.load(f)
                    data["data_source"] = "cache"
                    result[code] = data
                    continue
                except Exception:
                    pass
            # Last resort: minimal mock
            result[code] = {
                "code": code, "price": 100.0, "rsi": 50.0,
                "trend": "neutral", "change_pct": 0.0,
                "ma5": 100.0, "ma20": 100.0, "volume": 1e7,
                "data_source": "emergency_mock"
            }
        return result

    def run_daily_analysis(self, news_items=None) -> dict:
        """
        每日分析流程 - 带逐步错误处理和优雅降级
        """
        start_time = time.time()
        print("=" * 60)
        print(f"📊 LLM增强版Agent - 每日分析")
        print(f"⏰ {datetime.now().strftime('%Y-%m-%d %H:%M')}")
        print("=" * 60)

        result = {}

        # Step 1: 获取市场数据
        print("\n1️⃣ 获取市场数据...")
        step_start = time.time()
        try:
            market_data = self._load_market_data()
        except Exception as e:
            print(f"⚠️ Step1 数据加载异常: {e}，使用安全 fallback")
            market_data = self._load_market_data_safe()
        step_duration = time.time() - step_start
        _log_performance("load_market_data", step_duration, {"stocks_count": len(market_data)})
        
        # 验证数据源：如果没有有效数据，拒绝生成决策
        if not market_data or len(market_data) == 0:
            print("\n❌ 错误: 无法获取市场数据，拒绝生成决策!")
            print("   原因: 数据采集失败，需要真实数据才能生成决策")
            return {
                "error": "no_market_data",
                "message": "无法获取市场数据，拒绝生成决策",
                "market_data_source": "failed"
            }
        
        # 检查数据源是否为真实数据
        sample_data = list(market_data.values())[0]
        data_source = sample_data.get("data_source", "unknown")
        if data_source in ["mock", "fallback_mock", "unknown"]:
            print(f"\n⚠️ 警告: 当前数据源为 '{data_source}'")
            print("   AI决策需要真实数据，拒绝生成基于mock数据的决策!")
            return {
                "error": "mock_data_rejected",
                "message": "数据源为mock数据，拒绝生成决策",
                "market_data_source": data_source
            }
        
        print(f"   已获取 {len(market_data)} 只股票数据 [数据源: {data_source}]")
        result["market_data_source"] = (
            list(market_data.values())[0].get("data_source", "unknown")
            if market_data else "empty"
        )

        # Step 2: LLM信号提取
        print("\n2️⃣ LLM信号提取...")
        step_start = time.time()
        try:
            if news_items:
                llm_signals = self.llm_extractor.analyze_news(news_items)
            else:
                llm_signals = self.llm_extractor.get_latest_signals()
        except Exception as e:
            print(f"⚠️ Step2 LLM信号提取异常: {e}")
            llm_signals = {}
        step_duration = time.time() - step_start
        _log_performance("llm_signal_extraction", step_duration, {"has_news": news_items is not None})

        # 安全打印信号（兼容任意 key 格式）
        for code in ["00700", "09988", "03690"]:
            sentiment_key = f"{code}_sentiment"
            if sentiment_key in llm_signals:
                print(f"   {code} 情绪: {llm_signals[sentiment_key]:.2f}")
            elif code in llm_signals:
                val = llm_signals[code]
                if isinstance(val, (int, float)):
                    print(f"   {code} 情绪: {val:.2f}")

        # Step 3: 世界模型预测
        print("\n3️⃣ 世界模型预测...")
        step_start = time.time()
        try:
            prediction = self.wm_integration.predict_future(
                market_data, self.portfolio, horizon=5
            )
            if prediction.get("enabled"):
                print(f"   预测天数: {prediction['horizon']}天")
                print(f"   累计收益: {prediction['cumulative_return']:+.2f}%")
                print(f"   置信度: {prediction['confidence']:.0%}")
                print(f"   建议: {prediction['recommendation']}")
            else:
                print(f"   ⚠️ {prediction.get('message', '预测失败')}")
        except Exception as e:
            print(f"⚠️ Step3 世界模型预测异常: {e}")
            prediction = {
                "enabled": False, "recommendation": "hold",
                "confidence": 0.0, "predicted_returns": {},
                "cumulative_return": 0.0, "reasoning": str(e), "actions": []
            }
        step_duration = time.time() - step_start
        _log_performance("world_model_prediction", step_duration,
                         {"enabled": prediction.get("enabled", False) if prediction else False})

        # Step 4: 统一策略引擎决策
        print("\n4️⃣ 统一策略引擎决策...")
        step_start = time.time()
        try:
            base_decision = self._base_strategy(market_data, prediction)
        except Exception as e:
            print(f"⚠️ Step4 基础策略异常: {e}，全部 HOLD")
            base_decision = {
                "decisions": {code: {"action": "hold", "confidence": 0.5}
                              for code in market_data},
                "summary": f"策略引擎异常: {e}"
            }
        step_duration = time.time() - step_start

        # 兼容两种格式：直接 dict 或 {"decisions": {...}}
        decisions_map = (base_decision.get("decisions", base_decision)
                         if isinstance(base_decision, dict) else base_decision)
        _log_performance("base_strategy", step_duration, {"stocks_count": len(decisions_map)})

        for code, dec in decisions_map.items():
            print(f"   {code}: {dec['action']} (置信度{dec['confidence']:.0%})")
            engine = "strategy_engine" if self.strategy_engine is not None else "fallback_strategy"
            _log_decision(code, dec["action"], dec["confidence"],
                          dec.get("_engine_reason", "传统策略"), engine)

        # Step 5: LLM决策增强
        print("\n5️⃣ LLM决策增强...")
        step_start = time.time()
        try:
            enhanced = self.llm_enhancer.enhance_decision(
                base_decision, market_data, self.portfolio,
                prediction=prediction, llm_signals=llm_signals
            )
        except Exception as e:
            print(f"⚠️ Step5 决策增强异常: {e}")
            enhanced = {
                "final_decision": decisions_map,
                "llm_output": {},
                "error": str(e)
            }
        step_duration = time.time() - step_start
        _log_performance("llm_decision_enhancement", step_duration,
                         {"stocks_count": len(enhanced.get("final_decision", {}))})

        # 安全打印增强结果
        llm_out = enhanced.get("llm_output", {})
        if llm_out and "analysis" in llm_out:
            print(f"   LLM分析: {llm_out['analysis'][:50]}...")
        print(f"   最终决策:")
        for code, dec in enhanced.get("final_decision", {}).items():
            reason = dec.get("reason", "N/A")
            print(f"     {code}: {dec['action']} ({str(reason)[:30]}...)")
            _log_decision(code, dec["action"], dec.get("confidence", 0.5),
                          reason, "llm_enhanced")

        # 记录总体性能
        total_duration = time.time() - start_time
        _log_performance("daily_analysis_total", total_duration, {
            "stocks_analyzed": len(market_data),
            "steps": 5,
            "prediction_enabled": prediction is not None and prediction.get("enabled", False)
        })

        result.update(enhanced)
        
        # ========== 保存数据到文件 ==========
        self._save_analysis_result(result, market_data)
        
        return result
    
    def _save_analysis_result(self, result: Dict, market_data: Dict):
        """
        保存分析结果到文件（云端运行必需）
        
        保存内容:
        1. market_data_YYYYMMDD.json - 市场数据
        2. analysis_result_YYYYMMDD.json - 完整分析结果
        3. analysis_result_latest.json - 最新结果（方便读取）
        """
        today = datetime.now().strftime("%Y%m%d")
        timestamp = datetime.now().isoformat()
        
        # 1. 保存市场数据
        market_data_file = os.path.join(self.data_dir, f"market_data_{today}.json")
        try:
            # 添加时间戳
            market_data_with_time = {
                code: {**data, "updated_at": timestamp}
                for code, data in market_data.items()
            }
            with open(market_data_file, "w", encoding="utf-8") as f:
                json.dump(market_data_with_time, f, indent=2, ensure_ascii=False)
            
            # 更新 latest 链接
            latest_file = os.path.join(self.data_dir, "market_data_latest.json")
            with open(latest_file, "w", encoding="utf-8") as f:
                json.dump(market_data_with_time, f, indent=2, ensure_ascii=False)
            
            print(f"   ✅ 市场数据已保存：{market_data_file}")
        except Exception as e:
            print(f"   ❌ 市场数据保存失败：{e}")
        
        # 2. 保存完整分析结果
        result_file = os.path.join(self.data_dir, f"analysis_result_{today}.json")
        try:
            result_with_meta = {
                "timestamp": timestamp,
                "date": today,
                "analysis_result": result,
                "market_data_summary": {
                    code: {
                        "price": data.get("price"),
                        "change_pct": data.get("change_pct"),
                        "trend": data.get("trend")
                    }
                    for code, data in market_data.items()
                }
            }
            with open(result_file, "w", encoding="utf-8") as f:
                json.dump(result_with_meta, f, indent=2, ensure_ascii=False)
            
            # 更新 latest 链接
            latest_result = os.path.join(self.data_dir, "analysis_result_latest.json")
            with open(latest_result, "w", encoding="utf-8") as f:
                json.dump(result_with_meta, f, indent=2, ensure_ascii=False)
            
            print(f"   ✅ 分析结果已保存：{result_file}")
        except Exception as e:
            print(f"   ❌ 分析结果保存失败：{e}")
        
        # 3. 保存投资决策（简化版，方便推送程序读取）
        decisions_file = os.path.join(self.data_dir, f"decisions_{today}.json")
        try:
            final_decisions = result.get("final_decision", {})
            decisions_summary = {
                "timestamp": timestamp,
                "date": today,
                "decisions": {
                    code: {
                        "action": dec.get("action", "hold"),
                        "confidence": dec.get("confidence", 0.5),
                        "reason": dec.get("reason", "")[:100]  # 限制长度
                    }
                    for code, dec in final_decisions.items()
                },
                "world_model": {
                    "enabled": result.get("prediction", {}).get("enabled", False),
                    "recommendation": result.get("prediction", {}).get("recommendation", ""),
                    "confidence": result.get("prediction", {}).get("confidence", 0.0)
                } if result.get("prediction") else {}
            }
            with open(decisions_file, "w", encoding="utf-8") as f:
                json.dump(decisions_summary, f, indent=2, ensure_ascii=False)
            
            print(f"   ✅ 投资决策已保存：{decisions_file}")
        except Exception as e:
            print(f"   ❌ 投资决策保存失败：{e}")
    
    def _base_strategy(self, market_data: Dict, prediction: Optional[Dict]) -> Dict:
        """
        基础策略（统一策略引擎 + 世界模型增强）
        """
        decisions = {}
        
        # 1. 使用统一策略引擎生成基础信号
        if self.strategy_engine is not None:
            try:
                # 使用策略引擎生成信号
                engine_signals = self.strategy_engine.generate_signals(market_data)
                print("   📊 策略引擎信号生成完成")
                
                # 转换为决策格式
                for code, signal in engine_signals.items():
                    action = signal.get("action", "hold")
                    conf = signal.get("confidence", 0.5)
                    reason = signal.get("reason", "")
                    
                    decisions[code] = {
                        "action": action,
                        "confidence": round(conf, 2),
                        "_engine_reason": reason
                    }
                    
            except Exception as e:
                print(f"   ⚠️ 策略引擎失败: {e}")
                # 回退到传统策略
                decisions = self._fallback_strategy(market_data)
        else:
            # 无策略引擎，使用传统策略
            decisions = self._fallback_strategy(market_data)
        
        # 2. 世界模型增强（如果有预测）
        if prediction and prediction.get('enabled'):
            print("   🧠 应用世界模型增强...")
            for code, decision in decisions.items():
                action = decision["action"]
                conf = decision["confidence"]
                
                if prediction['recommendation'] == action:
                    # 增强置信度
                    conf = min(0.95, conf + 0.2)
                elif prediction['confidence'] > 0.8:
                    # 世界模型高置信度，覆盖策略
                    action = prediction['recommendation']
                    conf = prediction['confidence']
                    print(f"      {code}: 世界模型覆盖 → {action} (置信度{conf:.0%})")
                
                decisions[code] = {
                    "action": action,
                    "confidence": round(conf, 2)
                }
        
        return decisions
    
    def _fallback_strategy(self, market_data: Dict) -> Dict:
        """
        传统策略（回退方案）
        """
        decisions = {}
        
        # 使用统一常量
        stock_codes = constants.DEFAULT_STOCKS
        
        for code in stock_codes:
            data = market_data.get(code, {})
            rsi = data.get('rsi', 50)
            change = data.get('change_pct', 0)
            
            # 基础规则
            if rsi > 70 and change > 2:
                action = "sell"
                conf = 0.6
            elif rsi < 30 and change < -2:
                action = "buy"
                conf = 0.6
            else:
                action = "hold"
                conf = 0.5
            
            decisions[code] = {
                "action": action,
                "confidence": round(conf, 2)
            }
        
        return decisions


def main():
    """主函数"""
    print("\n" + "="*60)
    print("🤖 LLM增强版恒生科技Agent")
    print("="*60 + "\n")
    
    # 创建Agent
    agent = LLMEnhancedAgent()
    
    # 模拟新闻
    sample_news = [
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
    
    # 运行分析
    result = agent.run_daily_analysis(news_items=sample_news)
    
    print("\n✅ 分析完成!")
    print(f"📁 数据保存于: {agent.data_dir}/")
    print("\n下一步:")
    print("1. 接入真实新闻API (如新浪财经、华尔街见闻)")
    print("2. 接入真实LLM API (OpenAI/Claude)")
    print("3. 部署到定时任务，每日自动运行")


if __name__ == "__main__":
    main()
