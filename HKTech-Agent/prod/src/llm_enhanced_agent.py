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

import json
import os
import time
from datetime import datetime
from typing import Dict, Optional

# 导入共享常量
SHARED_CONSTANTS_AVAILABLE = False
constants = None  # 默认值
try:
    import constants
    SHARED_CONSTANTS_AVAILABLE = True
except ImportError:
    print("⚠️ 共享常量模块不可用，使用本地定义")

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
        # 计算正确的active_src路径
        current_dir = os.path.dirname(os.path.abspath(__file__))
        active_src_path = os.path.join(current_dir, '../../active_src')
        sys.path.insert(0, active_src_path)
        
        try:
            from data_collector import HKStockDataCollector
            
            print("   🌐 正在从实时数据源获取...")
            collector = HKStockDataCollector(self.data_dir)
            data = collector.get_daily_data(days=30)
            
            # 转换为内部格式
            market_data = {}
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
            
            print(f"   ✅ 成功获取 {len(market_data)} 只股票实时数据")
            return market_data
            
        except Exception as e:
            print(f"   ⚠️ 实时数据获取失败: {e}")
            print("   🔄 尝试备用数据源...")
            
            # 尝试从最新的历史数据文件加载
            import glob
            import os
            
            # 查找最新的市场数据文件
            pattern = f"{self.data_dir}/market_data_*.json"
            files = glob.glob(pattern)
            
            if files:
                # 按修改时间排序，取最新的
                latest_file = max(files, key=os.path.getmtime)
                try:
                    with open(latest_file, 'r') as f:
                        data = json.load(f)
                    print(f"   ✅ 使用备用数据: {os.path.basename(latest_file)}")
                    return data
                except Exception as e2:
                    print(f"   ⚠️ 备用数据也失败: {e2}")
            
            # 最后回退：使用默认值（带警告）
            print("   ⚠️ 警告：使用默认模拟数据（非真实股价！）")
            return {
                "00700": {"price": 385, "ma5": 382, "ma20": 375, "rsi": 65, "change_pct": 0, "data_source": "fallback_mock"},
                "09988": {"price": 85, "ma5": 84, "ma20": 86, "rsi": 45, "change_pct": 0, "data_source": "fallback_mock"},
                "03690": {"price": 130, "ma5": 128, "ma20": 125, "rsi": 70, "change_pct": 0, "data_source": "fallback_mock"}
            }
    
    def run_daily_analysis(self, news_items=None):
        """
        每日分析流程
        """
        start_time = time.time()
        print("="*60)
        print(f"📊 LLM增强版Agent - 每日分析")
        print(f"⏰ {datetime.now().strftime('%Y-%m-%d %H:%M')}")
        print("="*60)
        
        # Step 1: 获取市场数据
        print("\n1️⃣ 获取市场数据...")
        step_start = time.time()
        market_data = self._load_market_data()
        step_duration = time.time() - step_start
        _log_performance("load_market_data", step_duration, {"stocks_count": len(market_data)})
        print(f"   已获取 {len(market_data)} 只股票数据")
        
        # Step 2: LLM信号提取
        print("\n2️⃣ LLM信号提取...")
        step_start = time.time()
        if news_items:
            llm_signals = self.llm_extractor.analyze_news(news_items)
        else:
            # 使用已有信号
            llm_signals = self.llm_extractor.get_latest_signals()
        step_duration = time.time() - step_start
        _log_performance("llm_signal_extraction", step_duration, {"has_news": news_items is not None})
        
        print(f"   腾讯情绪: {llm_signals['00700_sentiment']:.2f}")
        print(f"   阿里情绪: {llm_signals['09988_sentiment']:.2f}")
        print(f"   美团情绪: {llm_signals['03690_sentiment']:.2f}")
        
        # Step 3: 世界模型预测
        print("\n3️⃣ 世界模型预测...")
        step_start = time.time()
        if self.wm_integration.enabled:
            prediction = self.wm_integration.predict_future(
                market_data, self.portfolio, horizon=5
            )
            if prediction.get('enabled'):
                print(f"   预测天数: {prediction['horizon']}天")
                print(f"   累计收益: {prediction['cumulative_return']:+.2f}%")
                print(f"   置信度: {prediction['confidence']:.0%}")
                print(f"   建议: {prediction['recommendation']}")
            else:
                print(f"   ⚠️ {prediction.get('message', '预测失败')}")
                prediction = None
        else:
            print("   ⚠️ 世界模型未启用")
            prediction = None
        step_duration = time.time() - step_start
        _log_performance("world_model_prediction", step_duration, {"enabled": self.wm_integration.enabled})
        
        # Step 4: 统一策略引擎决策
        print("\n4️⃣ 统一策略引擎决策...")
        step_start = time.time()
        base_decision = self._base_strategy(market_data, prediction)
        step_duration = time.time() - step_start
        _log_performance("base_strategy", step_duration, {"stocks_count": len(base_decision)})
        
        for code, dec in base_decision.items():
            print(f"   {code}: {dec['action']} (置信度{dec['confidence']:.0%})")
            # 记录基础决策日志
            engine = "strategy_engine" if self.strategy_engine is not None else "fallback_strategy"
            _log_decision(code, dec['action'], dec['confidence'], 
                         dec.get('_engine_reason', '传统策略'), engine)
        
        # Step 5: LLM决策增强
        print("\n5️⃣ LLM决策增强...")
        step_start = time.time()
        enhanced = self.llm_enhancer.enhance_decision(
            base_decision, market_data, self.portfolio,
            prediction, llm_signals
        )
        step_duration = time.time() - step_start
        _log_performance("llm_decision_enhancement", step_duration, {"stocks_count": len(enhanced.get('final_decision', {}))})
        
        print(f"   LLM分析: {enhanced['llm_output']['analysis'][:50]}...")
        print(f"   最终决策:")
        for code, dec in enhanced['final_decision'].items():
            print(f"     {code}: {dec['action']} ({dec['reason'][:30]}...)")
            # 记录最终决策日志
            _log_decision(code, dec['action'], dec['confidence'], 
                         dec.get('reason', 'LLM增强决策'), "llm_enhanced")
        
        # Step 6: 生成报告
        print("\n6️⃣ 生成投资报告...")
        step_start = time.time()
        report = self.llm_enhancer.generate_daily_report(enhanced)
        
        # 保存报告
        report_file = f"{self.data_dir}/daily_report_{datetime.now().strftime('%Y%m%d')}.txt"
        with open(report_file, 'w') as f:
            f.write(report)
        step_duration = time.time() - step_start
        _log_performance("generate_report", step_duration, {"report_file": report_file})
        
        print(f"   💾 报告已保存: {report_file}")
        
        # 打印报告
        print("\n" + "="*60)
        print(report)
        print("="*60)
        
        # 记录总体性能
        total_duration = time.time() - start_time
        _log_performance("daily_analysis_total", total_duration, {
            "stocks_analyzed": len(market_data),
            "steps": 6,
            "prediction_enabled": prediction is not None and prediction.get('enabled', False)
        })
        
        return enhanced
    
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
        
        # 使用共享常量或本地定义
        if SHARED_CONSTANTS_AVAILABLE:
            stock_codes = constants.DEFAULT_STOCKS
        else:
            stock_codes = ["00700", "09988", "03690"]
        
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
