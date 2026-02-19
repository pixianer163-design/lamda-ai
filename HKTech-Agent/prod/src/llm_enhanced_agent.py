#!/usr/bin/env python3
"""
LLM增强版恒生科技Agent
集成 RSSM世界模型 + LLM信号提取 + LLM决策增强
"""

import sys
sys.path.insert(0, '/opt/hktech-agent/src')

import json
import os
from datetime import datetime
from typing import Dict

# 导入所有模块
from rssm_world_model import WorldModelTrainer
from world_model_integration import WorldModelIntegration
from llm_signal_extractor import LLMSignalExtractor
from llm_decision_enhancer import LLMDecisionEnhancer


class LLMEnhancedAgent:
    """
    LLM增强版Agent
    
    架构:
    1. 数据收集 -> 2. LLM信号提取 -> 3. RSSM世界模型预测 -> 4. LLM决策增强 -> 5. 执行
    """
    
    def __init__(self, data_dir="/opt/hktech-agent/data"):
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
        sys.path.insert(0, '/opt/hktech-agent/active_src')
        
        try:
            from data_collector import HKStockDataCollector
            
            print("   🌐 正在从实时数据源获取...")
            collector = HKStockDataCollector()
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
        print("="*60)
        print(f"📊 LLM增强版Agent - 每日分析")
        print(f"⏰ {datetime.now().strftime('%Y-%m-%d %H:%M')}")
        print("="*60)
        
        # Step 1: 获取市场数据
        print("\n1️⃣ 获取市场数据...")
        market_data = self._load_market_data()
        print(f"   已获取 {len(market_data)} 只股票数据")
        
        # Step 2: LLM信号提取
        print("\n2️⃣ LLM信号提取...")
        if news_items:
            llm_signals = self.llm_extractor.analyze_news(news_items)
        else:
            # 使用已有信号
            llm_signals = self.llm_extractor.get_latest_signals()
        
        print(f"   腾讯情绪: {llm_signals['00700_sentiment']:.2f}")
        print(f"   阿里情绪: {llm_signals['09988_sentiment']:.2f}")
        print(f"   美团情绪: {llm_signals['03690_sentiment']:.2f}")
        
        # Step 3: 世界模型预测
        print("\n3️⃣ 世界模型预测...")
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
        
        # Step 4: 基础决策（规则策略）
        print("\n4️⃣ 基础策略决策...")
        base_decision = self._base_strategy(market_data, prediction)
        for code, dec in base_decision.items():
            print(f"   {code}: {dec['action']} (置信度{dec['confidence']:.0%})")
        
        # Step 5: LLM决策增强
        print("\n5️⃣ LLM决策增强...")
        enhanced = self.llm_enhancer.enhance_decision(
            base_decision, market_data, self.portfolio,
            prediction, llm_signals
        )
        
        print(f"   LLM分析: {enhanced['llm_output']['analysis'][:50]}...")
        print(f"   最终决策:")
        for code, dec in enhanced['final_decision'].items():
            print(f"     {code}: {dec['action']} ({dec['reason'][:30]}...)")
        
        # Step 6: 生成报告
        print("\n6️⃣ 生成投资报告...")
        report = self.llm_enhancer.generate_daily_report(enhanced)
        
        # 保存报告
        report_file = f"{self.data_dir}/daily_report_{datetime.now().strftime('%Y%m%d')}.txt"
        with open(report_file, 'w') as f:
            f.write(report)
        print(f"   💾 报告已保存: {report_file}")
        
        # 打印报告
        print("\n" + "="*60)
        print(report)
        print("="*60)
        
        return enhanced
    
    def _base_strategy(self, market_data: Dict, prediction: Dict) -> Dict:
        """
        基础策略（规则 + 世界模型）
        """
        decisions = {}
        
        for code in ["00700", "09988", "03690"]:
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
            
            # 世界模型增强
            if prediction and prediction.get('enabled'):
                if prediction['recommendation'] == action:
                    conf = min(0.9, conf + 0.2)
                elif prediction['confidence'] > 0.8:
                    # 世界模型高置信度，覆盖规则
                    action = prediction['recommendation']
                    conf = prediction['confidence']
            
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
    print(f"📁 数据保存于: /opt/hktech-agent/data/")
    print("\n下一步:")
    print("1. 接入真实新闻API (如新浪财经、华尔街见闻)")
    print("2. 接入真实LLM API (OpenAI/Claude)")
    print("3. 部署到定时任务，每日自动运行")


if __name__ == "__main__":
    main()
