#!/usr/bin/env python3
"""
LLM决策增强模块 (模拟版本)
增强基础策略决策，提供更智能的分析和报告
"""

import json
import os
import random
import sys
from datetime import datetime
from typing import Dict, List, Any, Optional

# 导入共享常量
SHARED_CONSTANTS_AVAILABLE = False
constants = None  # 默认值
try:
    import constants
    SHARED_CONSTANTS_AVAILABLE = True
except ImportError:
    print("⚠️ 共享常量模块不可用，使用本地定义")


class LLMDecisionEnhancer:
    """
    LLM决策增强器 - 模拟版本
    真实部署时需要替换为实际LLM API调用
    """
    
    def __init__(self, data_dir=None):
        if data_dir is None:
            # 默认使用当前目录下的data文件夹
            current_dir = os.path.dirname(os.path.abspath(__file__))
            data_dir = os.path.join(current_dir, "../../data")
        self.data_dir = data_dir
        os.makedirs(data_dir, exist_ok=True)
        
        # 股票信息（使用共享常量或本地定义）
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
        
        # 决策理由模板
        self.reason_templates = {
            "buy": [
                "技术指标显示超卖，RSI低于30，存在反弹机会",
                "股价接近重要支撑位，风险收益比合适",
                "基本面稳健，估值处于历史低位",
                "市场情绪过度悲观，提供买入机会",
                "资金流向显示机构增持迹象"
            ],
            "sell": [
                "技术指标显示超买，RSI高于70，存在回调风险",
                "股价接近重要阻力位，上涨空间有限",
                "获利盘较多，有获利了结压力",
                "基本面出现恶化迹象",
                "市场情绪过度乐观，存在泡沫风险"
            ],
            "hold": [
                "股价在合理区间震荡，建议观望",
                "技术指标中性，无明显方向信号",
                "等待更明确的突破信号",
                "基本面平稳，无重大变化",
                "市场多空力量平衡"
            ]
        }
        
        print(f"🧠 LLM决策增强器初始化 (模拟模式)")
    
    def enhance_decision(self, 
                         base_decision: Dict[str, Dict],
                         market_data: Dict[str, Dict],
                         portfolio: Dict[str, Any],
                         prediction: Optional[Dict[str, Any]] = None,
                         llm_signals: Optional[Dict[str, float]] = None) -> Dict[str, Any]:
        """
        增强基础决策，提供更智能的分析
        
        Args:
            base_decision: 基础策略决策 {code: {"action": "buy/sell/hold", "confidence": float}}
            market_data: 市场数据
            portfolio: 当前投资组合
            prediction: 世界模型预测结果（可选）
            llm_signals: LLM情绪信号（可选）
            
        Returns:
            Dict: 增强决策结果
        """
        print(f"   🤔 分析 {len(base_decision)} 只股票的决策...")
        
        # 模拟LLM分析过程
        llm_output = self._simulate_llm_analysis(
            base_decision, market_data, portfolio, prediction, llm_signals
        )
        
        # 生成最终决策（可基于LLM分析调整基础决策）
        final_decision = self._generate_final_decision(
            base_decision, llm_output, market_data
        )
        
        result = {
            "llm_output": llm_output,
            "final_decision": final_decision,
            "timestamp": datetime.now().isoformat(),
            "enhancement_score": random.uniform(0.6, 0.9)  # 模拟增强效果评分
        }
        
        # 保存决策记录
        self._save_decision_record(result)
        
        return result
    
    def _simulate_llm_analysis(self, base_decision, market_data, portfolio, prediction, llm_signals):
        """模拟LLM分析过程"""
        analysis_parts = []
        
        # 1. 市场概况分析
        market_summary = self._analyze_market_summary(market_data)
        analysis_parts.append(f"📊 市场概况: {market_summary}")
        
        # 2. 个股分析
        for code in self.stocks:
            if code in base_decision:
                action = base_decision[code]["action"]
                confidence = base_decision[code].get("confidence", 0.5)
                stock_analysis = self._analyze_stock(code, action, confidence, market_data.get(code, {}))
                analysis_parts.append(f"📈 {self.stock_names.get(code, code)}: {stock_analysis}")
        
        # 3. 组合风险分析
        risk_analysis = self._analyze_portfolio_risk(portfolio, market_data)
        analysis_parts.append(f"🛡️  风险分析: {risk_analysis}")
        
        # 4. 预测集成分析
        if prediction and prediction.get('enabled'):
            pred_analysis = f"世界模型预测: {prediction.get('recommendation', '未知')}, 置信度{prediction.get('confidence', 0):.0%}"
            analysis_parts.append(f"🔮 {pred_analysis}")
        
        # 5. 情绪信号分析
        if llm_signals:
            sentiment_analysis = self._analyze_sentiment_signals(llm_signals)
            analysis_parts.append(f"😊 情绪分析: {sentiment_analysis}")
        
        # 合并分析
        full_analysis = "\n".join(analysis_parts)
        
        return {
            "analysis": full_analysis,
            "summary": "基于技术分析、基本面评估和市场情绪的综合判断",
            "confidence": random.uniform(0.6, 0.85),
            "risk_level": random.choice(["低", "中", "中高"]),
            "suggested_position": random.uniform(0.3, 0.7)
        }
    
    def _analyze_market_summary(self, market_data: Dict) -> str:
        """分析市场概况"""
        if not market_data:
            return "市场数据缺失"
        
        changes = []
        for code in self.stocks:
            if code in market_data:
                change = market_data[code].get('change_pct', 0)
                changes.append(change)
        
        if changes:
            avg_change = sum(changes) / len(changes)
            if avg_change > 1:
                return f"市场整体上涨{avg_change:.1f}%，情绪偏多"
            elif avg_change < -1:
                return f"市场整体下跌{abs(avg_change):.1f}%，情绪偏空"
            else:
                return f"市场震荡，平均涨跌{avg_change:.1f}%"
        return "市场平稳"
    
    def _analyze_stock(self, code: str, action: str, confidence: float, stock_data: Dict) -> str:
        """分析单只股票"""
        name = self.stock_names.get(code, code)
        
        analysis = f"建议{action}，置信度{confidence:.0%}"
        
        if stock_data:
            rsi = stock_data.get('rsi', 50)
            if rsi > 70:
                analysis += f"，RSI({rsi:.0f})超买"
            elif rsi < 30:
                analysis += f"，RSI({rsi:.0f})超卖"
            else:
                analysis += f"，RSI({rsi:.0f})中性"
            
            change = stock_data.get('change_pct', 0)
            if abs(change) > 2:
                analysis += f"，今日涨跌{change:+.1f}%"
        
        return analysis
    
    def _analyze_portfolio_risk(self, portfolio: Dict, market_data: Dict) -> str:
        """分析组合风险"""
        cash = portfolio.get('cash', 0)
        holdings = portfolio.get('holdings', {})
        
        if not holdings:
            return "无持仓，现金比例100%"
        
        # 简单风险评估
        holding_count = len(holdings)
        if holding_count <= 1:
            risk = "集中度高"
        elif holding_count <= 3:
            risk = "适度分散"
        else:
            risk = "分散良好"
        
        return f"持仓{holding_count}只股票，{risk}"
    
    def _analyze_sentiment_signals(self, llm_signals: Dict[str, float]) -> str:
        """分析情绪信号"""
        sentiments = []
        for code in self.stocks:
            key = f"{code}_sentiment"
            if key in llm_signals:
                sentiment = llm_signals[key]
                if sentiment > 0.6:
                    sentiments.append("乐观")
                elif sentiment < 0.4:
                    sentiments.append("悲观")
                else:
                    sentiments.append("中性")
        
        if sentiments:
            return f"情绪分布: {', '.join(sentiments)}"
        return "情绪信号缺失"
    
    def _generate_final_decision(self, base_decision: Dict, llm_output: Dict, market_data: Dict) -> Dict:
        """生成最终决策"""
        final_decision = {}
        
        for code, base_dec in base_decision.items():
            action = base_dec["action"]
            confidence = base_dec.get("confidence", 0.5)
            
            # 可基于LLM分析调整决策（这里简单保持原决策）
            final_action = action
            
            # 生成理由
            reason = self._generate_reason(code, final_action, confidence, market_data.get(code, {}))
            
            final_decision[code] = {
                "action": final_action,
                "confidence": confidence,
                "reason": reason,
                "base_decision": base_dec,
                "enhanced": True
            }
        
        return final_decision
    
    def _generate_reason(self, code: str, action: str, confidence: float, stock_data: Dict) -> str:
        """生成决策理由"""
        templates = self.reason_templates.get(action, ["基于综合分析"])
        reason = random.choice(templates)
        
        # 添加具体数据支持
        details = []
        if stock_data:
            rsi = stock_data.get('rsi', 50)
            if rsi > 70:
                details.append(f"RSI={rsi:.0f}(超买)")
            elif rsi < 30:
                details.append(f"RSI={rsi:.0f}(超卖)")
            
            change = stock_data.get('change_pct', 0)
            if abs(change) > 1:
                details.append(f"涨跌{change:+.1f}%")
        
        if details:
            reason += f"，技术面：{'，'.join(details)}"
        
        reason += f"，决策置信度{confidence:.0%}"
        
        return reason
    
    def generate_daily_report(self, enhanced_result: Dict[str, Any]) -> str:
        """
        生成每日投资报告
        
        Args:
            enhanced_result: enhance_decision返回的结果
            
        Returns:
            str: 格式化的报告文本
        """
        timestamp = enhanced_result.get('timestamp', datetime.now().isoformat())
        llm_output = enhanced_result.get('llm_output', {})
        final_decision = enhanced_result.get('final_decision', {})
        
        # 构建报告
        report_lines = []
        
        # 头部
        report_lines.append("=" * 60)
        report_lines.append("📊 恒生科技Agent - 每日投资报告")
        report_lines.append(f"⏰ 生成时间: {timestamp}")
        report_lines.append("=" * 60)
        report_lines.append("")
        
        # 1. 执行摘要
        report_lines.append("📋 执行摘要")
        report_lines.append("-" * 40)
        
        actions = {"buy": 0, "sell": 0, "hold": 0}
        for code, dec in final_decision.items():
            action = dec.get("action", "hold")
            if action in actions:
                actions[action] += 1
        
        summary = f"今日建议: 买入{actions['buy']}只，卖出{actions['sell']}只，持有{actions['hold']}只"
        report_lines.append(summary)
        report_lines.append(f"增强评分: {enhanced_result.get('enhancement_score', 0):.1%}")
        report_lines.append("")
        
        # 2. LLM分析摘要
        report_lines.append("🧠 LLM分析摘要")
        report_lines.append("-" * 40)
        analysis = llm_output.get('analysis', '无分析结果')
        # 限制长度
        if len(analysis) > 300:
            analysis = analysis[:300] + "..."
        report_lines.append(analysis)
        report_lines.append("")
        
        # 3. 详细决策
        report_lines.append("🎯 详细投资决策")
        report_lines.append("-" * 40)
        
        for code, dec in final_decision.items():
            name = self.stock_names.get(code, code)
            action = dec.get("action", "hold").upper()
            confidence = dec.get("confidence", 0)
            reason = dec.get("reason", "无详细理由")
            
            report_lines.append(f"📈 {name} ({code})")
            report_lines.append(f"   决策: {action} (置信度: {confidence:.0%})")
            report_lines.append(f"   理由: {reason}")
            report_lines.append("")
        
        # 4. 风险提示
        report_lines.append("⚠️  风险提示")
        report_lines.append("-" * 40)
        risk_level = llm_output.get('risk_level', '中')
        report_lines.append(f"综合风险等级: {risk_level}")
        report_lines.append("• 股市有风险，投资需谨慎")
        report_lines.append("• 本报告为AI生成，仅供参考")
        report_lines.append("• 实际投资请结合个人风险承受能力")
        report_lines.append("")
        
        # 5. 后续建议
        report_lines.append("📅 后续建议")
        report_lines.append("-" * 40)
        report_lines.append("• 建议每日关注市场动态")
        report_lines.append("• 重要财报发布前调整仓位")
        report_lines.append("• 设置止损止盈，控制风险")
        report_lines.append("")
        
        report_lines.append("=" * 60)
        report_lines.append("📱 更多功能请访问: http://60.205.245.131:8080")
        report_lines.append("=" * 60)
        
        return "\n".join(report_lines)
    
    def _save_decision_record(self, result: Dict):
        """保存决策记录"""
        try:
            record_file = f"{self.data_dir}/decision_records.json"
            
            records = []
            if os.path.exists(record_file):
                with open(record_file, 'r') as f:
                    records = json.load(f)
            
            # 只保留最近100条记录
            records.append(result)
            if len(records) > 100:
                records = records[-100:]
            
            with open(record_file, 'w') as f:
                json.dump(records, f, indent=2)
                
        except Exception as e:
            print(f"   ⚠️ 保存决策记录失败: {e}")
    
    def _calculate_enhanced_confidence(self, base_confidence: float, sentiment: float) -> float:
        """
        计算增强后的置信度
        简单的加权平均: base_confidence * 0.5 + sentiment * 0.5
        """
        enhanced = base_confidence * 0.5 + sentiment * 0.5
        return max(0.0, min(1.0, enhanced))


if __name__ == "__main__":
    # 测试模块
    enhancer = LLMDecisionEnhancer()
    
    # 测试数据
    test_base_decision = {
        "00700": {"action": "buy", "confidence": 0.7},
        "09988": {"action": "hold", "confidence": 0.5},
        "03690": {"action": "sell", "confidence": 0.6}
    }
    
    test_market_data = {
        "00700": {"price": 385, "ma5": 382, "ma20": 375, "rsi": 65, "change_pct": 1.5},
        "09988": {"price": 85, "ma5": 84, "ma20": 86, "rsi": 45, "change_pct": -0.8},
        "03690": {"price": 130, "ma5": 128, "ma20": 125, "rsi": 70, "change_pct": 2.1}
    }
    
    test_portfolio = {
        "cash": 19000,
        "holdings": {
            "00700": {"shares": 10, "avg_price": 380},
            "03690": {"shares": 20, "avg_price": 125}
        },
        "total_value": 21000
    }
    
    test_prediction = {
        'enabled': True,
        'horizon': 5,
        'cumulative_return': 1.2,
        'confidence': 0.8,
        'recommendation': '持有'
    }
    
    test_llm_signals = {
        '00700_sentiment': 0.6,
        '09988_sentiment': 0.45,
        '03690_sentiment': 0.53
    }
    
    print("🧪 测试LLM决策增强...")
    enhanced = enhancer.enhance_decision(
        test_base_decision, test_market_data, test_portfolio,
        test_prediction, test_llm_signals
    )
    
    print(f"📊 增强结果:")
    print(f"  LLM分析摘要: {enhanced['llm_output']['analysis'][:100]}...")
    
    print(f"\n🎯 最终决策:")
    for code, dec in enhanced['final_decision'].items():
        print(f"  {code}: {dec['action']} - {dec['reason'][:50]}...")
    
    print(f"\n📄 生成报告...")
    report = enhancer.generate_daily_report(enhanced)
    print(report[:500] + "..." if len(report) > 500 else report)