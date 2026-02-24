#!/usr/bin/env python3
"""
LLM决策增强模块
增强基础策略决策，提供更智能的分析和报告
支持 DeepSeek API 或基于规则的 fallback（无随机数）
"""

import json
import os
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
    LLM决策增强器
    支持 DeepSeek API 调用，或基于规则的 fallback（均无随机数）
    """

    def __init__(self, data_dir=None):
        if data_dir is None:
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

        print(f"🧠 LLM决策增强器初始化")

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def enhance_decision(self,
                         base_decision: Dict[str, Any],
                         market_data: Dict[str, Dict],
                         portfolio: Dict[str, Any],
                         prediction: Optional[Dict[str, Any]] = None,
                         llm_signals: Optional[Dict[str, float]] = None) -> Dict[str, Any]:
        """
        增强基础决策，提供更智能的分析。

        Args:
            base_decision: 基础策略决策，格式可为
                           {code: {"action": ..., "confidence": ...}}
                           或 {"decisions": {code: {...}}, "summary": ...}
            market_data:   市场数据
            portfolio:     当前投资组合
            prediction:    世界模型预测结果（可选）
            llm_signals:   LLM情绪信号（可选）

        Returns:
            Dict: 增强决策结果
        """
        # 兼容两种 base_decision 格式
        if "decisions" in base_decision:
            decisions_map = base_decision["decisions"]
        else:
            decisions_map = base_decision

        print(f"   🤔 分析 {len(decisions_map)} 只股票的决策...")

        # LLM 分析（确定性）
        llm_output = self._simulate_llm_analysis(
            decisions_map, market_data, portfolio, prediction, llm_signals
        )

        # 最终决策（确定性，使用加权合并）
        final_decision = self._generate_final_decision(
            decisions_map, llm_output, market_data, llm_signals
        )

        result = {
            "llm_output": llm_output,
            "final_decision": final_decision,
            "timestamp": datetime.now().isoformat(),
            "enhancement_score": llm_output.get("confidence", 0.7),
        }

        self._save_decision_record(result)
        return result

    # ------------------------------------------------------------------
    # DeepSeek API
    # ------------------------------------------------------------------

    def _call_deepseek_decision(self, stock_code: str, technical_signal: dict,
                                predicted_return: float, sentiment: float) -> dict:
        """调用 DeepSeek 做单股决策，失败时返回基于规则的 fallback"""
        import requests

        api_key = os.environ.get("DEEPSEEK_API_KEY", "")
        if not api_key:
            return self._rule_based_decision(technical_signal, predicted_return, sentiment)

        stock_name = self.stock_names.get(stock_code, stock_code) if hasattr(self, "stock_names") else stock_code
        rsi = technical_signal.get("rsi", 50)
        trend = technical_signal.get("trend", "中性")
        prompt = (
            f"你是一位专业的港股量化交易决策助手。综合以下信息给出交易建议：\n\n"
            f"股票：{stock_name}({stock_code})\n"
            f"技术信号：RSI={rsi:.1f}, 趋势={trend}\n"
            f"世界模型预测5日收益：{predicted_return:.2%}\n"
            f"市场情感得分：{sentiment:.2f}（0=极度悲观，1=极度乐观）\n\n"
            "请返回JSON（只返回JSON，不要其他文字）：\n"
            '{"action":"BUY/SELL/HOLD","confidence":0到1之间的浮点数,'
            '"reasoning":"50字以内的中文理由","risk_level":"LOW/MEDIUM/HIGH"}'
        )
        try:
            resp = requests.post(
                "https://api.deepseek.com/v1/chat/completions",
                headers={"Authorization": f"Bearer {api_key}",
                         "Content-Type": "application/json"},
                json={"model": "deepseek-chat",
                      "messages": [{"role": "user", "content": prompt}],
                      "temperature": 0.1, "max_tokens": 200},
                timeout=30,
            )
            content = resp.json()["choices"][0]["message"]["content"].strip()
            if "{" in content:
                content = content[content.index("{"):content.rindex("}") + 1]
            result = json.loads(content)
            result["action"] = result["action"].upper()
            result["confidence"] = max(0.0, min(1.0, float(result["confidence"])))
            return result
        except Exception as e:
            print(f"⚠️ DeepSeek 决策 API 失败 ({stock_code}): {e}")
            return self._rule_based_decision(technical_signal, predicted_return, sentiment)

    # ------------------------------------------------------------------
    # Rule-based fallback (no random)
    # ------------------------------------------------------------------

    def _rule_based_decision(self, technical_signal: dict,
                              predicted_return: float, sentiment: float) -> dict:
        """规则 fallback（无随机数）"""
        confidence = technical_signal.get("confidence", 0.5)
        world_score = max(0.0, min(1.0, predicted_return * 10 + 0.5))
        score = confidence * 0.4 + world_score * 0.3 + sentiment * 0.3
        if score > 0.6:
            action = "BUY"
        elif score < 0.4:
            action = "SELL"
        else:
            action = "HOLD"
        return {"action": action, "confidence": round(score, 3),
                "reasoning": "基于技术指标+预测+情感的规则决策", "risk_level": "MEDIUM"}

    # ------------------------------------------------------------------
    # Weighted signal merge
    # ------------------------------------------------------------------

    def _merge_signals(self, tech_confidence: float, tech_action: str,
                       world_confidence: float, world_action: str,
                       sentiment_score: float) -> dict:
        """三路信号加权合并: tech×0.4 + world×0.3 + sentiment×0.3"""
        action_score = {"buy": 1.0, "BUY": 1.0, "sell": 0.0, "SELL": 0.0,
                        "hold": 0.5, "HOLD": 0.5}
        tech_s = action_score.get(tech_action, 0.5) * tech_confidence
        world_s = action_score.get(world_action, 0.5) * world_confidence
        merged = tech_s * 0.4 + world_s * 0.3 + sentiment_score * 0.3
        if merged > 0.6:
            action = "buy"
        elif merged < 0.4:
            action = "sell"
        else:
            action = "hold"
        return {"action": action, "confidence": round(merged, 4)}

    # ------------------------------------------------------------------
    # Internal analysis helpers (deterministic)
    # ------------------------------------------------------------------

    def _simulate_llm_analysis(self, base_decision, market_data, portfolio, prediction, llm_signals):
        """确定性 LLM 分析过程（无随机数）"""
        analysis_parts = []

        market_summary = self._analyze_market_summary(market_data)
        analysis_parts.append(f"📊 市场概况: {market_summary}")

        for code in self.stocks:
            if code in base_decision:
                action = base_decision[code]["action"]
                confidence = base_decision[code].get("confidence", 0.5)
                stock_analysis = self._analyze_stock(
                    code, action, confidence, market_data.get(code, {})
                )
                analysis_parts.append(f"📈 {self.stock_names.get(code, code)}: {stock_analysis}")

        risk_analysis = self._analyze_portfolio_risk(portfolio, market_data)
        analysis_parts.append(f"🛡️  风险分析: {risk_analysis}")

        if prediction and prediction.get("enabled"):
            pred_analysis = (
                f"世界模型预测: {prediction.get('recommendation', '未知')}, "
                f"置信度{prediction.get('confidence', 0):.0%}"
            )
            analysis_parts.append(f"🔮 {pred_analysis}")

        if llm_signals:
            sentiment_analysis = self._analyze_sentiment_signals(llm_signals)
            analysis_parts.append(f"😊 情绪分析: {sentiment_analysis}")

        full_analysis = "\n".join(analysis_parts)

        # 置信度由情感信号或默认值确定（无随机）
        if llm_signals:
            sentiment_values = [v for k, v in llm_signals.items() if k.endswith("_sentiment")]
            overall_confidence = sum(sentiment_values) / len(sentiment_values) if sentiment_values else 0.7
        else:
            overall_confidence = 0.7

        return {
            "analysis": full_analysis,
            "summary": "基于技术分析、基本面评估和市场情绪的综合判断",
            "confidence": round(overall_confidence, 4),
            "risk_level": "中",
            "suggested_position": 0.5,
        }

    def _generate_final_decision(self, base_decision: Dict, llm_output: Dict,
                                  market_data: Dict,
                                  llm_signals: Optional[Dict[str, float]] = None) -> Dict:
        """生成最终决策，使用加权信号合并（无随机数）"""
        final_decision = {}

        for code, base_dec in base_decision.items():
            tech_action = base_dec["action"]
            tech_confidence = base_dec.get("confidence", 0.5)

            # 获取情感分数（确定性）
            sentiment_score = 0.5
            if llm_signals:
                key = f"{code}_sentiment"
                if key in llm_signals:
                    sentiment_score = llm_signals[key]

            # 世界模型置信度（来自 llm_output，确定性）
            world_confidence = llm_output.get("confidence", 0.5)
            world_action = tech_action  # 默认与技术动作一致

            # 加权合并
            merged = self._merge_signals(
                tech_confidence=tech_confidence,
                tech_action=tech_action,
                world_confidence=world_confidence,
                world_action=world_action,
                sentiment_score=sentiment_score,
            )

            reason = self._generate_reason(
                code, merged["action"], merged["confidence"], market_data.get(code, {})
            )

            final_decision[code] = {
                "action": merged["action"],
                "confidence": merged["confidence"],
                "reason": reason,
                "base_decision": base_dec,
                "enhanced": True,
            }

        return final_decision

    def _analyze_market_summary(self, market_data: Dict) -> str:
        """分析市场概况"""
        if not market_data:
            return "市场数据缺失"

        changes = []
        for code in self.stocks:
            if code in market_data:
                change = market_data[code].get("change_pct", 0)
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
        analysis = f"建议{action}，置信度{confidence:.0%}"

        if stock_data:
            rsi = stock_data.get("rsi", 50)
            if rsi > 70:
                analysis += f"，RSI({rsi:.0f})超买"
            elif rsi < 30:
                analysis += f"，RSI({rsi:.0f})超卖"
            else:
                analysis += f"，RSI({rsi:.0f})中性"

            change = stock_data.get("change_pct", 0)
            if abs(change) > 2:
                analysis += f"，今日涨跌{change:+.1f}%"

        return analysis

    def _analyze_portfolio_risk(self, portfolio: Dict, market_data: Dict) -> str:
        """分析组合风险"""
        holdings = portfolio.get("holdings", {})

        if not holdings:
            return "无持仓，现金比例100%"

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

    def _generate_reason(self, code: str, action: str, confidence: float, stock_data: Dict) -> str:
        """生成决策理由（确定性，无随机）"""
        action_lower = action.lower()
        reason_map = {
            "buy": "技术指标综合偏多，建议买入",
            "sell": "技术指标综合偏空，建议卖出",
            "hold": "技术指标中性，建议观望",
        }
        reason = reason_map.get(action_lower, "基于综合分析")

        details = []
        if stock_data:
            rsi = stock_data.get("rsi", 50)
            if rsi > 70:
                details.append(f"RSI={rsi:.0f}(超买)")
            elif rsi < 30:
                details.append(f"RSI={rsi:.0f}(超卖)")

            change = stock_data.get("change_pct", 0)
            if abs(change) > 1:
                details.append(f"涨跌{change:+.1f}%")

        if details:
            reason += f"，技术面：{'，'.join(details)}"

        reason += f"，决策置信度{confidence:.0%}"
        return reason

    # ------------------------------------------------------------------
    # Report & persistence
    # ------------------------------------------------------------------

    def _save_decision_record(self, result: Dict):
        """保存决策记录"""
        try:
            record_file = f"{self.data_dir}/decision_records.json"

            records = []
            if os.path.exists(record_file):
                with open(record_file, "r") as f:
                    records = json.load(f)

            records.append(result)
            if len(records) > 100:
                records = records[-100:]

            with open(record_file, "w") as f:
                json.dump(records, f, indent=2)

        except Exception as e:
            print(f"   ⚠️ 保存决策记录失败: {e}")

    def _calculate_enhanced_confidence(self, base_confidence: float, sentiment: float) -> float:
        """计算增强后的置信度: base×0.5 + sentiment×0.5"""
        enhanced = base_confidence * 0.5 + sentiment * 0.5
        return max(0.0, min(1.0, enhanced))


if __name__ == "__main__":
    enhancer = LLMDecisionEnhancer()

    test_base_decision = {
        "00700": {"action": "buy", "confidence": 0.7},
        "09988": {"action": "hold", "confidence": 0.5},
        "03690": {"action": "sell", "confidence": 0.6},
    }

    test_market_data = {
        "00700": {"price": 385, "ma5": 382, "ma20": 375, "rsi": 65, "change_pct": 1.5},
        "09988": {"price": 85, "ma5": 84, "ma20": 86, "rsi": 45, "change_pct": -0.8},
        "03690": {"price": 130, "ma5": 128, "ma20": 125, "rsi": 70, "change_pct": 2.1},
    }

    test_portfolio = {
        "cash": 19000,
        "holdings": {
            "00700": {"shares": 10, "avg_price": 380},
            "03690": {"shares": 20, "avg_price": 125},
        },
        "total_value": 21000,
    }

    enhanced = enhancer.enhance_decision(test_base_decision, test_market_data, test_portfolio)
    print(f"\n最终决策:")
    for code, dec in enhanced["final_decision"].items():
        print(f"  {code}: {dec['action']} conf={dec['confidence']:.4f}")
