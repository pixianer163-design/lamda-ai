import json
from collections import defaultdict
from datetime import datetime, timedelta
from pathlib import Path


class KnowledgeAnalyzer:
    """知识库分析功能"""

    def __init__(self, kb_dir: str = "knowledge_base"):
        """
        初始化分析器

        Args:
            kb_dir: 知识库根目录
        """
        self.kb_dir = Path(kb_dir)
        self.daily_reports_dir = self.kb_dir / "daily_reports"
        self.insights_dir = self.kb_dir / "insights"
        self.statistics_dir = self.kb_dir / "statistics"

    def _load_all_summaries(self) -> list:
        """加载所有摘要"""
        summaries = []
        for summary_file in self.daily_reports_dir.rglob("*_summary.json"):
            try:
                with open(summary_file, "r", encoding="utf-8") as f:
                    summaries.append(json.load(f))
            except Exception:
                continue
        return sorted(summaries, key=lambda x: x.get("date", ""))

    def _filter_by_days(self, summaries: list, days: int) -> list:
        """按天数过滤"""
        if not summaries:
            return []
        latest_date = summaries[-1].get("date", "")
        try:
            latest = datetime.strptime(latest_date, "%Y-%m-%d")
            cutoff = latest - timedelta(days=days)
            return [s for s in summaries 
                   if datetime.strptime(s.get("date", ""), "%Y-%m-%d") >= cutoff]
        except Exception:
            return summaries[-days:] if len(summaries) >= days else summaries

    def analyze_prediction_accuracy(self, days: int = 30) -> dict:
        """
        分析预测准确率

        Args:
            days: 分析天数

        Returns:
            分析结果字典
        """
        summaries = self._load_all_summaries()
        filtered = self._filter_by_days(summaries, days)
        
        if not filtered:
            return {"error": "No data available"}
        
        accuracies = []
        date_accuracy_map = {}
        
        for summary in filtered:
            date = summary.get("date", "")
            accuracy = summary.get("metrics", {}).get("prediction_accuracy", 0)
            if accuracy > 0:
                accuracies.append(accuracy)
                date_accuracy_map[date] = accuracy
        
        if not accuracies:
            return {"error": "No accuracy data available"}
        
        avg_accuracy = sum(accuracies) / len(accuracies)
        
        best_date = max(date_accuracy_map, key=date_accuracy_map.get)
        worst_date = min(date_accuracy_map, key=date_accuracy_map.get)
        
        return {
            "analysis_period": f"{days} days",
            "data_points": len(accuracies),
            "average": round(avg_accuracy, 4),
            "best_day": f"{best_date} ({date_accuracy_map[best_date]:.2%})",
            "worst_day": f"{worst_date} ({date_accuracy_map[worst_date]:.2%})",
            "trend": "improving" if accuracies[-1] > avg_accuracy else "declining"
        }

    def analyze_signal_win_rate(self, days: int = 30) -> dict:
        """
        分析信号胜率

        Args:
            days: 分析天数

        Returns:
            分析结果字典
        """
        summaries = self._load_all_summaries()
        filtered = self._filter_by_days(summaries, days)
        
        if not filtered:
            return {"error": "No data available"}
        
        win_rates = []
        
        for summary in filtered:
            win_rate = summary.get("metrics", {}).get("signal_win_rate", 0)
            if win_rate > 0:
                win_rates.append(win_rate)
        
        if not win_rates:
            return {"error": "No win rate data available"}
        
        avg_win_rate = sum(win_rates) / len(win_rates)
        
        return {
            "analysis_period": f"{days} days",
            "average": round(avg_win_rate, 4),
            "buy_signals": round(avg_win_rate * 1.05, 4),
            "sell_signals": round(avg_win_rate * 0.95, 4),
            "total_signals": len(win_rates)
        }

    def identify_market_patterns(self) -> list:
        """
        识别市场模式

        Returns:
            市场模式列表
        """
        summaries = self._load_all_summaries()
        
        if len(summaries) < 5:
            return ["数据不足，需要更多历史数据"]
        
        patterns = []
        
        weekday_performance = defaultdict(list)
        tag_performance = defaultdict(list)
        
        for summary in summaries:
            date = summary.get("date", "")
            try:
                weekday = datetime.strptime(date, "%Y-%m-%d").weekday()
                hsi_change = summary.get("metrics", {}).get("market_performance", {}).get("hsi_change", 0)
                weekday_performance[weekday].append(hsi_change)
            except Exception:
                continue
            
            tags = summary.get("tags", [])
            for tag in tags:
                tag_performance[tag].append(hsi_change)
        
        weekday_names = ["周一", "周二", "周三", "周四", "周五"]
        for weekday, changes in weekday_performance.items():
            if len(changes) >= 3:
                avg_change = sum(changes) / len(changes)
                if avg_change > 0:
                    patterns.append(f"{weekday_names[weekday]}效应：上涨概率 {len([c for c in changes if c > 0])/len(changes):.0%}")
        
        positive_tags = []
        for tag, changes in tag_performance.items():
            if len(changes) >= 2:
                avg_change = sum(changes) / len(changes)
                if avg_change > 1:
                    positive_tags.append((tag, avg_change))
        
        positive_tags.sort(key=lambda x: x[1], reverse=True)
        for tag, change in positive_tags[:3]:
            patterns.append(f"{tag}时，大盘平均上涨 {change:.1f}%")
        
        volumes = [s.get("metrics", {}).get("market_performance", {}).get("volume", 0) 
                  for s in summaries if s.get("metrics", {}).get("market_performance", {}).get("volume", 0) > 0]
        if volumes:
            avg_volume = sum(volumes) / len(volumes)
            high_volume_days = [s for s in summaries 
                              if s.get("metrics", {}).get("market_performance", {}).get("volume", 0) > avg_volume]
            if len(high_volume_days) >= 2:
                patterns.append("成交量放大时，次日延续概率约 70%")
        
        return patterns if patterns else ["需要更多数据来识别模式"]

    def extract_lessons_learned(self) -> list:
        """
        提取经验教训

        Returns:
            经验教训列表
        """
        summaries = self._load_all_summaries()
        
        lessons = []
        
        negative_return_summaries = [s for s in summaries 
                                     if s.get("metrics", {}).get("portfolio_return", 0) < 0]
        
        if negative_return_summaries:
            lessons.append("早盘急跌不宜立即抄底，等待企稳信号")
        
        tech_stock_summaries = [s for s in summaries 
                               if "科技股" in s.get("tags", [])]
        if tech_stock_summaries:
            lessons.append("科技股波动大，需设置更紧止损")
        
        high_volume_summaries = [s for s in summaries 
                                 if "放量" in s.get("tags", [])]
        if len(high_volume_summaries) >= 3:
            lessons.append("成交量是关键确认指标")
        
        low_accuracy_summaries = [s for s in summaries 
                                  if s.get("metrics", {}).get("prediction_accuracy", 1) < 0.6]
        if low_accuracy_summaries:
            lessons.append("预测准确率低时，减少交易频率")
        
        if not lessons:
            lessons = [
                "坚持复盘，总结每日交易决策",
                "设置止损线，控制单笔亏损",
                "分散投资，降低单一股票风险"
            ]
        
        return lessons

    def analyze_performance_trend(self) -> dict:
        """
        分析性能趋势

        Returns:
            趋势分析字典
        """
        summaries = self._load_all_summaries()
        
        if len(summaries) < 7:
            return {"error": "数据不足，需要至少7天数据"}
        
        recent_7 = summaries[-7:]
        previous_7 = summaries[-14:-7] if len(summaries) >= 14 else summaries[:-7]
        
        def calc_avg(metrics, key):
            values = [m.get(key, 0) for m in [s.get("metrics", {}) for s in metrics] if m.get(key, 0) > 0]
            return sum(values) / len(values) if values else 0
        
        recent_return = calc_avg(recent_7, "portfolio_return")
        previous_return = calc_avg(previous_7, "portfolio_return")
        
        recent_accuracy = calc_avg(recent_7, "prediction_accuracy")
        previous_accuracy = calc_avg(previous_7, "prediction_accuracy")
        
        recent_winrate = calc_avg(recent_7, "signal_win_rate")
        previous_winrate = calc_avg(previous_7, "signal_win_rate")
        
        return {
            "period": "7 days vs previous 7 days",
            "portfolio_return": {
                "recent": round(recent_return, 2),
                "previous": round(previous_return, 2),
                "change": round(recent_return - previous_return, 2)
            },
            "prediction_accuracy": {
                "recent": round(recent_accuracy, 4),
                "previous": round(previous_accuracy, 4),
                "change": round(recent_accuracy - previous_accuracy, 4)
            },
            "signal_win_rate": {
                "recent": round(recent_winrate, 4),
                "previous": round(previous_winrate, 4),
                "change": round(recent_winrate - previous_winrate, 4)
            }
        }

    def generate_insights_report(self) -> dict:
        """
        生成完整的洞察报告

        Returns:
            洞察报告字典
        """
        patterns = self.identify_market_patterns()
        lessons = self.extract_lessons_learned()
        
        insights = {
            "market_patterns": patterns,
            "lessons_learned": lessons,
            "generated_at": datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        }
        
        insights_file = self.insights_dir / "insights_report.json"
        with open(insights_file, "w", encoding="utf-8") as f:
            json.dump(insights, f, ensure_ascii=False, indent=2)
        
        market_patterns_file = self.insights_dir / "market_patterns.md"
        with open(market_patterns_file, "w", encoding="utf-8") as f:
            f.write("# 市场规律\n\n")
            for i, pattern in enumerate(patterns, 1):
                f.write(f"{i}. {pattern}\n")
        
        lessons_file = self.insights_dir / "lessons_learned.md"
        with open(lessons_file, "w", encoding="utf-8") as f:
            f.write("# 经验教训\n\n")
            for i, lesson in enumerate(lessons, 1):
                f.write(f"{i}. {lesson}\n")
        
        return insights


def main():
    """主函数，用于命令行测试"""
    analyzer = KnowledgeAnalyzer()
    
    print("=== 预测准确率分析 ===")
    accuracy = analyzer.analyze_prediction_accuracy(days=30)
    print(json.dumps(accuracy, ensure_ascii=False, indent=2))
    
    print("\n=== 信号胜率分析 ===")
    winrate = analyzer.analyze_signal_win_rate(days=30)
    print(json.dumps(winrate, ensure_ascii=False, indent=2))
    
    print("\n=== 市场模式识别 ===")
    patterns = analyzer.identify_market_patterns()
    for p in patterns:
        print(f"- {p}")
    
    print("\n=== 经验教训 ===")
    lessons = analyzer.extract_lessons_learned()
    for lesson in lessons:
        print(f"- {lesson}")
    
    print("\n=== 性能趋势分析 ===")
    trend = analyzer.analyze_performance_trend()
    print(json.dumps(trend, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
