import json
from datetime import datetime
from pathlib import Path
from typing import Optional


class KnowledgeSearch:
    """知识库搜索功能"""

    def __init__(self, kb_dir: str = "knowledge_base"):
        """
        初始化搜索器

        Args:
            kb_dir: 知识库根目录
        """
        self.kb_dir = Path(kb_dir)
        self.daily_reports_dir = self.kb_dir / "daily_reports"
        self.index_dir = self.kb_dir / "index"
        self.statistics_dir = self.kb_dir / "statistics"

    def _load_topics_index(self) -> dict:
        """加载主题索引"""
        index_file = self.index_dir / "topics_index.json"
        if index_file.exists():
            with open(index_file, "r", encoding="utf-8") as f:
                return json.load(f)
        return {}

    def _load_signals_index(self) -> dict:
        """加载信号索引"""
        index_file = self.index_dir / "signals_index.json"
        if index_file.exists():
            with open(index_file, "r", encoding="utf-8") as f:
                return json.load(f)
        return {}

    def _load_performance_index(self) -> dict:
        """加载性能索引"""
        index_file = self.index_dir / "performance_index.json"
        if index_file.exists():
            with open(index_file, "r", encoding="utf-8") as f:
                return json.load(f)
        return {}

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

    def _date_in_range(self, date: str, date_range: Optional[tuple]) -> bool:
        """检查日期是否在范围内"""
        if not date_range:
            return True
        start_date, end_date = date_range
        return start_date <= date <= end_date

    def search_keywords(self, keywords: str, date_range: Optional[tuple] = None) -> list:
        """
        关键词搜索

        Args:
            keywords: 搜索关键词
            date_range: 日期范围，格式 (start_date, end_date)

        Returns:
            搜索结果列表
        """
        results = []
        keywords_lower = keywords.lower()
        
        summaries = self._load_all_summaries()
        
        for summary in summaries:
            date = summary.get("date", "")
            if not self._date_in_range(date, date_range):
                continue
            
            match_score = 0
            matched_fields = []
            
            content_parts = []
            content_parts.extend(summary.get("key_insights", []))
            content_parts.extend(summary.get("tags", []))
            content = " ".join(content_parts).lower()
            
            for keyword in keywords_lower.split():
                if keyword in content:
                    match_score += 1
                    matched_fields.append(keyword)
            
            if match_score > 0:
                results.append({
                    "date": date,
                    "summary": summary,
                    "match_score": match_score,
                    "matched_keywords": matched_fields
                })
        
        results.sort(key=lambda x: x["match_score"], reverse=True)
        return results

    def search_by_tags(self, tags: list, date_range: Optional[tuple] = None) -> list:
        """
        按标签搜索

        Args:
            tags: 标签列表
            date_range: 日期范围

        Returns:
            搜索结果列表
        """
        results = []
        summaries = self._load_all_summaries()
        
        for summary in summaries:
            date = summary.get("date", "")
            if not self._date_in_range(date, date_range):
                continue
            
            summary_tags = summary.get("tags", [])
            matched_tags = [tag for tag in tags if tag in summary_tags]
            
            if matched_tags:
                results.append({
                    "date": date,
                    "summary": summary,
                    "matched_tags": matched_tags,
                    "match_count": len(matched_tags)
                })
        
        results.sort(key=lambda x: x["match_count"], reverse=True)
        return results

    def search_by_topic(self, topic: str) -> list:
        """
        按主题搜索

        Args:
            topic: 主题名称

        Returns:
            搜索结果列表
        """
        topics_index = self._load_topics_index()
        
        if topic not in topics_index:
            return []
        
        dates = topics_index[topic]
        summaries = self._load_all_summaries()
        
        summary_map = {s.get("date", ""): s for s in summaries}
        
        results = []
        for date in dates:
            if date in summary_map:
                results.append({
                    "date": date,
                    "summary": summary_map[date]
                })
        
        return results

    def semantic_search(self, query: str, top_k: int = 5) -> list:
        """
        语义搜索（模拟实现）

        Args:
            query: 查询语句
            top_k: 返回结果数量

        Returns:
            搜索结果列表
        """
        keywords = self._extract_keywords(query)
        
        results = []
        summaries = self._load_all_summaries()
        
        for summary in summaries:
            score = 0
            content = " ".join([
                " ".join(summary.get("key_insights", [])),
                " ".join(summary.get("tags", []))
            ]).lower()
            
            for keyword in keywords:
                if keyword in content:
                    score += 2
                
                if any(c in keyword for c in content):
                    score += 1
            
            metrics = summary.get("metrics", {})
            if any(k in query for k in ["准确", "胜率", "收益"]):
                if metrics.get("prediction_accuracy", 0) > 0.7:
                    score += 3
                if metrics.get("signal_win_rate", 0) > 0.6:
                    score += 3
                if metrics.get("portfolio_return", 0) > 0:
                    score += 2
            
            if score > 0:
                results.append({
                    "date": summary.get("date", ""),
                    "summary": summary,
                    "relevance_score": score
                })
        
        results.sort(key=lambda x: x["relevance_score"], reverse=True)
        return results[:top_k]

    def _extract_keywords(self, query: str) -> list:
        """从查询中提取关键词"""
        keywords = []
        
        stock_names = ["腾讯", "阿里巴巴", "美团", "小米", "京东", "网易", "百度"]
        for name in stock_names:
            if name in query:
                keywords.append(name)
        
        signal_words = ["买入", "卖出", "持有", "加仓", "减仓", "止损"]
        for word in signal_words:
            if word in query:
                keywords.append(word)
        
        pattern_words = ["低开高走", "高开低走", "突破", "回调", "放量", "缩量"]
        for word in pattern_words:
            if word in query:
                keywords.append(word)
        
        if not keywords:
            words = query.replace("？", "").replace("?", "").split()
            keywords = [w for w in words if len(w) >= 2]
        
        return keywords

    def get_daily_summary(self, date: str) -> Optional[dict]:
        """
        获取指定日期的摘要

        Args:
            date: 日期字符串，格式 YYYY-MM-DD

        Returns:
            摘要字典，如果没有则返回 None
        """
        date_obj = datetime.strptime(date, "%Y-%m-%d")
        year_month = date_obj.strftime("%Y-%m")
        
        summary_file = self.daily_reports_dir / year_month / f"{date}_summary.json"
        
        if summary_file.exists():
            with open(summary_file, "r", encoding="utf-8") as f:
                return json.load(f)
        
        return None

    def get_statistics(self, start_date: str, end_date: str) -> dict:
        """
        获取指定日期范围的统计信息

        Args:
            start_date: 开始日期
            end_date: 结束日期

        Returns:
            统计信息字典
        """
        summaries = self._load_all_summaries()
        
        filtered = [s for s in summaries 
                   if start_date <= s.get("date", "") <= end_date]
        
        if not filtered:
            return {"error": "No data in range"}
        
        total = len(filtered)
        
        hsi_changes = []
        accuracies = []
        win_rates = []
        returns = []
        
        for summary in filtered:
            metrics = summary.get("metrics", {})
            hsi_changes.append(metrics.get("market_performance", {}).get("hsi_change", 0))
            accuracies.append(metrics.get("prediction_accuracy", 0))
            win_rates.append(metrics.get("signal_win_rate", 0))
            returns.append(metrics.get("portfolio_return", 0))
        
        return {
            "period": f"{start_date} to {end_date}",
            "total_days": total,
            "averages": {
                "hsi_change": round(sum(hsi_changes) / len(hsi_changes), 2) if hsi_changes else 0,
                "prediction_accuracy": round(sum(accuracies) / len(accuracies), 4) if accuracies else 0,
                "signal_win_rate": round(sum(win_rates) / len(win_rates), 4) if win_rates else 0,
                "portfolio_return": round(sum(returns) / len(returns), 2) if returns else 0
            }
        }

    def get_recent_reports(self, days: int = 7) -> list:
        """
        获取最近的报告

        Args:
            days: 天数

        Returns:
            报告列表
        """
        summaries = self._load_all_summaries()
        return summaries[-days:] if len(summaries) > days else summaries


def main():
    """主函数，用于命令行测试"""
    searcher = KnowledgeSearch()
    
    print("=== 测试关键词搜索 ===")
    results = searcher.search_keywords("科技股")
    print(f"找到 {len(results)} 条结果")
    
    print("\n=== 测试按标签搜索 ===")
    results = searcher.search_by_tags(["低开高走", "科技股"])
    print(f"找到 {len(results)} 条结果")
    
    print("\n=== 测试语义搜索 ===")
    results = searcher.semantic_search("什么时候应该买入科技股？")
    print(f"找到 {len(results)} 条结果")
    
    print("\n=== 测试获取统计 ===")
    stats = searcher.get_statistics("2026-02-01", "2026-02-28")
    print(json.dumps(stats, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
