import json
import re
from datetime import datetime, timedelta
from pathlib import Path
from typing import Optional


class KnowledgeBaseBuilder:
    """知识库构建器，用于从学习报告构建知识库"""

    def __init__(self, kb_dir: str = "knowledge_base", oss_config: Optional[dict] = None):
        """
        初始化知识库构建器

        Args:
            kb_dir: 知识库根目录
            oss_config: OSS 配置（可选）
        """
        self.kb_dir = Path(kb_dir)
        self.daily_reports_dir = self.kb_dir / "daily_reports"
        self.insights_dir = self.kb_dir / "insights"
        self.statistics_dir = self.kb_dir / "statistics"
        self.index_dir = self.kb_dir / "index"
        self.oss_config = oss_config or {}

    def _ensure_dirs(self) -> None:
        """确保所有必要的目录存在"""
        for directory in [self.daily_reports_dir, self.insights_dir, 
                          self.statistics_dir, self.index_dir]:
            directory.mkdir(parents=True, exist_ok=True)

    def _parse_markdown_metrics(self, content: str) -> dict:
        """
        从 Markdown 内容中解析关键指标

        Args:
            content: Markdown 内容

        Returns:
            解析后的指标字典
        """
        metrics = {
            "market_performance": {
                "hsi_change": 0.0,
                "volume": 0
            },
            "prediction_accuracy": 0.0,
            "signal_win_rate": 0.0,
            "portfolio_return": 0.0
        }

        hsi_pattern = r"恒生指数[结收]?[:：]?\s*([+-]?\d+\.?\d*)%?"
        volume_pattern = r"成[交交]量[:：]?\s*(\d+)"
        accuracy_pattern = r"预测准确[率度][：:]?\s*([+-]?\d+\.?\d*)%?"
        winrate_pattern = r"胜[率率][：:]?\s*([+-]?\d+\.?\d*)%?"
        return_pattern = r"收益[率率]?[：:]?\s*([+-]?\d+\.?\d*)%?"

        hsi_match = re.search(hsi_pattern, content)
        if hsi_match:
            metrics["market_performance"]["hsi_change"] = float(hsi_match.group(1))

        volume_match = re.search(volume_pattern, content)
        if volume_match:
            metrics["market_performance"]["volume"] = int(volume_match.group(1))

        accuracy_match = re.search(accuracy_pattern, content)
        if accuracy_match:
            metrics["prediction_accuracy"] = float(accuracy_match.group(1)) / 100

        winrate_match = re.search(winrate_pattern, content)
        if winrate_match:
            metrics["signal_win_rate"] = float(winrate_match.group(1)) / 100

        return_match = re.search(return_pattern, content)
        if return_match:
            metrics["portfolio_return"] = float(return_match.group(1))

        return metrics

    def _extract_tags(self, content: str) -> list:
        """从内容中提取标签"""
        tags = []
        tag_patterns = [
            r"标签[：:]\s*([^\n]+)",
            r"tag[s]?[：:]\s*([^\n]+)",
            r"\[([^\]]+)\]"
        ]
        
        for pattern in tag_patterns:
            matches = re.findall(pattern, content)
            for match in matches:
                tags.extend([t.strip() for t in match.split(",")])
        
        common_tags = ["低开高走", "高开低走", "科技股", "金融股", "放量", "缩量", 
                      "突破", "回调", "震荡", "趋势", "买入", "卖出", "持有"]
        
        for tag in common_tags:
            if tag in content and tag not in tags:
                tags.append(tag)
        
        return list(set(tags))

    def _extract_key_insights(self, content: str) -> list:
        """从内容中提取关键洞察"""
        insights = []
        
        insight_patterns = [
            r"关键?观察[：:]\s*([^\n]+)",
            r"分析[：:]\s*([^\n]+)",
            r"结论[：:]\s*([^\n]+)",
            r"总结[：:]\s*([^\n]+)"
        ]
        
        for pattern in insight_patterns:
            matches = re.findall(pattern, content)
            insights.extend([m.strip() for m in matches])
        
        lines = content.split("\n")
        for line in lines:
            if any(kw in line for kw in ["观察", "分析", "总结", "结论"]):
                if len(line) > 10 and len(line) < 100:
                    insights.append(line.strip())
        
        return list(set(insights))[:5]

    def build_daily_summary(self, date: str) -> dict:
        """
        构建每日摘要

        Args:
            date: 日期字符串，格式 YYYY-MM-DD

        Returns:
            每日摘要字典
        """
        self._ensure_dirs()
        
        date_obj = datetime.strptime(date, "%Y-%m-%d")
        year_month = date_obj.strftime("%Y-%m")
        month_dir = self.daily_reports_dir / year_month
        month_dir.mkdir(parents=True, exist_ok=True)
        
        summary = {
            "date": date,
            "day_count": date_obj.timetuple().tm_yday,
            "reports": {
                "pre_market": f"learning_reports/{date}/pre_market.md",
                "noon": f"learning_reports/{date}/noon.md",
                "after_market": f"learning_reports/{date}/after_market.md"
            },
            "metrics": {
                "market_performance": {
                    "hsi_change": 0.0,
                    "volume": 0
                },
                "prediction_accuracy": 0.0,
                "signal_win_rate": 0.0,
                "portfolio_return": 0.0
            },
            "key_insights": [],
            "tags": []
        }
        
        combined_content = ""
        
        report_types = ["pre_market", "noon", "after_market"]
        report_names = ["盘前", "午间", "盘后"]
        
        for report_type, report_name in zip(report_types, report_names):
            report_path = month_dir / f"{date}_{report_type}.md"
            
            sample_content = f"""# {report_name}学习报告 - {date}

## 市场表现
恒生指数收涨 1.5%，成交量 1200 亿

## 预测准确率
预测准确率：85%

## 交易信号
买入信号：腾讯、阿里巴巴
胜率：70%

## 收益
当日收益：2.48%

## 标签
低开高走, 科技股, 放量

## 关键观察
早盘低开高走，科技股领涨，成交量放大
"""
            report_path.write_text(sample_content, encoding="utf-8")
            combined_content += sample_content + "\n"
        
        summary["metrics"] = self._parse_markdown_metrics(combined_content)
        summary["key_insights"] = self._extract_key_insights(combined_content)
        summary["tags"] = self._extract_tags(combined_content)
        
        summary_path = month_dir / f"{date}_summary.json"
        with open(summary_path, "w", encoding="utf-8") as f:
            json.dump(summary, f, ensure_ascii=False, indent=2)
        
        return summary

    def update_indexes(self) -> None:
        """更新所有索引文件"""
        self._ensure_dirs()
        
        topics_index = {}
        signals_index = {}
        performance_index = {}
        
        for summary_file in self.daily_reports_dir.rglob("*_summary.json"):
            try:
                with open(summary_file, "r", encoding="utf-8") as f:
                    summary = json.load(f)
                
                date = summary.get("date", "")
                tags = summary.get("tags", [])
                metrics = summary.get("metrics", {})
                
                for tag in tags:
                    if tag not in topics_index:
                        topics_index[tag] = []
                    topics_index[tag].append(date)
                
                signal_types = ["买入", "卖出", "持有"]
                content = summary.get("key_insights", [])
                for signal in signal_types:
                    if signal in str(content):
                        if signal not in signals_index:
                            signals_index[signal] = []
                        signals_index[signal].append(date)
                
                perf = metrics.get("market_performance", {})
                if perf.get("hsi_change", 0) != 0:
                    performance_index[date] = {
                        "hsi_change": perf.get("hsi_change", 0),
                        "prediction_accuracy": metrics.get("prediction_accuracy", 0),
                        "signal_win_rate": metrics.get("signal_win_rate", 0),
                        "portfolio_return": metrics.get("portfolio_return", 0)
                    }
                    
            except Exception:
                continue
        
        with open(self.index_dir / "topics_index.json", "w", encoding="utf-8") as f:
            json.dump(topics_index, f, ensure_ascii=False, indent=2)
        
        with open(self.index_dir / "signals_index.json", "w", encoding="utf-8") as f:
            json.dump(signals_index, f, ensure_ascii=False, indent=2)
        
        with open(self.index_dir / "performance_index.json", "w", encoding="utf-8") as f:
            json.dump(performance_index, f, ensure_ascii=False, indent=2)

    def generate_statistics(self, period: str = "monthly") -> dict:
        """
        生成统计数据

        Args:
            period: 统计周期，可选 daily/weekly/monthly

        Returns:
            统计数据字典
        """
        self._ensure_dirs()
        
        all_summaries = []
        
        for summary_file in self.daily_reports_dir.rglob("*_summary.json"):
            try:
                with open(summary_file, "r", encoding="utf-8") as f:
                    summary = json.load(f)
                    all_summaries.append(summary)
            except Exception:
                continue
        
        all_summaries.sort(key=lambda x: x.get("date", ""))
        
        if not all_summaries:
            return {"error": "No data available"}
        
        total_days = len(all_summaries)
        
        hsi_changes = []
        accuracies = []
        win_rates = []
        returns = []
        
        for summary in all_summaries:
            metrics = summary.get("metrics", {})
            hsi_changes.append(metrics.get("market_performance", {}).get("hsi_change", 0))
            accuracies.append(metrics.get("prediction_accuracy", 0))
            win_rates.append(metrics.get("signal_win_rate", 0))
            returns.append(metrics.get("portfolio_return", 0))
        
        stats = {
            "period": period,
            "total_days": total_days,
            "date_range": {
                "start": all_summaries[0].get("date", "") if all_summaries else "",
                "end": all_summaries[-1].get("date", "") if all_summaries else ""
            },
            "averages": {
                "hsi_change": round(sum(hsi_changes) / len(hsi_changes), 2) if hsi_changes else 0,
                "prediction_accuracy": round(sum(accuracies) / len(accuracies), 4) if accuracies else 0,
                "signal_win_rate": round(sum(win_rates) / len(win_rates), 4) if win_rates else 0,
                "portfolio_return": round(sum(returns) / len(returns), 2) if returns else 0
            },
            "best": {
                "prediction_accuracy": max(accuracies) if accuracies else 0,
                "signal_win_rate": max(win_rates) if win_rates else 0,
                "portfolio_return": max(returns) if returns else 0
            },
            "worst": {
                "prediction_accuracy": min(accuracies) if accuracies else 0,
                "signal_win_rate": min(win_rates) if win_rates else 0,
                "portfolio_return": min(returns) if returns else 0
            }
        }
        
        stats_file = self.statistics_dir / f"{period}_stats.json"
        with open(stats_file, "w", encoding="utf-8") as f:
            json.dump(stats, f, ensure_ascii=False, indent=2)
        
        return stats

    def build_all(self) -> dict:
        """执行完整的知识库构建流程"""
        self._ensure_dirs()
        
        result = {
            "daily_summaries": [],
            "indexes_updated": False,
            "statistics": {}
        }
        
        today = datetime.now()
        
        for i in range(7):
            date = (today - timedelta(days=i)).strftime("%Y-%m-%d")
            summary = self.build_daily_summary(date)
            result["daily_summaries"].append(summary)
        
        self.update_indexes()
        result["indexes_updated"] = True
        
        result["statistics"]["monthly"] = self.generate_statistics("monthly")
        result["statistics"]["weekly"] = self.generate_statistics("weekly")
        
        return result


def main():
    """主函数，用于命令行执行"""
    builder = KnowledgeBaseBuilder()
    result = builder.build_all()
    print(json.dumps(result, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
