# 每日学习知识库系统使用指南

## 概述

每日学习知识库系统将盘前/午间/盘后的学习内容构建成可搜索、可分析的知识库，支持全文搜索、语义搜索、数据分析和可视化展示。

## 目录结构

```
knowledge_base/
├── daily_reports/           # 每日学习报告
│   └── YYYY-MM/            # 按年月组织
│       └── *_{pre_market,noon,after_market}.md
│       └── *_summary.json  # 每日摘要
├── insights/                # 知识洞察
│   ├── market_patterns.md  # 市场规律
│   ├── lessons_learned.md  # 经验教训
│   └── insights_report.json
├── statistics/              # 统计数据
│   ├── daily_stats.json
│   ├── weekly_stats.json
│   └── monthly_stats.json
└── index/                   # 索引文件
    ├── topics_index.json    # 主题索引
    ├── signals_index.json   # 信号索引
    └── performance_index.json
```

## 快速开始

### 1. 构建知识库

```python
from knowledge_base.builder import KnowledgeBaseBuilder

builder = KnowledgeBaseBuilder()

# 构建所有数据
result = builder.build_all()

# 或单独执行
summary = builder.build_daily_summary("2026-02-28")
builder.update_indexes()
stats = builder.generate_statistics("monthly")
```

### 2. 搜索知识

```python
from knowledge_base.search import KnowledgeSearch

searcher = KnowledgeSearch()

# 关键词搜索
results = searcher.search_keywords("腾讯 买入")

# 按标签搜索
results = searcher.search_by_tags(["低开高走", "科技股"])

# 语义搜索
results = searcher.semantic_search("什么时候应该买入科技股？", top_k=5)

# 获取统计
stats = searcher.get_statistics("2026-02-01", "2026-02-28")
```

### 3. 分析数据

```python
from knowledge_base.analyzer import KnowledgeAnalyzer

analyzer = KnowledgeAnalyzer()

# 预测准确率分析
accuracy = analyzer.analyze_prediction_accuracy(days=30)
print(f"平均准确率: {accuracy['average']:.2%}")

# 信号胜率分析
winrate = analyzer.analyze_signal_win_rate(days=30)

# 市场模式识别
patterns = analyzer.identify_market_patterns()

# 经验教训
lessons = analyzer.extract_lessons_learned()

# 性能趋势
trend = analyzer.analyze_performance_trend()

# 生成洞察报告
insights = analyzer.generate_insights_report()
```

## 功能说明

### 知识库构建器 (builder.py)

| 方法 | 说明 |
|------|------|
| `build_daily_summary(date)` | 构建指定日期的摘要 |
| `update_indexes()` | 更新主题/信号/性能索引 |
| `generate_statistics(period)` | 生成统计报告 |
| `build_all()` | 执行完整构建流程 |

### 搜索功能 (search.py)

| 方法 | 说明 |
|------|------|
| `search_keywords(keywords, date_range)` | 关键词全文搜索 |
| `search_by_tags(tags, date_range)` | 按标签搜索 |
| `search_by_topic(topic)` | 按主题搜索 |
| `semantic_search(query, top_k)` | 语义搜索 |
| `get_daily_summary(date)` | 获取指定日期摘要 |
| `get_statistics(start_date, end_date)` | 获取日期范围统计 |

### 分析功能 (analyzer.py)

| 方法 | 说明 |
|------|------|
| `analyze_prediction_accuracy(days)` | 预测准确率分析 |
| `analyze_signal_win_rate(days)` | 信号胜率分析 |
| `identify_market_patterns()` | 市场模式识别 |
| `extract_lessons_learned()` | 经验教训提取 |
| `analyze_performance_trend()` | 性能趋势分析 |
| `generate_insights_report()` | 生成洞察报告 |

## 可视化界面

访问 `web/knowledge_base.html` 查看可视化界面：

- **首页**: 显示最近的学习报告
- **搜索**: 支持关键词和标签搜索
- **统计**: 展示准确率、胜率、收益率等指标
- **洞察**: 展示市场规律和经验教训

## 自动化任务

已配置 Cron 任务，每日 17:00 (周一至周五) 自动执行：

1. 从 OSS 下载当日学习报告
2. 解析并生成摘要 JSON
3. 更新知识库索引
4. 生成统计数据

查看 cron 任务：
```bash
openclaw cron list
```

## 数据格式

### 每日摘要 JSON

```json
{
  "date": "2026-02-28",
  "day_count": 59,
  "reports": {
    "pre_market": "learning_reports/2026-02-28/pre_market.md",
    "noon": "learning_reports/2026-02-28/noon.md",
    "after_market": "learning_reports/2026-02-28/after_market.md"
  },
  "metrics": {
    "market_performance": {
      "hsi_change": 1.5,
      "volume": 1200
    },
    "prediction_accuracy": 0.85,
    "signal_win_rate": 0.70,
    "portfolio_return": 2.48
  },
  "key_insights": ["早盘低开高走", "科技股领涨"],
  "tags": ["低开高走", "科技股", "放量"]
}
```

## 使用示例

### 查询历史交易信号

```python
searcher = KnowledgeSearch()
results = searcher.search_by_tags(["买入信号", "腾讯"])
for result in results:
    print(f"{result['date']}: {result['summary']['metrics']['portfolio_return']}")
```

### 分析预测准确率

```python
analyzer = KnowledgeAnalyzer()
accuracy = analyzer.analyze_prediction_accuracy(days=30)
print(f"30天平均准确率: {accuracy['average']:.2%}")
print(f"最佳日期: {accuracy['best_day']}")
```

### 查看经验教训

```python
analyzer = KnowledgeAnalyzer()
lessons = analyzer.extract_lessons_learned()
for lesson in lessons:
    print(f"- {lesson}")
```

## 验收标准

- [x] 知识库目录结构正确
- [x] 能成功解析学习报告
- [x] 能成功生成摘要 JSON
- [x] 搜索功能正常
- [x] 分析功能正常
- [x] 可视化界面可用
- [x] Ruff 检查通过
- [x] Cron 任务已配置

## 维护说明

### 手动更新知识库

```bash
python3 knowledge_base/builder.py
```

### 查看日志

```bash
openclaw cron runs
```

### 禁用/启用任务

```bash
openclaw cron disable "知识库每日更新"
openclaw cron enable "知识库每日更新"
```
