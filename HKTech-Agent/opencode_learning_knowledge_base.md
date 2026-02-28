# Opencode 开发任务：构建每日学习知识库系统

**优先级**: P0 (最高)  
**执行时间**: 立即  
**目标**: 将每日学习内容构建成可搜索、可分析的知识库

---

## 📋 项目背景

已有每日学习系统（盘前/午间/盘后），学习内容保存到 OSS。现在需要：
1. 构建本地知识库（Markdown + JSON）
2. 实现知识检索功能
3. 支持知识分析和洞察
4. 提供可视化界面

---

## 🎯 功能需求

### 1. 知识库存储结构

**目录结构**:
```
knowledge_base/
├── daily_reports/           # 每日学习报告
│   ├── 2026-02/
│   │   ├── 2026-02-28_pre_market.md
│   │   ├── 2026-02-28_noon.md
│   │   ├── 2026-02-28_after_market.md
│   │   └── 2026-02-28_summary.json
│   └── 2026-03/
├── insights/                # 知识洞察
│   ├── market_patterns.md   # 市场规律
│   ├── trading_signals.md   # 交易信号总结
│   └── lessons_learned.md   # 经验教训
├── statistics/              # 统计数据
│   ├── daily_stats.json     # 每日统计
│   ├── weekly_stats.json    # 每周统计
│   └── monthly_stats.json   # 每月统计
└── index/                   # 索引文件
    ├── topics_index.json    # 主题索引
    ├── signals_index.json   # 信号索引
    └── performance_index.json # 性能索引
```

**每日摘要 JSON 格式**:
```json
{
  "date": "2026-02-28",
  "day_count": 1,
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
  "key_insights": [
    "早盘低开高走",
    "科技股领涨",
    "成交量放大"
  ],
  "tags": ["低开高走", "科技股", "放量"]
}
```

---

### 2. 知识检索功能

**文件**: `knowledge_base/search.py`

**功能**:
- ✅ 全文搜索（支持关键词）
- ✅ 按日期范围搜索
- ✅ 按标签搜索
- ✅ 按主题搜索
- ✅ 相似度搜索（语义搜索）

**API 设计**:
```python
class KnowledgeSearch:
    def search_keywords(keywords: str, date_range: tuple = None) -> list
    def search_by_tags(tags: list) -> list
    def search_by_topic(topic: str) -> list
    def semantic_search(query: str, top_k: int = 5) -> list
    def get_daily_summary(date: str) -> dict
    def get_statistics(start_date: str, end_date: str) -> dict
```

**使用示例**:
```python
searcher = KnowledgeSearch()

# 关键词搜索
results = searcher.search_keywords("腾讯 买入")

# 按标签搜索
results = searcher.search_by_tags(["低开高走", "科技股"])

# 语义搜索
results = searcher.semantic_search("什么时候应该买入科技股？")

# 获取统计
stats = searcher.get_statistics("2026-02-01", "2026-02-28")
```

---

### 3. 知识分析功能

**文件**: `knowledge_base/analyzer.py`

**功能**:
- ✅ 预测准确率分析
- ✅ 信号胜率分析
- ✅ 市场模式识别
- ✅ 经验教训提取
- ✅ 趋势分析

**分析方法**:
```python
class KnowledgeAnalyzer:
    def analyze_prediction_accuracy(days: int = 30) -> dict
    def analyze_signal_win_rate(days: int = 30) -> dict
    def identify_market_patterns() -> list
    def extract_lessons_learned() -> list
    def analyze_performance_trend() -> dict
```

**输出示例**:
```json
{
  "analysis_period": "30 days",
  "prediction_accuracy": {
    "average": 0.82,
    "best_day": "2026-02-28 (0.95)",
    "worst_day": "2026-02-15 (0.60)"
  },
  "signal_win_rate": {
    "average": 0.68,
    "buy_signals": 0.72,
    "sell_signals": 0.65
  },
  "market_patterns": [
    "周一效应：低开概率 65%",
    "科技股领涨时，大盘跟随概率 80%",
    "成交量放大时，次日延续概率 70%"
  ],
  "lessons_learned": [
    "早盘急跌不宜立即抄底，等待企稳信号",
    "科技股波动大，需设置更紧止损",
    "成交量是关键确认指标"
  ]
}
```

---

### 4. 知识库构建器

**文件**: `knowledge_base/builder.py`

**功能**:
- ✅ 从 OSS 下载学习报告
- ✅ 解析 Markdown 提取关键信息
- ✅ 生成每日摘要 JSON
- ✅ 更新索引文件
- ✅ 生成统计数据

**执行流程**:
```python
class KnowledgeBaseBuilder:
    def __init__(self, oss_config: dict, kb_dir: str)
    
    def build_daily_summary(date: str) -> dict
        # 1. 从 OSS 下载报告
        # 2. 解析 Markdown
        # 3. 提取关键指标
        # 4. 生成摘要 JSON
        # 5. 保存到知识库
    
    def update_indexes()
        # 1. 更新主题索引
        # 2. 更新信号索引
        # 3. 更新性能索引
    
    def generate_statistics(period: str = 'monthly')
        # 1. 计算统计数据
        # 2. 生成统计报告
        # 3. 保存到 statistics/
    
    def build_all()
        # 完整构建流程
```

---

### 5. 可视化界面

**文件**: `web/knowledge_base.html`

**功能**:
- ✅ 知识库首页（显示最近报告）
- ✅ 搜索界面（支持多种搜索）
- ✅ 统计图表（准确率、胜率趋势）
- ✅ 知识洞察展示
- ✅ 经验教训列表

**页面结构**:
```html
<!DOCTYPE html>
<html>
<head>
    <title>学习知识库</title>
</head>
<body>
    <!-- 导航栏 -->
    <nav>
        <a href="#home">首页</a>
        <a href="#search">搜索</a>
        <a href="#statistics">统计</a>
        <a href="#insights">洞察</a>
    </nav>
    
    <!-- 首页 -->
    <section id="home">
        <h1>最近学习报告</h1>
        <div class="report-list">...</div>
    </section>
    
    <!-- 搜索 -->
    <section id="search">
        <input type="text" id="search-input" placeholder="搜索关键词...">
        <div class="search-results">...</div>
    </section>
    
    <!-- 统计 -->
    <section id="statistics">
        <canvas id="accuracy-chart"></canvas>
        <canvas id="winrate-chart"></canvas>
    </section>
    
    <!-- 洞察 -->
    <section id="insights">
        <h2>市场规律</h2>
        <ul class="patterns">...</ul>
        
        <h2>经验教训</h2>
        <ul class="lessons">...</ul>
    </section>
</body>
</html>
```

---

### 6. 自动化更新

**Cron 任务**: 每日 17:00 执行

**功能**:
- ✅ 下载当日学习报告
- ✅ 构建知识库摘要
- ✅ 更新索引
- ✅ 生成统计

**Cron 配置**:
```json
{
  "name": "知识库每日更新",
  "schedule": "0 17 * * 1-5",
  "payload": {
    "kind": "agentTurn",
    "message": "执行知识库更新任务：\n\n1. 从 OSS 下载今日学习报告\n2. 解析并生成摘要 JSON\n3. 更新知识库索引\n4. 生成统计数据\n5. 更新可视化界面"
  }
}
```

---

## ✅ 验收标准

### 代码质量
- [ ] 所有模块有类型注解
- [ ] 所有公共方法有文档字符串
- [ ] 通过 Ruff 检查
- [ ] 通过 Mypy 检查
- [ ] 单元测试覆盖率 > 60%

### 功能完整性
- [ ] 知识库目录结构正确
- [ ] 能成功解析学习报告
- [ ] 能成功生成摘要 JSON
- [ ] 搜索功能正常
- [ ] 分析功能正常
- [ ] 可视化界面可用

### 性能要求
- [ ] 搜索响应时间 < 1 秒
- [ ] 知识库构建时间 < 5 分钟
- [ ] 支持 1000+ 报告检索

---

## 🚀 执行步骤

### Step 1: 创建知识库目录结构 (10 分钟)

```bash
cd /root/.openclaw/workspace/Lamda-ai/HKTech-Agent
mkdir -p knowledge_base/{daily_reports,insights,statistics,index}
mkdir -p web
```

### Step 2: 实现知识库构建器 (40 分钟)

```bash
cat > knowledge_base/builder.py << 'EOF'
# 实现代码
EOF
```

### Step 3: 实现搜索功能 (30 分钟)

```bash
cat > knowledge_base/search.py << 'EOF'
# 实现代码
EOF
```

### Step 4: 实现分析功能 (30 分钟)

```bash
cat > knowledge_base/analyzer.py << 'EOF'
# 实现代码
EOF
```

### Step 5: 创建可视化界面 (30 分钟)

```bash
cat > web/knowledge_base.html << 'EOF'
# 实现代码
EOF
```

### Step 6: 创建 Cron 任务 (10 分钟)

```bash
openclaw cron add --name "知识库每日更新" --schedule "0 17 * * 1-5" --message "..."
```

### Step 7: 测试和文档 (20 分钟)

```bash
# 测试构建功能
python3 knowledge_base/builder.py

# 测试搜索功能
python3 knowledge_base/search.py

# 创建使用文档
cat > docs/KNOWLEDGE_BASE_GUIDE.md << 'EOF'
# 文档内容
EOF
```

### Step 8: 提交并推送 (5 分钟)

```bash
git add -A
git commit -m "feat: 构建每日学习知识库系统"
git push origin main
```

---

## 📊 预期成果

**文件**:
- `knowledge_base/builder.py` (~300 行)
- `knowledge_base/search.py` (~250 行)
- `knowledge_base/analyzer.py` (~200 行)
- `web/knowledge_base.html` (~200 行)
- `docs/KNOWLEDGE_BASE_GUIDE.md` (~400 行)

**功能**:
- ✅ 知识库自动构建
- ✅ 全文搜索
- ✅ 语义搜索
- ✅ 数据分析
- ✅ 可视化界面
- ✅ 每日自动更新

**质量**:
- ✅ Ruff 检查通过
- ✅ Mypy 检查通过
- ✅ 测试覆盖率 > 60%

---

## 🎯 使用场景

### 场景 1: 查询历史交易信号

```python
searcher = KnowledgeSearch()
results = searcher.search_by_tags(["买入信号", "腾讯"])
for result in results:
    print(f"{result['date']}: {result['summary']}")
```

### 场景 2: 分析预测准确率

```python
analyzer = KnowledgeAnalyzer()
accuracy = analyzer.analyze_prediction_accuracy(days=30)
print(f"30 天平均准确率：{accuracy['average']:.2%}")
```

### 场景 3: 查看经验教训

```python
lessons = analyzer.extract_lessons_learned()
for lesson in lessons:
    print(f"- {lesson}")
```

### 场景 4: 可视化界面

访问：`http://60.205.245.131:8080/knowledge_base.html`

---

## 🎯 开始执行！

请按照上述步骤执行，完成后生成验收报告。

**预计时间**: 175 分钟 (~3 小时)  
**优先级**: P0 (最高)  
**重要性**: ⭐⭐⭐⭐⭐
