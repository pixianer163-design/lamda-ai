# 每日学习功能 + OSS 存储指南

**版本**: v1.0  
**创建时间**: 2026-02-28  
**状态**: ✅ 已启用

---

## 📋 功能概述

每日学习系统自动执行三次学习任务，并将学习报告保存到 OSS：

| 任务 | 时间 | 频率 | OSS 路径 |
|------|------|------|---------|
| 🌅 盘前学习 | 9:00 AM | 工作日 | `learning_reports/YYYY-MM-DD/pre_market_YYYY-MM-DD.md` |
| 🌞 午间学习 | 12:30 PM | 工作日 | `learning_reports/YYYY-MM-DD/noon_YYYY-MM-DD.md` |
| 🌙 盘后学习 | 4:30 PM | 工作日 | `learning_reports/YYYY-MM-DD/after_market_YYYY-MM-DD.md` |

---

## 🚀 快速开始

### 1. 配置 OSS 环境变量

```bash
export ALIYUN_ACCESS_KEY_ID=your_access_key_id
export ALIYUN_ACCESS_KEY_SECRET=your_access_key_secret
export ALIYUN_OSS_ENDPOINT=oss-cn-beijing.aliyuncs.com
export ALIYUN_OSS_BUCKET=hktech-agent-models
```

### 2. 安装依赖

```bash
pip install oss2 --break-system-packages
```

### 3. 测试上传功能

```bash
cd /root/.openclaw/workspace/Lamda-ai/HKTech-Agent
python3 scripts/upload_learning_to_oss.py
```

---

## 📝 学习任务详情

### 🌅 盘前学习 (9:00 AM)

**内容**:
1. 获取 overnight 新闻和全球市场动态
2. 分析美股/欧股/日股收盘对港股的影响
3. 阅读当天重要财经新闻（LLM 情绪分析）
4. 更新市场日历
5. 生成开盘策略建议

**输出格式**:
```markdown
# 🌅 盘前学习报告 - Day X
**日期**: 2026-02-28 09:00

## 📰 全球 markets overnight
- 美股：道指 +0.5%, 纳指 +0.8%
- 欧股：斯托克 600 +0.3%
- 日股：日经 225 +1.2%

## 📊 港股开盘策略
- 腾讯：买入信号
- 阿里：观望
- 美团：持有

## ⚠️ 风险提示
- 美联储讲话
- 地缘政治风险
```

---

### 🌞 午间学习 (12:30 PM)

**内容**:
1. 获取上午港股收盘数据
2. 分析上午行情特征
3. 对比开盘预测 vs 实际走势
4. 更新持仓盈亏
5. 调整下午策略
6. 检查交易信号

**输出格式**:
```markdown
# 🌞 午间学习报告 - Day X
**日期**: 2026-02-28 12:30

## 📊 上午行情复盘
- 恒生指数：+1.2%
- 成交量：500 亿港元

## 📈 持仓盈亏更新
- 腾讯：+2.5%
- 阿里：+1.8%
- 美团：-0.5%

## 🎯 下午策略调整
- 加仓腾讯
- 减仓美团

## ⚡ 交易信号
- 无
```

---

### 🌙 盘后学习 (4:30 PM)

**内容**:
1. 获取全天港股收盘数据
2. 执行回测验证
3. 更新世界模型
4. 记录交易日志
5. 生成全天学习报告
6. 准备明日策略

**输出格式**:
```markdown
# 🌙 盘后学习报告 - Day X
**日期**: 2026-02-28 16:30

## 📊 全天行情总结
- 恒生指数：+1.5%
- 成交额：1200 亿港元

## 📚 今日学习要点
- 早盘低开高走
- 科技股领涨
- 成交量放大

## 🎯 明日策略预案
- 关注美联储讲话
- 准备回调买入

## 📈 回测验证结果
- 预测准确率：85%
- 信号胜率：70%
```

---

## 💾 OSS 存储结构

```
oss://hktech-agent-models/learning_reports/
├── 2026-02-28/
│   ├── pre_market_2026-02-28.md
│   ├── noon_2026-02-28.md
│   └── after_market_2026-02-28.md
├── 2026-03-01/
│   ├── pre_market_2026-03-01.md
│   ├── noon_2026-03-01.md
│   └── after_market_2026-03-01.md
└── ...
```

---

## 🔧 使用脚本

### 上传报告

```python
from scripts.upload_learning_to_oss import LearningReportUploader

uploader = LearningReportUploader()

# 上传 Markdown 报告
url = uploader.upload_report(
    report_content="# 学习报告内容...",
    report_type='pre_market',
    date='2026-02-28'
)
print(f"OSS URL: {url}")

# 上传 JSON 报告
url = uploader.upload_json_report(
    report_data={'accuracy': 0.85, 'signals': 5},
    report_type='daily_stats',
    date='2026-02-28'
)
```

### 本地 +OSS 双存储

```python
result = uploader.save_to_local_and_upload(
    report_content="# 报告内容...",
    report_type='after_market',
    local_dir='/path/to/local/reports'
)

print(f"本地路径：{result['local_path']}")
print(f"OSS URL: {result['oss_url']}")
```

### 列出报告

```python
reports = uploader.list_reports(date='2026-02-28')
for report in reports:
    print(f"- {report['key']} ({report['size']} bytes)")
```

---

## 📊 Cron 任务配置

### 查看任务状态

```bash
openclaw cron list
```

### 手动触发任务

```bash
# 盘前学习
openclaw cron run e8565106-7445-4e76-9e88-e177f2b01d2e

# 午间学习
openclaw cron run 0683093a-d921-4a4e-9f16-88aa5dcbb665

# 盘后学习
openclaw cron run f4795a3f-167d-4a90-b009-de145850354c
```

---

## 🔍 访问 OSS 报告

### 方法 1: 通过 URL 访问

```
https://hktech-agent-models.oss-cn-beijing.aliyuncs.com/learning_reports/2026-02-28/pre_market_2026-02-28.md
```

### 方法 2: 通过 OSS 控制台

1. 登录阿里云 OSS 控制台
2. 进入 `hktech-agent-models` Bucket
3. 浏览 `learning_reports/` 目录

### 方法 3: 使用 ossutil

```bash
ossutil ls oss://hktech-agent-models/learning_reports/2026-02-28/
ossutil cp oss://hktech-agent-models/learning_reports/2026-02-28/pre_market_2026-02-28.md ./
```

---

## 📈 学习数据分析

### 统计学习报告数量

```python
reports = uploader.list_reports()
print(f"今日报告数：{len(reports)}")
```

### 计算平均准确率

```python
import json
import oss2

# 下载 JSON 统计数据
data = bucket.get_object('learning_reports/2026-02-28/daily_stats.json')
stats = json.loads(data.read())
print(f"平均准确率：{stats['avg_accuracy']}")
```

---

## ⚠️ 注意事项

### 1. OSS 费用

- 存储费用：约 0.12 元/GB/月
- 流量费用：约 0.50 元/GB
- 请求费用：约 0.01 元/万次

**预估**: 每月约 1-5 元（取决于报告大小）

### 2. 安全建议

- 使用 RAM 子账号访问 OSS
- 限制 Bucket 权限（只读/只写）
- 定期轮换 AccessKey

### 3. 备份策略

- OSS 自动多副本存储
- 本地保留最近 7 天报告
- 每月导出一次完整备份

---

## 🎯 最佳实践

### 1. 报告格式

- 使用 Markdown 格式
- 包含日期和时间戳
- 结构化数据（标题、列表）

### 2. 命名规范

```
{type}_{YYYY-MM-DD}.md
```

例如：
- `pre_market_2026-02-28.md`
- `noon_2026-02-28.md`
- `after_market_2026-02-28.md`

### 3. 内容组织

```markdown
# 标题
**元数据**: 日期、时间

## 章节 1
内容...

## 章节 2
内容...

---
*自动生成*
```

---

## 📞 故障排除

### 问题 1: OSS 上传失败

**错误**: `AccessDenied`

**解决**:
```bash
# 检查环境变量
echo $ALIYUN_ACCESS_KEY_ID
echo $ALIYUN_ACCESS_KEY_SECRET

# 验证权限
ossutil ls oss://hktech-agent-models/
```

### 问题 2: Cron 任务未执行

**检查**:
```bash
openclaw cron list
openclaw cron status
```

### 问题 3: 报告内容为空

**解决**:
- 检查 LLM API 状态
- 验证数据源连接
- 查看日志文件

---

## 📚 相关文档

- [OSS 配置指南](ALIYUN_OSS_GUIDE.md)
- [Cron 任务配置](CRON_SETUP.md)
- [学习任务设计](LEARNING_TASK_DESIGN.md)

---

**文档版本**: v1.0  
**最后更新**: 2026-02-28  
**维护者**: Alex 🐾
