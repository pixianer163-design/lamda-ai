# Agent 内置任务调度器 - 最终配置

**更新时间**: 2026-02-24 20:27  
**版本**: v1.0

---

## 📋 架构设计

### 双层调度架构

```
┌─────────────────────────────────────────────────────────┐
│                   定时任务                               │
└──────────────────┬──────────────────────────────────────┘
                   │
       ┌───────────┴───────────┐
       │                       │
       ▼                       ▼
┌─────────────┐         ┌─────────────┐
│ 核心任务     │         │ 简单任务     │
│ (1 个)       │         │ (4 个)        │
└──────┬──────┘         └──────┬──────┘
       │                       │
       ▼                       ▼
┌─────────────┐         ┌─────────────┐
│ 内置调度器   │         │ 脚本调度     │
│ scheduler.py│         │ cron_       │
│ (每 5 分钟检查) │         │ scheduler.sh│
└─────────────┘         └─────────────┘
```

---

## 📊 任务分配

### 核心任务（使用内置调度器）

| 任务 | 时间 | 说明 | 执行方式 |
|------|------|------|---------|
| **恒生 Agent 每日运行** | 工作日 9:30 AM | 数据采集、LLM 分析、世界模型预测、推送 | `bash run_prod.sh` |

**特点**:
- ✅ 执行时间长（~5 分钟）
- ✅ 需要状态追踪
- ✅ 需要错过补偿
- ✅ 需要日志记录

### 简单任务（使用 cron_scheduler.sh）

| 任务 | 时间 | 说明 | 执行方式 |
|------|------|------|---------|
| **AI 技术学习简报** | 每天 8:00 AM | 生成 AI 简报并推送 | `sessions spawn` |
| **盘前学习** | 工作日 9:00 AM | 盘前分析推送 | `sessions spawn` |
| **午间学习** | 工作日 12:30 PM | 午间分析推送 | `sessions spawn` |
| **盘后学习** | 工作日 4:30 PM | 盘后分析推送 | `sessions spawn` |

**特点**:
- ✅ 执行时间短（~1 分钟）
- ✅ 简单推送任务
- ✅ 无需复杂状态管理

### 恒生 Agent 原有任务（保持不变）

| 任务 | 时间 | 说明 |
|------|------|------|
| 午前快报 | 12:00 PM | 盘中报告 |
| 午后快报 | 3:00 PM | 盘中报告 |
| 盘后总结 | 4:00 PM | 盘后报告 |
| OSS 同步 | 9:25 AM | 模型下载 |

---

## 🔧 配置文件

### task_config.json

```json
{
  "tasks": [
    {
      "id": "hktech_daily_run",
      "name": "恒生 Agent 每日运行",
      "schedule": {
        "type": "cron",
        "expression": "30 9 * * 1-5"
      },
      "execution": {
        "command": "cd /opt/hktech-agent && bash run_prod.sh",
        "timeout_seconds": 600,
        "log_path": "/opt/hktech-agent/prod/logs/scheduler_run.log"
      }
    }
  ]
}
```

### Crontab

```bash
# Agent 内置调度器（每 5 分钟检查）
*/5 * * * * python3 scheduler.py --check

# 简单推送任务
5 8 * * * cron_scheduler.sh daily_briefing
5 9 * * 1-5 cron_scheduler.sh morning_briefing
35 12 * * 1-5 cron_scheduler.sh noon_learning
35 16 * * 1-5 cron_scheduler.sh afternoon_learning

# 恒生 Agent 原有任务
0 12 * * 1-5 intraday_report_sender.py --type mid_morning
0 15 * * 1-5 intraday_report_sender.py --type mid_afternoon
0 16 * * 1-5 intraday_report_sender.py --type post_market
25 9 * * 1-5 90_sync_oss_model.sh
```

---

## 📁 文件结构

```
/root/.openclaw/workspace/Lamda-ai/HKTech-Agent/
├── scheduler/
│   ├── scheduler.py              # 内置调度器
│   ├── task_config.json          # 核心任务配置（仅 1 个）
│   └── TEST_REPORT.md            # 测试报告
├── scripts/
│   └── cron_scheduler.sh         # 简单推送脚本
└── ...

/opt/hktech-agent/
├── run_prod.sh                   # 恒生 Agent 主程序
├── prod/src/
│   └── intraday_report_sender.py # 盘中报告
└── cron/
    └── 90_sync_oss_model.sh      # OSS 同步
```

---

## 📊 执行流程

### 恒生 Agent 每日运行（9:30 AM）

```
09:25 AM ──→ OSS 模型同步（预加载）
    ↓
09:30 AM ──→ 内置调度器触发
    ↓
bash run_prod.sh
    ↓
1. 数据采集（6 只股票）
2. LLM 信号提取
3. 世界模型预测
4. 生成投资决策
5. 推送飞书双群
    ↓
✅ 完成（~5 分钟）
```

### 简单推送任务（以午间学习为例）

```
12:30 PM ──→ cron 触发
    ↓
cron_scheduler.sh noon_learning
    ↓
openclaw sessions spawn
    ↓
1. 获取上午数据
2. 分析行情
3. 推送飞书
    ↓
✅ 完成（~1 分钟）
```

---

## 🎯 优势对比

| 维度 | 内置调度器 | 脚本调度 |
|------|-----------|---------|
| **适用场景** | 核心复杂任务 | 简单推送任务 |
| **状态管理** | ✅ 完整 | ❌ 无 |
| **错过补偿** | ✅ 支持 | ❌ 无 |
| **执行日志** | ✅ JSONL | ✅ 文本 |
| **重试机制** | ✅ 2 次 | ❌ 无 |
| **配置复杂度** | 中 | 低 |
| **维护成本** | 中 | 低 |

---

## 📋 监控命令

### 查看内置调度器状态
```bash
python3 scheduler.py --status
```

### 查看简单任务日志
```bash
tail -f /root/.openclaw/workspace/logs/cron/noon_learning.log
```

### 查看恒生 Agent 日志
```bash
tail -f /opt/hktech-agent/prod/logs/cron.log
```

---

## ✅ 总结

**架构设计原则**:
1. **核心任务** → 内置调度器（状态追踪 + 错过补偿）
2. **简单任务** → 脚本调度（轻量 + 快速）
3. **原有任务** → 保持不变（稳定优先）

**明日执行计划**:
- ✅ 8:00 AM - AI 技术简报（脚本调度）
- ✅ 9:00 AM - 盘前学习（脚本调度）
- ✅ 9:25 AM - OSS 同步（原有）
- ✅ **9:30 AM - 恒生 Agent（内置调度器）** ⭐
- ✅ 12:00 PM - 午前快报（原有）
- ✅ 12:30 PM - 午间学习（脚本调度）
- ✅ 3:00 PM - 午后快报（原有）
- ✅ 4:00 PM - 盘后总结（原有）
- ✅ 4:30 PM - 盘后学习（脚本调度）

---

**最后更新**: 2026-02-24 20:27  
**状态**: ✅ 配置完成，待明日验证
