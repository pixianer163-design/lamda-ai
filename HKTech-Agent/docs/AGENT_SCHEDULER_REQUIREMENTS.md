# Agent 内置任务调度器 - 需求说明书

**版本**: v1.0  
**日期**: 2026-02-24  
**作者**: 阿莱士/Alex  
**状态**: ✅ 已完成需求分析

---

## 1. 背景与问题

### 1.1 当前问题

**OpenClaw 内置 Cron 连续失败**：
- 2/23：盘前学习、AI 简报全部错过
- 2/24：盘前学习、AI 简报再次错过

**根本原因**：
- `nextRunAtMs` 计算逻辑缺陷 - 错过整点就跳到明天
- 没有"错过补偿"机制
- 没有执行历史记录
- 无状态，依赖 scheduler 唤醒时间

### 1.2 架构反思

**信息推送类 vs Agent 运行类**：

| 维度 | 信息推送类 | Agent 运行类 |
|------|-----------|-------------|
| 示例 | 日报推送、简报推送 | 盘前学习、午间学习、盘后学习 |
| 状态 | 无状态 | 有状态（需学习、记忆、分析） |
| 复杂度 | 低（格式化 + 发送） | 高（数据获取、LLM 分析、决策） |
| 执行时间 | 秒级 | 分钟级 |
| 失败恢复 | 重发即可 | 需重新执行完整流程 |
| 调度方式 | 外部 cron ✅ | 需要 Agent 自调度 |

---

## 2. 需求概述

### 2.1 目标

构建 **Agent 内置任务调度器**，实现：
1. **可靠执行** - 不因系统启动时间错过任务
2. **状态追踪** - 记录每次执行状态、耗时、结果
3. **错过补偿** - 检测到错过时自动补发
4. **学习优化** - 记录执行历史，优化调度策略

### 2.2 范围

**纳入调度器的任务**：

| 任务名 | 时间 | 频率 | 类型 |
|--------|------|------|------|
| 盘前学习 | 工作日 9:00 AM | 每日 | Agent 运行类 |
| 午间学习 | 工作日 12:30 PM | 每日 | Agent 运行类 |
| 盘后学习 | 工作日 4:30 PM | 每日 | Agent 运行类 |
| AI 技术学习简报 | 每天 8:00 AM | 每日 | 混合（生成 + 推送） |

**不纳入的任务**：
- 恒生 Agent 日报推送（9:30 AM）- 纯推送，外部 cron 即可

---

## 3. 功能需求

### 3.1 任务调度核心

#### F1: 任务注册与配置
- **描述**: Agent 启动时加载任务配置
- **输入**: 任务配置文件（JSON/YAML）
- **输出**: 内存中的任务列表
- **验收标准**:
  - 支持 cron 表达式配置
  - 支持任务元数据（名称、描述、超时时间、重试次数）
  - 支持任务依赖关系（可选）

#### F2: 时间检查与触发
- **描述**: 定期检查是否有任务需要执行
- **触发方式**: 
  - Agent 启动时立即检查
  - 每次用户交互时检查（心跳）
  - 定时检查（每 5 分钟）
- **验收标准**:
  - 启动后 1 分钟内完成首次检查
  - 检查逻辑不阻塞正常对话

#### F3: 错过检测与补偿
- **描述**: 检测"应该执行但未执行"的任务
- **判断逻辑**:
  - 当前时间 > 计划执行时间
  - 且今日无执行记录
  - 且在补偿窗口内（默认 30 分钟）
- **补偿策略**:
  - 错过 < 15 分钟：立即执行
  - 错过 15-30 分钟：标记为"补发"，执行简化流程
  - 错过 > 30 分钟：记录日志，不执行，等待人工确认
- **验收标准**:
  - 错过任务在检测后 5 分钟内触发补偿
  - 补发任务有明确标识

#### F4: 任务执行引擎
- **描述**: 调用 sessions_spawn 执行具体任务
- **执行流程**:
  1. 更新任务状态为"执行中"
  2. 调用 sessions_spawn
  3. 监控执行状态
  4. 记录执行结果
- **验收标准**:
  - 支持超时控制
  - 支持失败重试（最多 3 次）
  - 执行日志完整记录

### 3.2 状态管理

#### F5: 执行状态追踪
- **描述**: 记录每次任务执行的详细信息
- **记录内容**:
  - 任务 ID、名称
  - 计划执行时间
  - 实际执行时间
  - 执行状态（pending/running/success/failed/compensated）
  - 执行耗时
  - 子 agent session key
  - 错误信息（如有）
- **存储位置**: `/root/.openclaw/workspace/logs/scheduler/executions.jsonl`
- **验收标准**:
  - 每次执行都有记录
  - 支持按日期查询

#### F6: 状态持久化
- **描述**: Agent 重启后恢复任务状态
- **持久化内容**:
  - 今日已执行任务列表
  - 待补偿任务列表
  - 执行统计（成功/失败次数）
- **存储位置**: `/root/.openclaw/workspace/logs/scheduler/state.json`
- **验收标准**:
  - Agent 重启后状态不丢失
  - 恢复时间 < 5 秒

### 3.3 监控与告警

#### F7: 执行日志
- **描述**: 记录调度器的运行日志
- **日志级别**:
  - INFO: 任务触发、执行完成
  - WARN: 任务错过、补偿触发
  - ERROR: 执行失败、系统异常
- **存储位置**: `/root/.openclaw/workspace/logs/scheduler/scheduler.log`
- **验收标准**:
  - 日志按日期轮转
  - 支持关键词搜索

#### F8: 失败告警
- **描述**: 任务失败时通知用户
- **告警方式**:
  - 飞书消息推送
  - 下次用户交互时提示
- **告警内容**:
  - 任务名称
  - 失败时间
  - 错误信息
  - 建议操作
- **验收标准**:
  - 失败后 10 分钟内告警
  - 告警信息清晰可操作

### 3.4 用户交互

#### F9: 状态查询命令
- **描述**: 用户可查询任务执行状态
- **支持命令**:
  - "今天任务执行了吗？"
  - "查看今日任务状态"
  - "盘前学习执行了吗？"
- **返回内容**:
  - 今日任务列表
  - 每个任务的执行状态
  - 执行时间/耗时
- **验收标准**:
  - 自然语言查询
  - 响应时间 < 3 秒

#### F10: 手动触发
- **描述**: 用户可手动触发任务执行
- **支持命令**:
  - "执行盘前学习"
  - "补发午间报告"
- **验收标准**:
  - 手动触发 bypass 时间检查
  - 执行流程与自动触发一致

---

## 4. 非功能需求

### 4.1 可靠性
- **任务执行成功率**: > 95%
- **错过检测准确率**: 100%
- **状态恢复成功率**: 100%

### 4.2 性能
- **启动检查时间**: < 1 分钟
- **状态查询响应**: < 3 秒
- **调度器开销**: < 5% CPU（空闲时）

### 4.3 可维护性
- **配置文件**: 人类可读（JSON/YAML）
- **日志格式**: 结构化（JSONL）
- **代码注释**: 关键逻辑有注释

### 4.4 可扩展性
- **新增任务**: 修改配置文件即可，无需改代码
- **插件化**: 支持自定义任务处理器

---

## 5. 数据设计

### 5.1 任务配置结构

```json
{
  "tasks": [
    {
      "id": "premarket_learning",
      "name": "盘前学习",
      "description": "获取 overnight 新闻，分析全球市场，生成开盘策略",
      "schedule": {
        "type": "cron",
        "expression": "0 9 * * 1-5",
        "timezone": "Asia/Shanghai"
      },
      "compensation": {
        "enabled": true,
        "window_minutes": 30,
        "simplified_threshold_minutes": 15
      },
      "execution": {
        "timeout_seconds": 300,
        "retry_count": 3,
        "retry_delay_seconds": 60
      },
      "task_prompt": "执行盘前学习任务：\n1. 获取 overnight 新闻...\n2. 分析全球市场...\n..."
    }
  ]
}
```

### 5.2 执行记录结构

```jsonl
{"timestamp":"2026-02-24T09:00:00+08:00","task_id":"premarket_learning","task_name":"盘前学习","scheduled_time":"2026-02-24T09:00:00+08:00","started_time":"2026-02-24T09:00:05+08:00","completed_time":"2026-02-24T09:05:23+08:00","status":"success","compensated":false,"session_key":"agent:main:subagent:xxx","duration_seconds":318,"error":null}
```

### 5.3 状态文件结构

```json
{
  "date": "2026-02-24",
  "last_check": "2026-02-24T10:45:00+08:00",
  "executed_tasks": [
    {
      "task_id": "premarket_learning",
      "executed_at": "2026-02-24T10:41:00+08:00",
      "status": "compensated",
      "session_key": "agent:main:subagent:589f440e..."
    }
  ],
  "pending_compensation": [],
  "stats": {
    "total_scheduled": 4,
    "executed": 2,
    "compensated": 1,
    "failed": 0,
    "pending": 2
  }
}
```

---

## 6. 接口设计

### 6.1 内部接口

#### Scheduler.check_and_execute()
```python
def check_and_execute(self) -> List[ExecutionResult]:
    """检查并执行到期任务"""
    pass
```

#### Scheduler.get_today_status()
```python
def get_today_status(self) -> TodayStatus:
    """获取今日任务执行状态"""
    pass
```

#### Scheduler.manual_trigger(task_id)
```python
def manual_trigger(self, task_id: str) -> ExecutionResult:
    """手动触发任务执行"""
    pass
```

### 6.2 外部接口

#### sessions_spawn
- 用于执行具体任务
- 返回 session_key 用于追踪

#### message
- 用于失败告警推送

---

## 7. 部署与运维

### 7.1 文件结构

```
/root/.openclaw/workspace/Lamda-ai/HKTech-Agent/scheduler/
├── scheduler.py           # 主程序（CLI + 模块）
├── task_config.json       # 任务配置
├── task_registry.py       # 任务注册管理
├── executor.py            # 任务执行器
├── state_manager.py       # 状态管理
├── models.py              # 数据模型
├── requirements.txt       # Python 依赖
└── tests/
    ├── test_scheduler.py
    ├── test_executor.py
    └── test_state.py

/root/.openclaw/workspace/logs/scheduler/
├── scheduler.log          # 运行日志
├── executions.jsonl       # 执行记录
└── state.json             # 当前状态
```

### 7.2 启动流程

1. Agent 启动
2. 加载任务配置
3. 恢复上次状态
4. 执行首次检查（补偿检测）
5. 进入待命状态

### 7.3 监控命令

```bash
# 检查调度器状态
python3 scheduler.py --status

# 查看执行历史
tail -f logs/scheduler/executions.jsonl

# 手动触发任务
python3 scheduler.py --trigger premarket_learning
```

---

## 8. 风险与缓解

### 8.1 技术风险

| 风险 | 影响 | 概率 | 缓解措施 |
|------|------|------|---------|
| sessions_spawn 超时 | 任务阻塞 | 中 | 设置 timeout，异步执行 |
| 状态文件并发写入 | 数据损坏 | 低 | 原子写入（临时文件 + 重命名） |
| crontab 不执行 | 任务错过 | 低 | 心跳触发作为备用 |
| OpenClaw CLI 变更 | 集成失败 | 低 | 版本锁定，定期测试 |

### 8.2 运维风险

| 风险 | 影响 | 概率 | 缓解措施 |
|------|------|------|---------|
| 日志文件过大 | 磁盘占用 | 中 | 按日期轮转，保留 30 天 |
| 配置错误 | 任务不执行 | 中 | 启动时验证配置 |
| 用户误操作 | 状态丢失 | 低 | 定期备份状态文件 |

---

## 9. 验收标准

### 9.1 功能验收

- [ ] 任务配置加载正确
- [ ] 定时触发执行正常
- [ ] 错过检测准确
- [ ] 补偿机制有效
- [ ] 状态持久化可靠
- [ ] 查询命令响应正确
- [ ] 手动触发执行正常

### 9.2 性能验收

- [ ] 启动检查 < 1 分钟
- [ ] 状态查询 < 3 秒
- [ ] 调度器开销 < 5% CPU

### 9.3 稳定性验收

- [ ] 连续运行 7 天无故障
- [ ] 任务成功率 > 95%
- [ ] Agent 重启后状态恢复

---

## 10. 后续优化

### v1.0（MVP）
- [ ] 核心调度逻辑
- [ ] 状态持久化
- [ ] CLI 工具
- [ ] 基础日志

### v1.1（增强）
- [ ] 后台线程检查
- [ ] 失败告警推送
- [ ] 执行统计报表
- [ ] 任务优先级

### v2.0（进阶）
- [ ] 任务依赖关系
- [ ] 动态任务添加
- [ ] Web 管理界面
- [ ] 集成 OpenClaw 核心

---

## 附录

### A. 命令参考

```bash
# 检查并执行任务
python3 scheduler.py --check

# 查看今日状态
python3 scheduler.py --status

# 手动触发任务
python3 scheduler.py --trigger <task_id>

# 健康检查
python3 scheduler.py --health

# 查看执行历史
python3 scheduler.py --history [--date YYYY-MM-DD]

# 验证配置
python3 scheduler.py --validate
```

### B. 配置文件示例

参见 5.1 节 `task_config.json` 完整示例

### C. 执行记录示例

```jsonl
{"timestamp":"2026-02-24T09:05:23+08:00","task_id":"premarket_learning","task_name":"盘前学习","scheduled_time":"2026-02-24T09:00:00+08:00","started_time":"2026-02-24T09:05:05+08:00","completed_time":"2026-02-24T09:05:23+08:00","status":"success","compensated":true,"simplified":true,"session_key":"agent:main:subagent:589f440e...","duration_seconds":18,"error":null}
```

---

**文档状态**: ✅ 需求分析完成  
**下一步**: 技术方案设计 → 实现 → 测试
