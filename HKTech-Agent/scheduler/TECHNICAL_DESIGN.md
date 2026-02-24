# Agent 内置任务调度器 - 技术方案设计

**版本**: v1.0  
**日期**: 2026-02-24  
**作者**: 阿莱士/Alex  
**状态**: 技术方案

---

## 1. 需求变更确认

根据老板反馈，调整以下参数：

| 参数 | 原设计 | 新设计 | 理由 |
|------|--------|--------|------|
| 补偿窗口 | 2 小时 | **30 分钟** | 盘前/午间/盘后学习时效性强，过期补发意义不大 |
| 检查频率 | 30 分钟 | **15 分钟** | 更频繁检查，减少错过时间 |

### 1.1 补偿策略更新

```
错过时间 < 15 分钟  → 立即执行（正常流程）
15 分钟 ≤ 错过时间 < 30 分钟 → 立即执行（简化流程，标注"补发"）
错过时间 ≥ 30 分钟 → 不执行，记录日志，下次用户询问时提示
```

---

## 2. 架构设计

### 2.1 整体架构

```
┌─────────────────────────────────────────────────────────┐
│                    Agent Main Session                   │
├─────────────────────────────────────────────────────────┤
│                                                         │
│  ┌──────────────┐    ┌──────────────┐                  │
│  │   Scheduler  │───▶│   Executor   │                  │
│  │   (调度器)   │    │   (执行器)   │                  │
│  └──────┬───────┘    └──────┬───────┘                  │
│         │                   │                           │
│         ▼                   ▼                           │
│  ┌──────────────┐    ┌──────────────┐                  │
│  │ StateManager │    │ TaskRegistry │                  │
│  │ (状态管理)   │    │ (任务注册)   │                  │
│  └──────────────┘    └──────────────┘                  │
│                                                         │
└─────────────────────────────────────────────────────────┘
         │                   │
         ▼                   ▼
┌─────────────────┐ ┌─────────────────┐
│  sessions_spawn │ │  File System    │
│  (子 agent)     │ │  (状态持久化)   │
└─────────────────┘ └─────────────────┘
```

### 2.2 模块职责

| 模块 | 职责 | 关键方法 |
|------|------|----------|
| **Scheduler** | 调度核心，检查任务、触发执行 | `check_and_execute()`, `get_status()` |
| **Executor** | 任务执行，调用 sessions_spawn | `execute_task()`, `handle_result()` |
| **StateManager** | 状态持久化、恢复 | `load_state()`, `save_state()`, `record_execution()` |
| **TaskRegistry** | 任务配置加载、验证 | `load_tasks()`, `get_task()`, `validate()` |

---

## 3. 技术选型

### 3.1 编程语言

**Python 3.10+**
- 与现有恒生 Agent 代码栈一致
- 丰富的标准库支持（JSON、日志、定时）
- 易于集成到 OpenClaw 工作流

### 3.2 定时机制

**方案选择**: Agent 心跳触发 + 可选后台线程

```python
# 方案 A：心跳触发（推荐）
# 每次用户交互时检查
def on_user_message():
    scheduler.check_and_execute()
    # ... 处理用户消息

# 方案 B：后台线程（可选增强）
import threading
import time

def background_checker():
    while True:
        time.sleep(900)  # 15 分钟
        scheduler.check_and_execute()

threading.Thread(target=background_checker, daemon=True).start()
```

**决策**: 
- **v1.0**: 仅心跳触发（简单、可靠）
- **v1.1**: 增加后台线程（需要 OpenClaw 支持长运行进程）

### 3.3 数据存储

| 数据类型 | 格式 | 位置 | 说明 |
|---------|------|------|------|
| 任务配置 | JSON | `scheduler/task_config.json` | 静态配置，手动编辑 |
| 执行记录 | JSONL | `logs/scheduler/executions.jsonl` | 追加写入，按日期轮转 |
| 当前状态 | JSON | `logs/scheduler/state.json` | 每次更新后写入 |
| 运行日志 | Text | `logs/scheduler/scheduler.log` | 按日期轮转，保留 30 天 |

### 3.4 错误处理

```python
class SchedulerError(Exception):
    """调度器基础异常"""
    pass

class TaskExecutionError(SchedulerError):
    """任务执行失败"""
    pass

class ConfigurationError(SchedulerError):
    """配置错误"""
    pass

# 重试装饰器
def retry(max_attempts=3, delay=60):
    def decorator(func):
        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            for attempt in range(max_attempts):
                try:
                    return func(*args, **kwargs)
                except Exception as e:
                    if attempt == max_attempts - 1:
                        raise
                    time.sleep(delay)
            return None
        return wrapper
    return decorator
```

---

## 4. 详细设计

### 4.1 任务配置结构

**文件**: `scheduler/task_config.json`

```json
{
  "version": "1.0",
  "timezone": "Asia/Shanghai",
  "compensation": {
    "enabled": true,
    "window_minutes": 30,
    "simplified_threshold_minutes": 15
  },
  "tasks": [
    {
      "id": "premarket_learning",
      "name": "盘前学习",
      "description": "获取 overnight 新闻，分析全球市场，生成开盘策略",
      "enabled": true,
      "schedule": {
        "type": "cron",
        "expression": "0 9 * * 1-5"
      },
      "execution": {
        "timeout_seconds": 300,
        "retry_count": 3,
        "retry_delay_seconds": 60,
        "model": "bailian/qwen3.5-plus",
        "label_template": "盘前学习 - {date}"
      },
      "prompt": "执行盘前学习任务：\n\n1. 获取 overnight 新闻和全球市场动态\n2. 分析美股/欧股/日股收盘对港股的影响\n3. 阅读当天重要财经新闻（用 LLM 提取情绪信号）\n4. 更新市场日历，确认今日无重大事件\n5. 生成开盘策略建议\n6. 推送到飞书\n\n输出格式：\n🌅 盘前学习报告 - Day X\n📰 全球 markets overnight\n📊 港股开盘策略\n⚠️ 风险提示"
    },
    {
      "id": "noon_learning",
      "name": "午间学习",
      "description": "获取上午港股收盘数据，分析上午行情，调整下午策略",
      "enabled": true,
      "schedule": {
        "type": "cron",
        "expression": "30 12 * * 1-5"
      },
      "execution": {
        "timeout_seconds": 300,
        "retry_count": 3,
        "model": "bailian/qwen3.5-plus",
        "label_template": "午间学习 - {date}"
      },
      "prompt": "执行午间学习任务：\n\n1. 获取上午港股收盘数据\n2. 分析上午行情特征\n3. 对比开盘预测 vs 实际走势\n4. 更新持仓盈亏\n5. 调整下午策略\n6. 推送到飞书"
    },
    {
      "id": "afternoon_learning",
      "name": "盘后学习",
      "description": "获取全天港股收盘数据，执行回测，更新世界模型",
      "enabled": true,
      "schedule": {
        "type": "cron",
        "expression": "30 16 * * 1-5"
      },
      "execution": {
        "timeout_seconds": 360,
        "retry_count": 3,
        "model": "bailian/qwen3.5-plus",
        "label_template": "盘后学习 - {date}"
      },
      "prompt": "执行盘后学习任务：\n\n1. 获取全天港股收盘数据\n2. 执行回测验证\n3. 更新世界模型\n4. 记录交易日志\n5. 生成全天学习报告\n6. 推送到飞书"
    },
    {
      "id": "ai_briefing",
      "name": "AI 技术学习简报",
      "description": "生成 AI 技术学习简报（网页版）并推送",
      "enabled": true,
      "schedule": {
        "type": "cron",
        "expression": "0 8 * * *"
      },
      "execution": {
        "timeout_seconds": 300,
        "retry_count": 3,
        "model": "bailian/qwen3.5-plus",
        "label_template": "AI 技术学习简报 - {date}"
      },
      "prompt": "生成并推送 AI 技术学习简报（网页版）：\n\n1. 获取今日日期和星期\n2. 根据星期确定本周主题\n3. 搜集 arXiv 最新论文\n4. 生成深度简报\n5. 创建 HTML 网页版\n6. 推送到飞书双群"
    }
  ]
}
```

### 4.2 核心类设计

#### Scheduler 类

```python
class Scheduler:
    """任务调度器核心"""
    
    def __init__(self, config_path: str, state_path: str, log_path: str):
        self.task_registry = TaskRegistry(config_path)
        self.state_manager = StateManager(state_path)
        self.executor = Executor()
        self.logger = self._setup_logger(log_path)
    
    def check_and_execute(self) -> List[ExecutionResult]:
        """检查并执行到期任务"""
        results = []
        now = datetime.now(timezone.utc).astimezone(
            timezone(timedelta(hours=8))  # Asia/Shanghai
        )
        today = now.date()
        
        # 加载今日状态
        state = self.state_manager.load_state(today)
        
        for task in self.task_registry.get_enabled_tasks():
            # 检查是否已执行
            if state.is_executed_today(task.id):
                self.logger.debug(f"任务 {task.name} 今日已执行，跳过")
                continue
            
            # 计算计划执行时间
            scheduled_time = task.get_next_scheduled_time(now)
            
            # 检查是否到期
            if scheduled_time > now:
                continue  # 还未到时间
            
            # 检查补偿窗口
            missed_minutes = (now - scheduled_time).total_seconds() / 60
            if missed_minutes > self.task_registry.compensation_window:
                self.logger.warning(f"任务 {task.name} 错过补偿窗口 ({missed_minutes:.0f}分钟)")
                state.mark_missed(task.id)
                continue
            
            # 执行任务
            self.logger.info(f"触发任务：{task.name} (错过{missed_minutes:.0f}分钟)")
            result = self.executor.execute_task(
                task=task,
                compensated=missed_minutes > 0,
                simplified=missed_minutes > self.task_registry.simplified_threshold
            )
            
            # 记录执行结果
            state.record_execution(task.id, result)
            results.append(result)
        
        # 保存状态
        self.state_manager.save_state(state)
        return results
    
    def get_today_status(self) -> TodayStatus:
        """获取今日任务执行状态"""
        today = datetime.now(timezone.utc).astimezone(
            timezone(timedelta(hours=8))
        ).date()
        state = self.state_manager.load_state(today)
        return state.get_status()
    
    def manual_trigger(self, task_id: str) -> ExecutionResult:
        """手动触发任务执行"""
        task = self.task_registry.get_task(task_id)
        if not task:
            raise SchedulerError(f"任务不存在：{task_id}")
        
        return self.executor.execute_task(task, compensated=False, simplified=False)
```

#### Executor 类

```python
class Executor:
    """任务执行器"""
    
    def __init__(self):
        self.logger = logging.getLogger(__name__)
    
    def execute_task(self, task: Task, compensated: bool, simplified: bool) -> ExecutionResult:
        """执行单个任务"""
        start_time = datetime.now()
        session_key = None
        
        try:
            # 构建任务 prompt
            prompt = self._build_prompt(task, compensated, simplified)
            
            # 调用 sessions_spawn
            spawn_result = self._call_sessions_spawn(task, prompt)
            session_key = spawn_result.session_key
            
            # 等待执行完成（可选：异步监控）
            # 简化模式下不等待，直接返回
            
            # 记录成功
            return ExecutionResult.success(
                task_id=task.id,
                session_key=session_key,
                duration=(datetime.now() - start_time).total_seconds(),
                compensated=compensated,
                simplified=simplified
            )
            
        except Exception as e:
            self.logger.error(f"任务执行失败：{task.name} - {e}")
            return ExecutionResult.failure(
                task_id=task.id,
                error=str(e),
                duration=(datetime.now() - start_time).total_seconds()
            )
    
    def _call_sessions_spawn(self, task: Task, prompt: str) -> SpawnResult:
        """调用 openclaw sessions spawn"""
        # 使用 subprocess 调用 openclaw CLI
        label = task.label_template.format(date=datetime.now().strftime("%Y-%m-%d"))
        
        cmd = [
            "openclaw", "sessions", "spawn",
            "--label", label,
            "--model", task.execution.model,
            "--timeout", str(task.execution.timeout_seconds),
            prompt
        ]
        
        result = subprocess.run(cmd, capture_output=True, text=True, timeout=60)
        
        if result.returncode != 0:
            raise TaskExecutionError(f"sessions spawn 失败：{result.stderr}")
        
        # 解析输出，提取 session_key
        return self._parse_spawn_output(result.stdout)
```

#### StateManager 类

```python
class StateManager:
    """状态管理器"""
    
    def __init__(self, state_path: str, executions_path: str):
        self.state_path = state_path
        self.executions_path = executions_path
    
    def load_state(self, date: date) -> DailyState:
        """加载指定日期的状态"""
        if not os.path.exists(self.state_path):
            return DailyState(date=date)
        
        try:
            with open(self.state_path, 'r') as f:
                data = json.load(f)
            
            if data.get('date') != date.isoformat():
                return DailyState(date=date)  # 日期不匹配，新建
            
            return DailyState.from_dict(data)
        
        except (json.JSONDecodeError, IOError) as e:
            logging.warning(f"状态文件读取失败：{e}，使用空状态")
            return DailyState(date=date)
    
    def save_state(self, state: DailyState):
        """保存状态"""
        # 原子写入（先写临时文件，再重命名）
        temp_path = self.state_path + '.tmp'
        with open(temp_path, 'w') as f:
            json.dump(state.to_dict(), f, indent=2, ensure_ascii=False)
        os.replace(temp_path, self.state_path)
    
    def record_execution(self, execution: ExecutionResult):
        """记录执行日志（JSONL）"""
        with open(self.executions_path, 'a') as f:
            f.write(json.dumps(execution.to_dict(), ensure_ascii=False) + '\n')
```

### 4.3 执行流程图

```
Agent 启动/用户交互
        │
        ▼
┌───────────────────┐
│ Scheduler.check() │
└─────────┬─────────┘
          │
          ▼
┌───────────────────┐
│ 加载今日状态      │
└─────────┬─────────┘
          │
          ▼
┌───────────────────┐
│ 遍历任务列表      │
└─────────┬─────────┘
          │
    ┌─────┴─────┐
    │           │
    ▼           ▼
已执行？    未执行？
    │           │
    │           ▼
    │     ┌─────────────┐
    │     │ 计算错过时间 │
    │     └──────┬──────┘
    │            │
    │      ┌─────┴─────┐
    │      │           │
    │      ▼           ▼
    │   <30 分钟    ≥30 分钟
    │      │           │
    │      │           ▼
    │      │     标记错过
    │      │           │
    │      ▼           │
    │ ┌────────────┐   │
    │ │执行任务    │   │
    │ └──────┬─────┘   │
    │        │         │
    │        ▼         │
    │ ┌────────────┐   │
    │ │记录状态    │   │
    │ └──────┬─────┘   │
    │        │         │
    └────────┼─────────┘
             │
             ▼
┌───────────────────┐
│ 保存状态文件      │
└─────────┬─────────┘
          │
          ▼
      完成
```

---

## 5. 集成方案

### 5.1 与 OpenClaw 集成

**方案 A：作为 Python 模块导入（推荐）**

```python
# 在 Agent 主循环中
from Lamda-ai.HKTech-Agent.scheduler import Scheduler

scheduler = Scheduler(
    config_path="/root/.openclaw/workspace/Lamda-ai/HKTech-Agent/scheduler/task_config.json",
    state_path="/root/.openclaw/workspace/logs/scheduler/state.json",
    log_path="/root/.openclaw/workspace/logs/scheduler/scheduler.log"
)

# 每次用户消息时检查
def on_user_message(message):
    scheduler.check_and_execute()
    # ... 处理用户消息
```

**方案 B：作为独立 CLI 工具**

```bash
# 在 HEARTBEAT.md 中添加
*/15 * * * * /root/.openclaw/workspace/Lamda-ai/HKTech-Agent/scheduler/scheduler.py --check
```

**决策**: v1.0 使用方案 B（独立 CLI），v1.1 升级为方案 A（模块集成）

### 5.2 与现有 Cron 共存

```bash
# /etc/crontab 或 crontab -e

# 外部 Cron（保留）- 简单推送
5 8 * * *   cd /root/.openclaw/workspace && ./Lamda-ai/HKTech-Agent/scripts/cron_scheduler.sh daily_briefing
35 9 * * 1-5 cd /root/.openclaw/workspace && ./Lamda-ai/HKTech-Agent/scripts/cron_scheduler.sh morning_briefing

# Agent 调度器检查（新增）
*/15 * * * * cd /root/.openclaw/workspace && python3 Lamda-ai/HKTech-Agent/scheduler/scheduler.py --check >> logs/scheduler/cron_check.log 2>&1
```

**注意**: 外部 Cron 和 Agent 调度器可能同时触发同一任务，需要通过状态文件去重。

---

## 6. 测试计划

### 6.1 单元测试

```python
# test_scheduler.py

def test_task_scheduling():
    """测试任务调度逻辑"""
    scheduler = Scheduler(test_config)
    # 模拟不同时间场景
    # 验证触发逻辑正确

def test_compensation_logic():
    """测试补偿逻辑"""
    # 错过 10 分钟 → 正常执行
    # 错过 20 分钟 → 简化执行
    # 错过 40 分钟 → 不执行

def test_state_persistence():
    """测试状态持久化"""
    # 写入状态 → 重启 → 读取状态
    # 验证状态不丢失
```

### 6.2 集成测试

1. **场景 1**: Agent 启动时检查补偿
   - 模拟 8:50 启动（错过 8:00 AI 简报 50 分钟）
   - 验证：不执行，记录错过

2. **场景 2**: 任务执行成功
   - 模拟 8:55 检查（AI 简报未到时间）
   - 模拟 9:05 检查（盘前学习错过 5 分钟）
   - 验证：执行盘前学习，状态更新

3. **场景 3**: Agent 重启恢复
   - 执行一个任务
   - 重启 Agent
   - 验证：状态恢复，不重复执行

### 6.3 压力测试

- 连续运行 7 天
- 模拟高并发用户交互
- 验证调度器性能开销

---

## 7. 部署计划

### 7.1 文件清单

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

### 7.2 部署步骤

1. **创建目录结构**
   ```bash
   mkdir -p /root/.openclaw/workspace/Lamda-ai/HKTech-Agent/scheduler
   mkdir -p /root/.openclaw/workspace/logs/scheduler
   ```

2. **部署代码文件**
   - 复制 Python 模块
   - 创建任务配置

3. **配置 crontab**
   ```bash
   crontab -e
   # 添加：*/15 * * * * ...scheduler.py --check
   ```

4. **首次运行测试**
   ```bash
   cd /root/.openclaw/workspace
   python3 Lamda-ai/HKTech-Agent/scheduler/scheduler.py --status
   ```

5. **监控日志**
   ```bash
   tail -f logs/scheduler/scheduler.log
   ```

### 7.3 回滚方案

如出现问题：
1. 移除 crontab 配置
2. 恢复旧版 cron_scheduler.sh
3. 禁用调度器

---

## 8. 监控与运维

### 8.1 健康检查

```bash
# 检查调度器状态
python3 scheduler.py --health

# 输出示例：
# ✅ Scheduler: OK
# ✅ Last check: 2026-02-24 10:45:00
# ✅ Tasks today: 4 scheduled, 2 executed, 0 failed
```

### 8.2 日志分析

```bash
# 查看今日执行记录
cat logs/scheduler/executions.jsonl | grep "2026-02-24" | python3 -m json.tool

# 查看失败任务
grep "ERROR" logs/scheduler/scheduler.log | tail -20

# 统计执行成功率
cat logs/scheduler/executions.jsonl | jq -r '.status' | sort | uniq -c
```

### 8.3 告警规则

| 条件 | 告警级别 | 操作 |
|------|----------|------|
| 任务失败 | WARN | 记录日志，下次用户交互时提示 |
| 连续 2 次失败 | ERROR | 立即飞书推送告警 |
| 状态文件损坏 | ERROR | 立即告警，尝试恢复 |
| 错过率 > 20% | WARN | 周报中提醒优化 |

---

## 9. 风险与缓解

### 9.1 技术风险

| 风险 | 影响 | 概率 | 缓解措施 |
|------|------|------|---------|
| sessions_spawn 超时 | 任务阻塞 | 中 | 设置 timeout，异步执行 |
| 状态文件并发写入 | 数据损坏 | 低 | 原子写入（临时文件 + 重命名） |
| crontab 不执行 | 任务错过 | 低 | 心跳触发作为备用 |
| OpenClaw CLI 变更 | 集成失败 | 低 | 版本锁定，定期测试 |

### 9.2 运维风险

| 风险 | 影响 | 概率 | 缓解措施 |
|------|------|------|---------|
| 日志文件过大 | 磁盘占用 | 中 | 按日期轮转，保留 30 天 |
| 配置错误 | 任务不执行 | 中 | 启动时验证配置 |
| 用户误操作 | 状态丢失 | 低 | 定期备份状态文件 |

---

## 10. 后续优化路线

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

参见 4.1 节 `task_config.json` 完整示例

### C. 执行记录示例

```jsonl
{"timestamp":"2026-02-24T09:05:23+08:00","task_id":"premarket_learning","task_name":"盘前学习","scheduled_time":"2026-02-24T09:00:00+08:00","started_time":"2026-02-24T09:05:05+08:00","completed_time":"2026-02-24T09:05:23+08:00","status":"success","compensated":true,"simplified":true,"session_key":"agent:main:subagent:589f440e...","duration_seconds":18,"error":null}
```

---

**文档状态**: 技术方案完成，待评审  
**下一步**: 实现 → 测试 → 部署
