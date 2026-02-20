# Coding Agent 会话管理指南

解决 Code Agent 异常退出导致进度丢失的问题。

## 🎯 方案概述

**核心组合**: tmux 持久化会话 + 自动 Git commit

- ✅ **tmux**: 网络断开/终端关闭后，会话仍在后台运行
- ✅ **自动 commit**: 每30分钟自动保存代码进度
- ✅ **恢复脚本**: 崩溃后快速恢复到之前状态

---

## 📁 文件说明

| 文件 | 用途 |
|------|------|
| `coding-session.sh` | 启动/附加/管理 tmux 会话 |
| `auto-commit.sh` | 每30分钟自动 commit 的守护进程 |
| `recover-session.sh` | 崩溃后恢复会话状态 |
| `.coding-progress.template.json` | 任务进度跟踪模板 |

---

## 🚀 快速开始

### 1. 首次使用（设置权限）

```bash
cd /path/to/your/project

# 复制脚本到项目
cp /path/to/scripts/*.sh ./
chmod +x *.sh

# 可选：复制进度模板
cp .coding-progress.template.json .coding-progress.json
```

### 2. 启动 Coding Agent（推荐方式）

```bash
# 启动新会话
./coding-session.sh /path/to/project start

# 在 tmux 会话中启动 Code Agent
claude code

# 开始工作...
```

### 3. 正常工作流程

```bash
# 在 Code Agent 中开发...
# 自动 commit 会在后台每30分钟执行

# 重要里程碑手动 commit
git add -A
git commit -m "feat: 完成 DPML 显式记忆网络"
git push
```

### 4. 如果崩溃了...

```bash
# 方法1: 直接恢复 tmux 会话
./coding-session.sh attach

# 方法2: 使用恢复脚本查看状态
./recover-session.sh

# 方法3: 查看自动保存的 commit
git log --oneline --grep="WIP:" -10
```

---

## 📋 完整命令参考

### coding-session.sh

```bash
# 启动新会话（或附加到现有会话）
./coding-session.sh /path/to/project start

# 附加到现有会话
./coding-session.sh attach

# 查看会话状态
./coding-session.sh status

# 列出所有会话
./coding-session.sh list

# 停止会话（慎用）
./coding-session.sh kill
```

### Git 相关

```bash
# 查看自动保存的历史
git log --oneline --grep="WIP:"

# 回滚到上一个自动保存点
git reset --soft HEAD~1

# 查看崩溃前的更改
git show HEAD --stat

# 推送到远程（可选）
git push origin feature/xxx
```

### 手动触发自动保存

```bash
# 如果不等30分钟，手动触发
./auto-commit.sh

# 或手动 commit
git add -A && git commit -m "WIP: 手动保存"
```

---

## 💡 最佳实践

### 1. 任务拆分建议

**小任务** (< 2小时):
- 不需要 tmux，直接运行 Code Agent
- 完成后立即 commit

**中任务** (2-4小时) ⭐ 推荐:
- 使用 tmux + 自动 commit
- 每小时手动 commit 里程碑
- 任务完成前 push 到远程

**大任务** (> 4小时):
- 拆分为多个中任务
- 每个子任务一个 feature 分支
- 使用 Git worktree 并行开发

### 2. Commit 规范

**自动 commit 格式**:
```
WIP: Auto-save progress #3

- Timestamp: 2026-02-19 14:30:00
- Session: coding-agent
- Status: In progress
```

**手动 commit 格式**:
```
feat: 完成 DPML 显式记忆网络

- 实现 LSTM 编码器
- 添加梯度裁剪
- 通过单元测试
```

### 3. 进度跟踪

创建 `.coding-progress.json` 文件:

```json
{
  "project": "Agent Factory",
  "task": "DPML World Model",
  "completed": ["显式记忆"],
  "in_progress": "隐式记忆",
  "pending": ["门控融合", "测试"],
  "estimated_remaining": "2h"
}
```

---

## 🛠️ 故障排除

### 问题1: tmux 会话丢失

```bash
# 检查是否存在
tmux list-sessions

# 如果存在但无法附加
pkill -f tmux  # 强制停止
tmux new -s coding  # 新建
```

### 问题2: 自动 commit 停止

```bash
# 检查进程
ps aux | grep auto-commit

# 重新启动
./auto-commit.sh &
```

### 问题3: 代码冲突

```bash
# 查看自动 commit 和当前更改
git log --oneline -5
git status

# 如果有冲突，手动解决
git add -A
git commit -m "fix: 解决合并冲突"
```

### 问题4: 忘记保存进度

```bash
# 查看最近的更改（包括未提交的）
git diff HEAD

# 恢复到最近一次自动保存
git stash  # 暂存当前更改
git log --grep="WIP:" --oneline -1  # 找到最新自动保存
git reset --soft HEAD~1  # 回滚到自动保存前
```

---

## 🔧 进阶配置

### 修改自动 commit 间隔

编辑 `auto-commit.sh`:
```bash
# 默认 1800秒 = 30分钟
sleep 1800

# 改为 10分钟
sleep 600
```

### 自动 push 到远程

编辑 `auto-commit.sh`:
```bash
# 找到这行，取消注释
# if [ $((SAVE_COUNT % 3)) -eq 0 ]; then
# 改为每次保存都 push
if [ $((SAVE_COUNT % 1)) -eq 0 ]; then
```

### 集成到 CI/CD

在 `.github/workflows/` 中添加:
```yaml
- name: Check WIP commits
  run: |
    if git log --oneline -1 | grep -q "WIP:"; then
      echo "⚠️  发现未完成的 WIP commit"
    fi
```

---

## 📊 效果对比

| 场景 | 无保护 | 有 tmux+autocommit |
|------|--------|-------------------|
| 网络断开 | 进度丢失 | ✅ 会话保持 |
| 终端关闭 | 进度丢失 | ✅ 后台运行 |
| Code Agent崩溃 | 进度丢失 | ✅ 自动保存 |
| 恢复时间 | 重新开始 | ✅ 1分钟恢复 |

---

## 🎉 总结

**核心收益**:
- 网络/终端问题不再导致进度丢失
- 每30分钟自动保存，最多丢失30分钟工作
- 崩溃后1分钟内恢复工作状态

**使用成本**:
- 首次设置：5分钟
- 日常使用：无额外成本
- 恢复操作：1条命令

---

**开始使用**: `./coding-session.sh start` 🚀
