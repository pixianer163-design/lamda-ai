#!/bin/bash
# 恒生 Agent Cron 任务状态检查脚本

echo "📋 恒生 Agent 定时任务状态检查"
echo "================================"
echo ""

# 检查 cron 服务状态
echo "🔧 Cron 服务状态:"
if systemctl is-active --quiet cron 2>/dev/null; then
    echo "   ✅ cron 服务运行中"
else
    echo "   ❌ cron 服务未运行"
fi
echo ""

# 检查 crontab 配置
echo "⏰ 已配置的定时任务:"
crontab -l 2>/dev/null | grep -E "cron_scheduler|hktech" | while read line; do
    echo "   $line"
done
echo ""

# 检查最近的执行日志
echo "📝 最近的执行日志:"
LOG_DIR="/root/.openclaw/workspace/logs/cron"
if [ -d "$LOG_DIR" ]; then
    TODAY=$(date +"%Y-%m-%d")
    if [ -f "$LOG_DIR/cron_$TODAY.log" ]; then
        echo "   找到今日日志文件：cron_$TODAY.log"
        echo "   最近 10 条记录:"
        tail -10 "$LOG_DIR/cron_$TODAY.log" | sed 's/^/   /'
    else
        echo "   ⚠️  今日日志文件不存在"
    fi
else
    echo "   ⚠️  日志目录不存在"
fi
echo ""

# 检查 OpenClaw cron 状态
echo "🤖 OpenClaw 内置 Cron 状态:"
openclaw cron status 2>/dev/null | head -5 || echo "   无法获取状态"
echo ""

# 下次执行时间
echo "📅 下次执行时间:"
echo "   AI 简报：明天 8:05 AM"
echo "   盘前学习：明天 9:05 AM（工作日）"
echo "   午间学习：今天 12:35 PM（工作日）"
echo "   盘后学习：今天 16:35 PM（工作日）"
echo ""

echo "================================"
echo "检查完成时间：$(date '+%Y-%m-%d %H:%M:%S')"
