#!/bin/bash
# 自动 Commit 守护进程
# 每30分钟自动 commit，保留开发进度

PROJECT_DIR="${1:-$(pwd)}"
cd "$PROJECT_DIR"

echo "🤖 自动 Commit 守护进程已启动"
echo "📍 项目目录: $PROJECT_DIR"
echo "⏰ 每30分钟自动保存进度"
echo "💾 按 Ctrl+C 停止"

# 计数器
SAVE_COUNT=0

while true; do
    sleep 1800  # 30分钟
    
    SAVE_COUNT=$((SAVE_COUNT + 1))
    TIMESTAMP=$(date '+%Y-%m-%d %H:%M:%S')
    
    # 检查是否有更改
    if git diff --quiet HEAD 2>/dev/null; then
        echo "[$TIMESTAMP] ⏸️  无更改，跳过"
        continue
    fi
    
    # 自动 commit
    git add -A
    git commit -m "WIP: Auto-save progress #$SAVE_COUNT

- Timestamp: $TIMESTAMP
- Session: coding-agent
- Status: In progress

This is an automatic checkpoint commit." || {
        echo "[$TIMESTAMP] ⚠️  Commit 失败"
        continue
    }
    
    echo "[$TIMESTAMP] ✅ 已保存进度 #$SAVE_COUNT"
    
    # 可选：自动 push（每3次保存）
    if [ $((SAVE_COUNT % 3)) -eq 0 ]; then
        git push origin $(git branch --show-current) 2>/dev/null && \
            echo "[$TIMESTAMP] ☁️  已推送至远程" || \
            echo "[$TIMESTAMP] ⚠️  推送失败（可能无远程仓库）"
    fi
done
