#!/bin/bash
# 会话恢复脚本
# 当 Coding Agent 崩溃后，快速恢复到之前的状态

PROJECT_DIR="${1:-$(pwd)}"
cd "$PROJECT_DIR"

echo "🔄 Coding Agent 会话恢复"
echo "========================"
echo ""

# 1. 检查 Git 状态
echo "📊 Git 状态:"
git status --short
echo ""

# 2. 显示最近的提交历史
echo "📜 最近提交:"
git log --oneline -5
echo ""

# 3. 显示最近的 WIP 提交
echo "🔍 最近的自动保存:"
git log --oneline --grep="WIP:" -5
echo ""

# 4. 检查是否有未提交的更改
if ! git diff --quiet HEAD 2>/dev/null; then
    echo "⚠️  发现未提交的更改:"
    git diff --stat
    echo ""
    
    read -p "是否提交未保存的更改? (y/n): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        git add -A
        git commit -m "WIP: Recovery commit - $(date '+%Y-%m-%d %H:%M:%S')"
        echo "✅ 已提交"
    fi
fi

# 5. 显示当前分支和工作进度
echo ""
echo "🌿 当前分支: $(git branch --show-current)"
echo ""

# 6. 如果有进度文件，显示进度
if [ -f .coding-progress.json ]; then
    echo "📋 任务进度:"
    cat .coding-progress.json
    echo ""
fi

# 7. 启动建议
echo "🚀 恢复选项:"
echo ""
echo "1. 继续当前工作:"
echo "   ./coding-session.sh attach"
echo ""
echo "2. 查看详细日志:"
echo "   git log --oneline -10"
echo ""
echo "3. 回滚到上一个稳定版本:"
echo "   git reset --soft HEAD~1"
echo ""
echo "4. 放弃所有更改重新开始:"
echo "   git reset --hard HEAD"
echo ""

# 8. 自动附加到会话（如果存在）
if tmux has-session -t coding 2>/dev/null; then
    echo "✅ 发现活跃的 tmux 会话"
    read -p "是否立即附加? (y/n): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        tmux attach -t coding
    fi
else
    echo "ℹ️  无活跃会话，运行以下命令启动:"
    echo "   ./coding-session.sh start"
fi
