#!/bin/bash
# Coding Agent 会话管理脚本
# 用法: ./coding-session.sh [start|attach|kill|list]

SESSION_NAME="coding"
PROJECT_DIR="${1:-$(pwd)}"

case "${2:-start}" in
    start)
        echo "🚀 启动 Coding Agent 会话..."
        
        # 检查是否已存在
        if tmux has-session -t $SESSION_NAME 2>/dev/null; then
            echo "⚠️  会话已存在，正在附加..."
            tmux attach -t $SESSION_NAME
        else
            # 创建新会话
            tmux new-session -d -s $SESSION_NAME -c "$PROJECT_DIR"
            
            # 设置窗口标题
            tmux rename-window -t $SESSION_NAME:0 "coding"
            
            # 发送初始化命令
            tmux send-keys -t $SESSION_NAME:0 "cd $PROJECT_DIR && echo '🚀 Coding Agent 已启动' && echo '💡 提示: 每小时自动 commit，崩溃后运行 ./coding-session.sh attach'" Enter
            
            # 启动自动 commit 守护进程（后台）
            tmux send-keys -t $SESSION_NAME:0 "./auto-commit.sh &" Enter
            
            # 附加到会话
            tmux attach -t $SESSION_NAME
        fi
        ;;
    
    attach)
        echo "🔗 附加到现有会话..."
        if tmux has-session -t $SESSION_NAME 2>/dev/null; then
            tmux attach -t $SESSION_NAME
        else
            echo "❌ 会话不存在，请先运行: ./coding-session.sh start"
        fi
        ;;
    
    kill)
        echo "🛑 停止会话..."
        tmux kill-session -t $SESSION_NAME 2>/dev/null && echo "✅ 已停止" || echo "❌ 会话不存在"
        ;;
    
    list)
        echo "📋 当前会话列表:"
        tmux list-sessions 2>/dev/null || echo "无活跃会话"
        ;;
    
    status)
        if tmux has-session -t $SESSION_NAME 2>/dev/null; then
            echo "✅ 会话正在运行"
            tmux list-windows -t $SESSION_NAME
        else
            echo "❌ 会话未运行"
        fi
        ;;
    
    *)
        echo "用法: $0 [项目目录] [start|attach|kill|list|status]"
        echo ""
        echo "命令:"
        echo "  start   - 启动新会话（或附加到现有会话）"
        echo "  attach  - 附加到现有会话"
        echo "  kill    - 停止会话"
        echo "  list    - 列出所有会话"
        echo "  status  - 查看会话状态"
        echo ""
        echo "示例:"
        echo "  $0 /path/to/project start"
        echo "  $0 attach"
        exit 1
        ;;
esac
