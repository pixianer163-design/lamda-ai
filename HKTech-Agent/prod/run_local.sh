#!/bin/bash
# Local development runner for HKTech-Agent production environment
# Uses local paths and mock data

set -e

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd $DIR

# Use system Python (no virtual env required for local testing)
# source /opt/hktech-agent/venv_rl/bin/activate  # Skip for local

# Load local environment variables
LOCAL_CONFIG_DIR="$(dirname "$DIR")/local_config"
if [ -f "$LOCAL_CONFIG_DIR/config.env" ]; then
    export $(grep -v '^#' "$LOCAL_CONFIG_DIR/config.env" | xargs)
    echo "✅ 已加载本地配置文件"
fi

# Set environment variables
export PYTHONPATH="$DIR/src:$DIR/../active_src:$DIR/../shared:$PYTHONPATH"
export AGENT_ENV="${AGENT_ENV:-local}"
export AGENT_LOG_DIR="$DIR/logs"
export AGENT_DATA_DIR="$DIR/../data"

mkdir -p $AGENT_LOG_DIR
mkdir -p $AGENT_DATA_DIR

echo "=========================================="
echo "🚀 恒生科技Agent - 本地开发环境"
echo "=========================================="
echo "时间: $(date)"
echo "环境: $AGENT_ENV"
echo "日志目录: $AGENT_LOG_DIR"
echo "数据目录: $AGENT_DATA_DIR"
echo "=========================================="

# Display configuration status
if [ -n "$FEISHU_WEBHOOK_URL" ]; then
    echo "📱 飞书推送: 已启用"
else
    echo "📱 飞书推送: 模拟模式"
fi
if [ -n "$DEEPSEEK_API_KEY" ]; then
    echo "🔑 API密钥: 已配置"
else
    echo "🔑 API密钥: 使用默认"
fi
echo "=========================================="

# Create dummy data files if they don't exist
if [ ! -f "$AGENT_DATA_DIR/portfolio.json" ]; then
    echo '{"cash": 100000, "holdings": {}}' > "$AGENT_DATA_DIR/portfolio.json"
    echo "📁 创建默认组合文件"
fi
if [ ! -f "$AGENT_DATA_DIR/day_count.json" ]; then
    echo '{"count": 1}' > "$AGENT_DATA_DIR/day_count.json"
    echo "📁 创建默认天数文件"
fi

# Run main program
echo "🤖 启动 LLM Enhanced Agent..."
python3 $DIR/src/llm_enhanced_agent.py 2>&1 | tee $AGENT_LOG_DIR/run_$(date +%Y%m%d_%H%M%S).log

echo "=========================================="
echo "✅ 运行完成: $(date)"
echo "=========================================="