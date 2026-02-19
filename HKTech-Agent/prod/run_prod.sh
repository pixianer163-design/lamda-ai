#!/bin/bash
# 自动生成的运行脚本

set -e

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd $DIR

source /opt/hktech-agent/venv_rl/bin/activate

# 加载环境变量配置
if [ -f "/opt/hktech-agent/config/config.env" ]; then
    export $(grep -v '^#' /opt/hktech-agent/config/config.env | xargs)
    echo "✅ 已加载配置文件"
fi

# 设置环境变量
export PYTHONPATH="$DIR/src:$PYTHONPATH"
export AGENT_ENV="prod"
export AGENT_LOG_DIR="$DIR/logs"

mkdir -p $AGENT_LOG_DIR

echo "=========================================="
echo "🚀 恒生科技Agent - prod环境"
echo "=========================================="
echo "时间: $(date)"
echo "=========================================="

# 显示配置状态
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

# 运行主程序
python3 $DIR/src/llm_enhanced_agent.py 2>&1 | tee $AGENT_LOG_DIR/run_$(date +%Y%m%d_%H%M%S).log

echo ""
echo "📱 发送日报到飞书..."
python3 $DIR/src/daily_report_sender.py 2>&1 | tee -a $AGENT_LOG_DIR/run_$(date +%Y%m%d_%H%M%S).log

echo "=========================================="
echo "✅ 运行完成: $(date)"
echo "=========================================="
