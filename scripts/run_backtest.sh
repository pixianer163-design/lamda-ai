#!/bin/bash
"""
回测执行脚本
统一入口，支持多种回测模式
"""

set -e

LOG_DIR="/opt/hktech-agent/logs"
DATA_DIR="/opt/hktech-agent/data"
BACKTEST_DIR="/opt/hktech-agent/prod/src"
LOG_FILE="${LOG_DIR}/backtest_$(date +%Y%m%d_%H%M%S).log"

# 创建日志目录
mkdir -p "$LOG_DIR"

echo "🧪 开始回测: $(date)" | tee -a "$LOG_FILE"
echo "========================================" | tee -a "$LOG_FILE"

# 检查历史数据
echo "📊 检查历史数据..." | tee -a "$LOG_FILE"
if [ -f "${DATA_DIR}/historical/00700_5y.json" ]; then
    echo "✅ 历史数据存在" | tee -a "$LOG_FILE"
else
    echo "❌ 历史数据不存在，请先收集历史数据" | tee -a "$LOG_FILE"
    exit 1
fi

# 运行简化回测
echo "🚀 运行简化回测..." | tee -a "$LOG_FILE"
cd "$BACKTEST_DIR"
python3 simple_backtest.py 2>&1 | tee -a "$LOG_FILE"

# 检查回测结果
if [ $? -eq 0 ]; then
    echo "✅ 回测完成" | tee -a "$LOG_FILE"
    echo "📁 日志文件: $LOG_FILE" | tee -a "$LOG_FILE"
    
    # 提取关键结果
    echo "📈 回测结果摘要:" | tee -a "$LOG_FILE"
    tail -10 "$LOG_FILE" | grep -E "(初始资金|最终资金|收益率|交易次数)" | tee -a "$LOG_FILE"
else
    echo "❌ 回测失败" | tee -a "$LOG_FILE"
    exit 1
fi

echo "========================================" | tee -a "$LOG_FILE"
echo "🏁 回测结束: $(date)" | tee -a "$LOG_FILE"