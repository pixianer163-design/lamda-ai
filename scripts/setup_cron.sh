#!/bin/bash
# setup_cron.sh - 设置定时任务

echo "🕐 设置恒生科技Agent定时任务..."

# 创建运行脚本
RUN_SCRIPT="/opt/hktech-agent/run_daily.sh"

cat > $RUN_SCRIPT << 'EOF'
#!/bin/bash
# 恒生科技Agent每日运行脚本

cd /opt/hktech-agent
export DEEPSEEK_API_KEY=sk-5ff142845d464bca9c382a4b4d37e803

# 记录日志
LOG_FILE="/opt/hktech-agent/logs/agent_$(date +%Y%m%d).log"
mkdir -p /opt/hktech-agent/logs

echo "========================================" >> $LOG_FILE
echo "运行时间: $(date)" >> $LOG_FILE
echo "========================================" >> $LOG_FILE

# 运行Agent
python3 src/main.py >> $LOG_FILE 2>&1

echo "" >> $LOG_FILE
echo "完成时间: $(date)" >> $LOG_FILE
echo "" >> $LOG_FILE
EOF

chmod +x $RUN_SCRIPT

# 添加定时任务（香港时间9:30，即北京时间9:30）
CRON_JOB="30 9 * * 1-5 $RUN_SCRIPT"

# 检查是否已存在
if crontab -l 2>/dev/null | grep -q "hktech-agent"; then
    echo "✅ 定时任务已存在"
else
    (crontab -l 2>/dev/null; echo "$CRON_JOB") | crontab -
    echo "✅ 定时任务已添加: 工作日9:30自动运行"
fi

echo ""
echo "📋 当前定时任务:"
crontab -l | grep hktech-agent
echo ""
echo "📁 运行日志: /opt/hktech-agent/logs/agent_YYYYMMDD.log"
