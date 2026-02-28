#!/bin/bash
# 恒生 Agent 定时任务调度器 - 简化版
# 直接使用 message 工具推送，不需要子 agent

LOG_DIR="/root/.openclaw/workspace/logs/cron"
mkdir -p "$LOG_DIR"

TIMESTAMP=$(date +"%Y-%m-%d %H:%M:%S")
DATE_STAMP=$(date +"%Y-%m-%d")

log() {
    echo "[$TIMESTAMP] $1" | tee -a "$LOG_DIR/cron_$DATE_STAMP.log"
}

# 飞书群组配置
FEISHU_GROUP1="oc_1c509d279aa4bd1b785a57f1cc13427c"  # 投资人专属群
FEISHU_GROUP2="oc_d5f6f6f591bc129e4ae9037b0acdd3a5"  # 牛马 Agent 消息群

case "$1" in
    "morning_briefing")
        log "🌅 开始执行：盘前学习"
        # 直接调用恒生 Agent 生产脚本
        cd /opt/hktech-agent && source venv/bin/activate && python3 prod/src/main.py --mode premarket 2>&1 | tee -a "$LOG_DIR/morning_$DATE_STAMP.log"
        log "✅ 盘前学习完成"
        ;;
    
    "noon_learning")
        log "🌞 开始执行：午间学习"
        # 直接调用恒生 Agent 生产脚本
        cd /opt/hktech-agent && source venv/bin/activate && python3 prod/src/main.py --mode noon 2>&1 | tee -a "$LOG_DIR/noon_$DATE_STAMP.log"
        log "✅ 午间学习完成"
        ;;
    
    "afternoon_learning")
        log "🌙 开始执行：盘后学习"
        # 直接调用恒生 Agent 生产脚本
        cd /opt/hktech-agent && source venv/bin/activate && python3 prod/src/main.py --mode postmarket 2>&1 | tee -a "$LOG_DIR/afternoon_$DATE_STAMP.log"
        log "✅ 盘后学习完成"
        ;;
    
    "daily_briefing")
        log "📬 开始执行：AI 技术学习简报"
        # 使用已有的网页版简报脚本
        cd /root/.openclaw/workspace && python3 reports/send_briefing_webpage.py 2>&1 | tee -a "$LOG_DIR/briefing_$DATE_STAMP.log"
        log "✅ AI 简报完成"
        ;;
    
    *)
        echo "用法：$0 {morning_briefing|noon_learning|afternoon_learning|daily_briefing}"
        exit 1
        ;;
esac
