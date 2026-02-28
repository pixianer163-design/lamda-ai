#!/bin/bash
# 恒生 Agent 定时任务调度器 - 简化版 v2
# 直接调用恒生 Agent 现有脚本，不需要子 agent

LOG_DIR="/root/.openclaw/workspace/logs/cron"
mkdir -p "$LOG_DIR"

# 加载环境变量（修复 LLM 不通的问题 + 世界模型 v2）
if [ -f "/opt/hktech-agent/config/config.env" ]; then
    set -a
    source /opt/hktech-agent/config/config.env
    set +a
    echo "✅ 已加载环境变量 (DEEPSEEK_API_KEY, WORLD_MODEL_VERSION=$WORLD_MODEL_VERSION)"
fi

TIMESTAMP=$(date +"%Y-%m-%d %H:%M:%S")
DATE_STAMP=$(date +"%Y-%m-%d")

log() {
    echo "[$TIMESTAMP] $1" | tee -a "$LOG_DIR/cron_$DATE_STAMP.log"
}

case "$1" in
    "morning_briefing")
        log "🌅 开始执行：盘前学习"
        # 调用恒生 Agent 生产脚本
        cd /opt/hktech-agent/HKTech-Agent/prod && bash run_prod.sh 2>&1 | tee -a "$LOG_DIR/morning_$DATE_STAMP.log"
        # 生成 Web 面板数据和动态 HTML
        cd /opt/hktech-agent && python3 src/generate_web_data.py 2>&1 | tee -a "$LOG_DIR/web_data_$DATE_STAMP.log"
        cd /opt/hktech-agent && python3 src/generate_dynamic_html.py 2>&1 | tee -a "$LOG_DIR/web_html_$DATE_STAMP.log"
        log "✅ 盘前学习完成"
        ;;
    
    "noon_learning")
        log "🌞 开始执行：午间学习"
        # 1. 运行主程序生成数据 (带环境变量)
        cd /opt/hktech-agent/HKTech-Agent/prod && source /opt/hktech-agent/venv_rl/bin/activate && timeout 180 python3 src/llm_enhanced_agent.py 2>&1 | tee -a "$LOG_DIR/noon_$DATE_STAMP.log"
        # 2. 生成 Web 面板数据和动态 HTML
        cd /opt/hktech-agent && python3 src/generate_web_data.py 2>&1 | tee -a "$LOG_DIR/web_data_$DATE_STAMP.log"
        cd /opt/hktech-agent && python3 src/generate_dynamic_html.py 2>&1 | tee -a "$LOG_DIR/web_html_$DATE_STAMP.log"
        # 3. 推送报告
        cd /opt/hktech-agent/HKTech-Agent/prod && source /opt/hktech-agent/venv_rl/bin/activate && python3 src/report_pusher.py 2>&1 | tee -a "$LOG_DIR/noon_push_$DATE_STAMP.log"
        log "✅ 午间学习完成"
        ;;
    
    "afternoon_learning")
        log "🌙 开始执行：盘后学习"
        # 1. 运行主程序生成数据 (带环境变量)
        cd /opt/hktech-agent/HKTech-Agent/prod && source /opt/hktech-agent/venv_rl/bin/activate && timeout 180 python3 src/llm_enhanced_agent.py 2>&1 | tee -a "$LOG_DIR/afternoon_$DATE_STAMP.log"
        # 2. 更新 Portfolio (新增)
        cd /opt/hktech-agent/prod && source /opt/hktech-agent/venv_rl/bin/activate && python3 src/update_portfolio.py 2>&1 | tee -a "$LOG_DIR/portfolio_$DATE_STAMP.log"
        # 3. 生成 Web 面板数据和动态 HTML
        cd /opt/hktech-agent && python3 src/generate_web_data.py 2>&1 | tee -a "$LOG_DIR/web_data_$DATE_STAMP.log"
        cd /opt/hktech-agent && python3 src/generate_dynamic_html.py 2>&1 | tee -a "$LOG_DIR/web_html_$DATE_STAMP.log"
        # 3. 推送报告 (内部群)
        cd /opt/hktech-agent/HKTech-Agent/prod && source /opt/hktech-agent/venv_rl/bin/activate && python3 src/report_pusher.py 2>&1 | tee -a "$LOG_DIR/afternoon_push_$DATE_STAMP.log"
        # 4. 推送投资人报告 (外部群)
        cd /opt/hktech-agent/prod && source /opt/hktech-agent/venv_rl/bin/activate && python3 src/investor_report.py 2>&1 | tee -a "$LOG_DIR/investor_push_$DATE_STAMP.log"
        log "✅ 盘后学习完成"
        ;;
    
    "daily_briefing")
        log "📬 开始执行：AI 技术学习简报"
        # 使用网页版简报脚本
        cd /root/.openclaw/workspace && python3 reports/send_briefing_webpage.py 2>&1 | tee -a "$LOG_DIR/briefing_$DATE_STAMP.log"
        log "✅ AI 简报完成"
        ;;
    
    *)
        echo "用法：$0 {morning_briefing|noon_learning|afternoon_learning|daily_briefing}"
        exit 1
        ;;
esac
