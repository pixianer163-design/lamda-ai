#!/bin/bash
# 恒生 Agent 定时任务调度器
# 使用 Linux 系统级 cron，更可靠

SCRIPT_DIR="/root/.openclaw/workspace/Lamda-ai/HKTech-Agent/scripts"
LOG_DIR="/root/.openclaw/workspace/logs/cron"

# 创建日志目录
mkdir -p "$LOG_DIR"

# 获取当前时间戳
TIMESTAMP=$(date +"%Y-%m-%d %H:%M:%S")
DATE_STAMP=$(date +"%Y-%m-%d")

log() {
    echo "[$TIMESTAMP] $1" | tee -a "$LOG_DIR/cron_$DATE_STAMP.log"
}

case "$1" in
    "morning_briefing")
        log "🌅 开始执行：盘前学习"
        # 使用 openclaw sessions_spawn 执行任务
        openclaw sessions spawn --label "盘前学习 - $DATE_STAMP" --model "bailian/qwen3.5-plus" --timeout 300 \
            "执行盘前学习任务：
            1. 获取 overnight 新闻和全球市场动态
            2. 分析美股/欧股/日股收盘对港股的影响
            3. 阅读当天重要财经新闻（用 LLM 提取情绪信号）
            4. 更新市场日历，确认今日无重大事件
            5. 生成开盘策略建议
            6. 推送到飞书双群
            
            输出格式：
            🌅 盘前学习报告 - Day X
            📰 全球 markets overnight
            📊 港股开盘策略
            ⚠️ 风险提示"
        log "✅ 盘前学习完成"
        ;;
    
    "noon_learning")
        log "🌞 开始执行：午间学习"
        openclaw sessions spawn --label "午间学习 - $DATE_STAMP" --model "bailian/qwen3.5-plus" --timeout 300 \
            "执行午间学习任务：
            1. 获取上午港股收盘数据（腾讯/阿里/美团/小米/快手/京东）
            2. 分析上午行情特征
            3. 对比开盘预测 vs 实际走势
            4. 更新持仓盈亏
            5. 调整下午策略
            6. 推送到飞书双群"
        log "✅ 午间学习完成"
        ;;
    
    "afternoon_learning")
        log "🌙 开始执行：盘后学习"
        openclaw sessions spawn --label "盘后学习 - $DATE_STAMP" --model "bailian/qwen3.5-plus" --timeout 300 \
            "执行盘后学习任务：
            1. 获取全天港股收盘数据
            2. 执行回测验证
            3. 更新世界模型
            4. 记录交易日志
            5. 生成全天学习报告
            6. 推送到飞书双群"
        log "✅ 盘后学习完成"
        ;;
    
    "daily_briefing")
        log "📬 开始执行：AI 技术学习简报"
        openclaw sessions spawn --label "AI 技术学习简报 - $DATE_STAMP" --model "bailian/qwen3.5-plus" --timeout 300 \
            "生成并推送 AI 技术学习简报（网页版）：
            1. 获取今日日期和星期，确定本周主题
            2. 搜集 arXiv 最新论文
            3. 生成深度简报
            4. 创建 HTML 网页版
            5. 推送到飞书双群（发送网页链接）"
        log "✅ AI 简报完成"
        ;;
    
    *)
        echo "用法：$0 {morning_briefing|noon_learning|afternoon_learning|daily_briefing}"
        exit 1
        ;;
esac
