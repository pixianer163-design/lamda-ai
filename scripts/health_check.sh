#!/bin/bash
# 恒生 Agent 系统健康检查脚本
# 功能：自动检测系统状态，发现故障并尝试恢复

set -e

# 配置
LOG_DIR="/opt/hktech-agent/logs"
HEALTH_LOG="$LOG_DIR/health_check.log"
ALERT_CHAT_ID="oc_d5f6f6f591bc129e4ae9037b0acdd3a5"  # 牛马 Agent 消息群
WEB_PORT=8080
MAX_MEMORY_GB=7
MAX_DISK_PERCENT=90

# 颜色输出
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# 日志函数
log() {
    echo -e "[$(date '+%Y-%m-%d %H:%M:%S')] $1" | tee -a "$HEALTH_LOG"
}

log_success() {
    log "${GREEN}✅ $1${NC}"
}

log_warning() {
    log "${YELLOW}⚠️  $1${NC}"
}

log_error() {
    log "${RED}❌ $1${NC}"
}

# 发送飞书告警
send_alert() {
    local message="$1"
    log_warning "发送告警：$message"
    
    # 使用飞书 Webhook 发送告警
    WEBHOOK_URL="https://open.feishu.cn/open-apis/bot/v2/hook/7a7dbe38-9181-4311-8094-ebaf6cf0f378"
    
    curl -s -X POST "$WEBHOOK_URL" \
        -H "Content-Type: application/json" \
        -d "{
            \"msg_type\": \"text\",
            \"content\": {
                \"text\": \"🚨 恒生 Agent 健康告警\\n\\n$message\"
            }
        }" > /dev/null
    
    log_success "告警已发送"
}

# 检查 1: Web 服务
check_web_service() {
    log "检查 Web 服务 (端口 $WEB_PORT)..."
    
    if netstat -tlnp 2>/dev/null | grep -q ":$WEB_PORT"; then
        log_success "Web 服务运行正常"
        return 0
    else
        log_error "Web 服务未运行"
        
        # 尝试重启
        log "尝试重启 Web 服务..."
        cd /opt/hktech-agent && python3 start_web_server.py &
        sleep 3
        
        if netstat -tlnp 2>/dev/null | grep -q ":$WEB_PORT"; then
            log_success "Web 服务重启成功"
            send_alert "Web 服务已自动恢复"
            return 0
        else
            log_error "Web 服务重启失败"
            send_alert "❌ Web 服务重启失败，请手动检查"
            return 1
        fi
    fi
}

# 检查 2: 数据采集
check_data_collection() {
    log "检查数据采集..."
    
    # 主数据源：腾讯财经 (港股数据稳定)
    response=$(curl -s -w "%{http_code}" -o /dev/null "https://qt.gtimg.cn/q=hk00700" 2>/dev/null || echo "000")
    
    if [ "$response" = "200" ]; then
        log_success "腾讯数据源正常 (主)"
    else
        log_error "腾讯数据源异常 (HTTP $response)"
        send_alert "❌ 腾讯数据源异常：HTTP $response"
    fi
    
    # 备用数据源：网易财经 (阿里 403 时的备选)
    response=$(curl -s -w "%{http_code}" -o /dev/null "https://api.money.126.net/data/feed/00700,09988" 2>/dev/null || echo "000")
    
    if [ "$response" = "200" ]; then
        log_success "网易数据源正常 (备用)"
    else
        log_warning "网易数据源异常 (HTTP $response)"
        # 备用源失败不告警，仅记录
    fi
    
    # 阿里数据源 (已失效，降级为可选检查)
    # response=$(curl -s -w "%{http_code}" -o /dev/null "https://hq.sinajs.cn/list=hk00700" 2>/dev/null || echo "000")
    # log_info "阿里数据源：HTTP $response (备用，已降级)"
}

# 检查 3: 磁盘空间
check_disk_space() {
    log "检查磁盘空间..."
    
    disk_usage=$(df / | tail -1 | awk '{print $5}' | sed 's/%//')
    
    if [ "$disk_usage" -lt "$MAX_DISK_PERCENT" ]; then
        log_success "磁盘使用正常 (${disk_usage}%)"
    else
        log_error "磁盘空间不足 (${disk_usage}% > ${MAX_DISK_PERCENT}%)"
        send_alert "❌ 磁盘空间不足：${disk_usage}%"
    fi
}

# 检查 4: 内存使用
check_memory() {
    log "检查内存使用..."
    
    memory_used=$(free -g | awk '/^Mem:/ {print $3}')
    memory_total=$(free -g | awk '/^Mem:/ {print $2}')
    
    if [ "$memory_used" -lt "$MAX_MEMORY_GB" ]; then
        log_success "内存使用正常 (${memory_used}GB/${memory_total}GB)"
    else
        log_warning "内存使用较高 (${memory_used}GB/${memory_total}GB)"
        send_alert "⚠️ 内存使用较高：${memory_used}GB/${memory_total}GB"
    fi
}

# 检查 5: 进程状态
check_processes() {
    log "检查关键进程..."
    
    # 检查 Web 服务进程
    if pgrep -f "start_web_server.py" > /dev/null; then
        log_success "Web 服务进程正常"
    else
        log_error "Web 服务进程未运行"
        send_alert "❌ Web 服务进程未运行"
    fi
    
    # 检查 Webhook 进程
    if pgrep -f "webhook" > /dev/null; then
        log_success "Webhook 进程正常"
    else
        log_error "Webhook 进程未运行"
        send_alert "❌ Webhook 进程未运行"
    fi
}

# 检查 6: 日志文件
check_logs() {
    log "检查日志文件..."
    
    # 检查最新错误日志
    if [ -f "$LOG_DIR/web_server.log" ]; then
        recent_errors=$(tail -100 "$LOG_DIR/web_server.log" 2>/dev/null | grep -c "ERROR" || echo "0")
        recent_errors=$(echo "$recent_errors" | head -1 | tr -cd '0-9')  # 清理输出
        
        if [ -z "$recent_errors" ]; then
            recent_errors=0
        fi
        
        if [ "$recent_errors" -gt 10 ]; then
            log_warning "最近日志中有 $recent_errors 条错误"
            send_alert "⚠️ 最近日志中有 $recent_errors 条错误，请检查"
        else
            log_success "日志错误数正常 ($recent_errors 条)"
        fi
    else
        log_success "日志文件未生成 (正常)"
    fi
}

# 主函数
main() {
    log "=========================================="
    log "🔍 恒生 Agent 系统健康检查"
    log "=========================================="
    
    # 确保日志目录存在
    mkdir -p "$LOG_DIR"
    
    # 执行检查
    check_web_service
    check_data_collection
    check_disk_space
    check_memory
    check_processes
    check_logs
    
    log "=========================================="
    log "✅ 健康检查完成"
    log "=========================================="
}

# 执行
main "$@"
