/*
 * MACD 趋势策略
 * MACD Trend Strategy
 * 原理：MACD 金叉且柱状图 > 0 买入，死叉且柱状图 < 0 卖出
 */

strategy MACDTrend
    description: "MACD 趋势策略，动态仓位，追踪止损"
{
    // 策略参数
    param macd_fast: period = 12
    param macd_slow: period = 26
    param macd_signal: period = 9
    
    // 技术指标
    indicator macd_line: MACD(fast=macd_fast, slow=macd_slow, signal=macd_signal)
    indicator signal_line: MACD(fast=macd_fast, slow=macd_slow, signal=macd_signal)
    indicator histogram: MACD(fast=macd_fast, slow=macd_slow, signal=macd_signal)
    
    // 入场条件：MACD 金叉 + 柱状图>0
    when "MACD Golden Cross" {
        trigger: crossover(macd_line, signal_line) and histogram > 0
        action: BUY
        size: 0.6
        confidence: 0.75
        priority: 1
    }
    
    // 出场条件：MACD 死叉 + 柱状图<0
    when "MACD Death Cross" {
        trigger: crossunder(macd_line, signal_line) and histogram < 0
        action: SELL
        size: 0.6
        confidence: 0.75
        priority: 1
    }
    
    // 仓位管理
    position {
        max_position = 0.7           // 最大仓位 70%
        risk_per_trade = 2%           // 单笔风险 2%
        trailing_stop = 3%            // 追踪止损 3%
    }
    
    // 退出规则：追踪止损
    exit "Trailing Stop" {
        close < entry_price * (1 - trailing_stop)
        type = STOP_LOSS
    }
}
