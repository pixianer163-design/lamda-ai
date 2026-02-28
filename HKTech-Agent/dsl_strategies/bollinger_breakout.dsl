/*
 * 布林带突破策略
 * Bollinger Band Breakout Strategy
 * 原理：价格突破布林带上轨买入，下轨卖出
 */

strategy BollingerBreakout
    description: "布林带突破策略，使用 ATR 动态止损"
{
    // 策略参数
    param bb_period: period = 20
    param bb_std: int = 2        // 布林带标准差倍数
    param atr_period: period = 14
    param risk_reward: int = 2   // 盈亏比 2:1
    
    // 技术指标
    indicator bb_upper: BB(period=bb_period, std=bb_std)
    indicator bb_middle: BB(period=bb_period, std=bb_std)
    indicator bb_lower: BB(period=bb_period, std=bb_std)
    indicator atr: ATR(period=atr_period)
    
    // 入场条件：价格突破上轨
    when "BB Upper Breakout" {
        trigger: close > bb_upper
        action: BUY
        size: 0.5
        confidence: 0.7
        priority: 1
    }
    
    // 出场条件：价格突破下轨
    when "BB Lower Breakout" {
        trigger: close < bb_lower
        action: SELL
        size: 0.5
        confidence: 0.7
        priority: 1
    }
    
    // 仓位管理
    position {
        max_position = 0.6           // 最大仓位 60%
        risk_per_trade = 2%           // 单笔风险 2%
        trailing_stop = 3%            // 追踪止损 3%（基于 ATR）
    }
    
    // 退出规则：止损
    exit "ATR Stop Loss" {
        close < entry_price * (1 - atr / close)
        type = STOP_LOSS
    }
}
