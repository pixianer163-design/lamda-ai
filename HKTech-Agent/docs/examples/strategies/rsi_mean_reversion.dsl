/*
 * RSI 超买超卖策略
 * RSI Mean Reversion Strategy
 * 原理：RSI<30 超卖买入，RSI>70 超买卖出
 */

strategy RSIMeanReversion
    description: "RSI 超买超卖均值回归策略"
{
    // 策略参数
    param rsi_period: period = 14
    param rsi_oversold: int = 30      // 超卖阈值
    param rsi_overbought: int = 70   // 超买阈值
    
    // 技术指标
    indicator rsi: RSI(period=rsi_period)
    
    // 入场条件：RSI 进入超卖区域
    when "RSI Oversold Entry" {
        trigger: rsi < rsi_oversold
        action: BUY
        size: 0.5
        confidence: 0.8
        priority: 1
    }
    
    // 出场条件：RSI 进入超买区域
    when "RSI Overbought Exit" {
        trigger: rsi > rsi_overbought
        action: SELL
        size: 0.5
        confidence: 0.8
        priority: 1
    }
    
    // 仓位管理
    position {
        max_position = 0.6           // 最大仓位 60%
        risk_per_trade = 2%           // 单笔风险 2%
        trailing_stop = 5%            // 追踪止损 5%
    }
    
    // 退出规则：RSI 回归中性
    exit "RSI Neutral" {
        rsi > 50
        type = SIGNAL
    }
}
