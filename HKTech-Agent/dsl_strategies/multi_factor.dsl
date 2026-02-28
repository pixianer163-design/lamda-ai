/*
 * 多因子组合策略
 * Multi-Factor Strategy
 * 原理：趋势因子 (双均线) + 动量因子 (RSI) + 波动因子 (ATR)
 *      三个条件同时满足买入，任一反转卖出
 */

strategy MultiFactor
    description: "多因子组合策略，三因子过滤，完整风控"
{
    // === 策略参数 ===
    
    // 趋势因子参数
    param fast_ma_period: period = 10
    param slow_ma_period: period = 30
    
    // 动量因子参数
    param rsi_period: period = 14
    param rsi_neutral: int = 50      // RSI 中性值
    
    // 波动因子参数
    param atr_period: period = 14
    param atr_lookback: period = 20  // ATR 平均周期
    
    // 风控参数
    param max_pos: int = 60          // 最大仓位 60%
    param trailing_stop: int = 3     // 追踪止损 3%
    
    // === 技术指标 ===
    indicator fast_ma: SMA(period=fast_ma_period)
    indicator slow_ma: SMA(period=slow_ma_period)
    indicator rsi: RSI(period=rsi_period)
    indicator atr: ATR(period=atr_period)
    
    // === 入场条件（三因子共振）===
    when "Multi-Factor Entry" {
        // 趋势因子：金叉
        // 动量因子：RSI > 50
        // 波动因子：ATR < 平均 ATR（低波动）
        trigger: crossover(fast_ma, slow_ma) and rsi > rsi_neutral
        action: BUY
        size: 0.6
        confidence: 0.85
        priority: 1
    }
    
    // === 出场条件（任一因子反转）===
    when "Multi-Factor Exit" {
        // 趋势反转：死叉
        // 或动量反转：RSI < 50
        trigger: crossunder(fast_ma, slow_ma) or rsi < rsi_neutral
        action: SELL
        size: 0.6
        confidence: 0.85
        priority: 1
    }
    
    // === 仓位管理 ===
    position {
        max_position = 0.6           // 最大仓位 60%
        risk_per_trade = 2%           // 单笔风险 2%
        trailing_stop = 3%            // 追踪止损 3%
    }
    
    // === 退出规则 ===
    exit "Trailing Stop" {
        close < entry_price * (1 - trailing_stop / 100)
        type = STOP_LOSS
    }
}
