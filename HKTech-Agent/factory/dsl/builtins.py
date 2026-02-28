"""
DSL Built-in Functions
DSL 内置函数库 - 技术指标和工具函数
"""

import numpy as np
import pandas as pd
from typing import Union


# ============================================================================
# 价格形态函数
# ============================================================================

def crossover(series_a: pd.Series, series_b: pd.Series) -> bool:
    """
    金叉检测：A 从下方穿越 B
    
    Args:
        series_a: 系列 A（如快线）
        series_b: 系列 B（如慢线）
    
    Returns:
        bool: 是否发生金叉
    """
    if len(series_a) < 2 or len(series_b) < 2:
        return False
    return (series_a.iloc[-2] < series_b.iloc[-2]) and (series_a.iloc[-1] >= series_b.iloc[-1])


def crossunder(series_a: pd.Series, series_b: pd.Series) -> bool:
    """
    死叉检测：A 从上方穿越 B
    
    Args:
        series_a: 系列 A
        series_b: 系列 B
    
    Returns:
        bool: 是否发生死叉
    """
    if len(series_a) < 2 or len(series_b) < 2:
        return False
    return (series_a.iloc[-2] > series_b.iloc[-2]) and (series_a.iloc[-1] <= series_b.iloc[-1])


def above(series: pd.Series, value: Union[float, pd.Series]) -> bool:
    """
    检测系列是否在值上方
    
    Args:
        series: 价格/指标系列
        value: 比较值或系列
    
    Returns:
        bool: 是否在上方
    """
    if isinstance(value, pd.Series):
        return series.iloc[-1] > value.iloc[-1]
    return series.iloc[-1] > value


def below(series: pd.Series, value: Union[float, pd.Series]) -> bool:
    """
    检测系列是否在值下方
    
    Args:
        series: 价格/指标系列
        value: 比较值或系列
    
    Returns:
        bool: 是否在下方
    """
    if isinstance(value, pd.Series):
        return series.iloc[-1] < value.iloc[-1]
    return series.iloc[-1] < value


def highest(series: pd.Series, period: int) -> float:
    """
    获取最近 N 周期的最高值
    
    Args:
        series: 价格系列
        period: 周期数
    
    Returns:
        float: 最高值
    """
    return series.iloc[-period:].max()


def lowest(series: pd.Series, period: int) -> float:
    """
    获取最近 N 周期的最低值
    
    Args:
        series: 价格系列
        period: 周期数
    
    Returns:
        float: 最低值
    """
    return series.iloc[-period:].min()


def change_pct(series: pd.Series, period: int) -> float:
    """
    计算 N 周期变化率
    
    Args:
        series: 价格系列
        period: 周期数
    
    Returns:
        float: 变化率（百分比）
    """
    if len(series) <= period:
        return 0.0
    return ((series.iloc[-1] - series.iloc[-period-1]) / series.iloc[-period-1]) * 100


def change(series: pd.Series, period: int = 1) -> float:
    """
    计算 N 周期变化值
    
    Args:
        series: 价格系列
        period: 周期数（默认 1）
    
    Returns:
        float: 变化值
    """
    if len(series) <= period:
        return 0.0
    return series.iloc[-1] - series.iloc[-period-1]


# ============================================================================
# K 线形态检测
# ============================================================================

def is_bullish_engulfing(open_prices: pd.Series, close_prices: pd.Series) -> bool:
    """
    检测看涨吞没形态
    
    Args:
        open_prices: 开盘价系列
        close_prices: 收盘价系列
    
    Returns:
        bool: 是否看涨吞没
    """
    if len(open_prices) < 2 or len(close_prices) < 2:
        return False
    
    # 前一天是阴线
    prev_bearish = close_prices.iloc[-2] < open_prices.iloc[-2]
    # 今天是阳线
    curr_bullish = close_prices.iloc[-1] > open_prices.iloc[-1]
    # 今天实体吞没前一天实体
    engulfing = (open_prices.iloc[-1] < close_prices.iloc[-2]) and \
                (close_prices.iloc[-1] > open_prices.iloc[-2])
    
    return prev_bearish and curr_bullish and engulfing


def is_bearish_engulfing(open_prices: pd.Series, close_prices: pd.Series) -> bool:
    """
    检测看跌吞没形态
    
    Args:
        open_prices: 开盘价系列
        close_prices: 收盘价系列
    
    Returns:
        bool: 是否看跌吞没
    """
    if len(open_prices) < 2 or len(close_prices) < 2:
        return False
    
    # 前一天是阳线
    prev_bullish = close_prices.iloc[-2] > open_prices.iloc[-2]
    # 今天是阴线
    curr_bearish = close_prices.iloc[-1] < open_prices.iloc[-1]
    # 今天实体吞没前一天实体
    engulfing = (open_prices.iloc[-1] > close_prices.iloc[-2]) and \
                (close_prices.iloc[-1] < open_prices.iloc[-2])
    
    return prev_bullish and curr_bearish and engulfing


def is_hammer(open_prices: pd.Series, high_prices: pd.Series, 
              low_prices: pd.Series, close_prices: pd.Series) -> bool:
    """
    检测锤子线形态
    
    Args:
        open_prices: 开盘价
        high_prices: 最高价
        low_prices: 最低价
        close_prices: 收盘价
    
    Returns:
        bool: 是否锤子线
    """
    if len(open_prices) < 1:
        return False
    
    body = abs(close_prices.iloc[-1] - open_prices.iloc[-1])
    upper_shadow = high_prices.iloc[-1] - max(open_prices.iloc[-1], close_prices.iloc[-1])
    lower_shadow = min(open_prices.iloc[-1], close_prices.iloc[-1]) - low_prices.iloc[-1]
    
    # 下影线至少是实体的 2 倍
    # 上影线很短
    return (lower_shadow >= 2 * body) and (upper_shadow <= body * 0.5)


def is_doji(open_prices: pd.Series, close_prices: pd.Series, 
            high_prices: pd.Series, low_prices: pd.Series) -> bool:
    """
    检测十字星形态
    
    Args:
        open_prices: 开盘价
        close_prices: 收盘价
        high_prices: 最高价
        low_prices: 最低价
    
    Returns:
        bool: 是否十字星
    """
    if len(open_prices) < 1:
        return False
    
    body = abs(close_prices.iloc[-1] - open_prices.iloc[-1])
    total_range = high_prices.iloc[-1] - low_prices.iloc[-1]
    
    # 实体非常小（小于总范围的 10%）
    return body <= total_range * 0.1


# ============================================================================
# 波动率函数
# ============================================================================

def historical_volatility(series: pd.Series, period: int = 20) -> float:
    """
    计算历史波动率
    
    Args:
        series: 价格系列
        period: 计算周期
    
    Returns:
        float: 年化波动率
    """
    returns = series.pct_change()
    return returns.iloc[-period:].std() * np.sqrt(252)


def average_true_range(high: pd.Series, low: pd.Series, 
                       close: pd.Series, period: int = 14) -> float:
    """
    计算平均真实波幅 (ATR)
    
    Args:
        high: 最高价系列
        low: 最低价系列
        close: 收盘价系列
        period: 周期
    
    Returns:
        float: ATR 值
    """
    high_low = high - low
    high_close = np.abs(high - close.shift())
    low_close = np.abs(low - close.shift())
    ranges = pd.concat([high_low, high_close, low_close], axis=1)
    true_range = ranges.max(axis=1)
    return true_range.rolling(period).mean().iloc[-1]


# ============================================================================
# 成交量函数
# ============================================================================

def volume_spike(volume: pd.Series, threshold: float = 2.0) -> bool:
    """
    检测成交量异常放大
    
    Args:
        volume: 成交量系列
        threshold: 阈值（相对于平均成交量的倍数）
    
    Returns:
        bool: 是否成交量异常
    """
    avg_volume = volume.iloc[-20:-1].mean()  # 过去 20 天平均（不包括今天）
    return volume.iloc[-1] > avg_volume * threshold


def on_balance_volume(close: pd.Series, volume: pd.Series) -> pd.Series:
    """
    计算能量潮 (OBV)
    
    Args:
        close: 收盘价系列
        volume: 成交量系列
    
    Returns:
        pd.Series: OBV 系列
    """
    direction = np.sign(close.diff())
    obv = (volume * direction).cumsum()
    return obv


# ============================================================================
# 统计函数
# ============================================================================

def zscore(series: pd.Series, period: int = 20) -> float:
    """
    计算 Z-Score（标准化值）
    
    Args:
        series: 数据系列
        period: 计算周期
    
    Returns:
        float: Z-Score 值
    """
    mean = series.iloc[-period:].mean()
    std = series.iloc[-period:].std()
    if std == 0:
        return 0.0
    return (series.iloc[-1] - mean) / std


def percentile_rank(series: pd.Series, period: int = 100) -> float:
    """
    计算百分位排名
    
    Args:
        series: 数据系列
        period: 计算周期
    
    Returns:
        float: 百分位（0-100）
    """
    window = series.iloc[-period:]
    rank = (window < series.iloc[-1]).sum()
    return (rank / len(window)) * 100


def correlation(series_a: pd.Series, series_b: pd.Series, period: int = 20) -> float:
    """
    计算两个系列的相关系数
    
    Args:
        series_a: 系列 A
        series_b: 系列 B
        period: 计算周期
    
    Returns:
        float: 相关系数 (-1 到 1)
    """
    return series_a.iloc[-period:].corr(series_b.iloc[-period:])


# ============================================================================
# 风险计算函数
# ============================================================================

def value_at_return(returns: pd.Series, confidence: float = 0.95) -> float:
    """
    计算在险价值 (VaR)
    
    Args:
        returns: 收益率系列
        confidence: 置信水平
    
    Returns:
        float: VaR 值
    """
    return returns.quantile(1 - confidence)


def conditional_var(returns: pd.Series, confidence: float = 0.95) -> float:
    """
    计算条件在险价值 (CVaR/Expected Shortfall)
    
    Args:
        returns: 收益率系列
        confidence: 置信水平
    
    Returns:
        float: CVaR 值
    """
    var = value_at_return(returns, confidence)
    return returns[returns <= var].mean()


def max_drawdown(equity_curve: pd.Series) -> float:
    """
    计算最大回撤
    
    Args:
        equity_curve: 资金曲线
    
    Returns:
        float: 最大回撤（百分比）
    """
    peak = equity_curve.expanding().max()
    drawdown = (equity_curve - peak) / peak
    return drawdown.min()


# ============================================================================
# 工具函数
# ============================================================================

def sma(series: pd.Series, period: int) -> pd.Series:
    """简单移动平均"""
    return series.rolling(window=period).mean()


def ema(series: pd.Series, period: int) -> pd.Series:
    """指数移动平均"""
    return series.ewm(span=period, adjust=False).mean()


def rsi(close: pd.Series, period: int = 14) -> pd.Series:
    """相对强弱指标"""
    delta = close.diff()
    gain = (delta.where(delta > 0, 0)).rolling(window=period).mean()
    loss = (-delta.where(delta < 0, 0)).rolling(window=period).mean()
    rs = gain / loss
    return 100 - (100 / (1 + rs))


def macd(close: pd.Series, fast: int = 12, slow: int = 26, 
         signal: int = 9) -> dict:
    """
    MACD 指标
    
    Returns:
        dict: {'macd': Series, 'signal': Series, 'histogram': Series}
    """
    ema_fast = close.ewm(span=fast, adjust=False).mean()
    ema_slow = close.ewm(span=slow, adjust=False).mean()
    macd_line = ema_fast - ema_slow
    signal_line = macd_line.ewm(span=signal, adjust=False).mean()
    histogram = macd_line - signal_line
    return {
        'macd': macd_line,
        'signal': signal_line,
        'histogram': histogram
    }


def bollinger_bands(close: pd.Series, period: int = 20, 
                    std_dev: float = 2.0) -> dict:
    """
    布林带
    
    Returns:
        dict: {'upper': Series, 'middle': Series, 'lower': Series}
    """
    middle = close.rolling(window=period).mean()
    std = close.rolling(window=period).std()
    return {
        'upper': middle + (std * std_dev),
        'middle': middle,
        'lower': middle - (std * std_dev)
    }
