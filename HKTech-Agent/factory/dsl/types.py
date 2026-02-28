"""
DSL Type Definitions
DSL 类型定义模块
"""

from dataclasses import dataclass, field
from typing import Dict, Any, List, Optional
from enum import Enum
import pandas as pd


class ActionType(Enum):
    """交易动作类型"""
    BUY = "BUY"
    SELL = "SELL"
    HOLD = "HOLD"
    CLOSE = "CLOSE"


class ExitType(Enum):
    """退出类型"""
    STOP_LOSS = "STOP_LOSS"
    TAKE_PROFIT = "TAKE_PROFIT"
    TIMEOUT = "TIMEOUT"
    SIGNAL = "SIGNAL"


class ParamType(Enum):
    """参数类型"""
    INT = "int"
    FLOAT = "float"
    BOOL = "bool"
    STRING = "string"
    PERIOD = "period"


class IndicatorType(Enum):
    """指标类型"""
    SMA = "SMA"
    EMA = "EMA"
    RSI = "RSI"
    MACD = "MACD"
    BB = "BB"
    ATR = "ATR"
    KDJ = "KDJ"
    VWAP = "VWAP"
    ADX = "ADX"
    CCI = "CCI"


@dataclass
class Signal:
    """交易信号"""
    timestamp: pd.Timestamp
    action: str  # BUY, SELL, HOLD, CLOSE
    confidence: float
    position_size: float
    metadata: Dict[str, Any] = field(default_factory=dict)
    price: Optional[float] = None
    
    def __post_init__(self):
        """验证信号"""
        if not 0 <= self.confidence <= 1:
            raise ValueError(f"Confidence must be between 0 and 1, got {self.confidence}")
        if self.position_size < 0:
            raise ValueError(f"Position size must be non-negative, got {self.position_size}")


@dataclass
class StrategyConfig:
    """策略配置"""
    name: str
    description: Optional[str] = None
    parameters: Dict[str, Any] = field(default_factory=dict)
    max_position: float = 0.95
    risk_per_trade: float = 0.02
    trailing_stop: Optional[float] = None
    take_profit_ratio: Optional[float] = None


@dataclass
class IndicatorResult:
    """指标计算结果"""
    name: str
    type: str
    values: pd.Series
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class ConditionResult:
    """条件检查结果"""
    name: str
    triggered: bool
    action: Optional[ActionType] = None
    confidence: float = 0.0
    position_size: float = 0.0
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class FusionResult:
    """融合结果（元学习 + 规则）"""
    signal: float
    confidence: float
    weights: Dict[str, float]
    scenario: str
    reasoning: str


@dataclass
class BacktestResult:
    """回测结果"""
    total_return: float
    annualized_return: float
    sharpe_ratio: float
    max_drawdown: float
    win_rate: float
    total_trades: int
    winning_trades: int
    losing_trades: int
    avg_win: float
    avg_loss: float
    profit_factor: float
    equity_curve: pd.Series = field(default_factory=lambda: pd.Series())
    trades: List[Dict[str, Any]] = field(default_factory=list)
    
    def summary(self) -> str:
        """生成回测摘要"""
        return f"""
回测结果摘要
============
总收益率：{self.total_return:.2%}
年化收益：{self.annualized_return:.2%}
夏普比率：{self.sharpe_ratio:.2f}
最大回撤：{self.max_drawdown:.2%}
胜率：{self.win_rate:.2%}
总交易：{self.total_trades}
盈亏比：{self.profit_factor:.2f}
"""


@dataclass
class CompileError(Exception):
    """编译错误"""
    line: int
    column: int
    message: str
    severity: str = "error"
    source_line: int = 0
    source_column: int = 0
    code_snippet: str = ""
    suggestion: Optional[str] = None
    valid_options: List[str] = field(default_factory=list)
    
    def __str__(self) -> str:
        return f"[{self.severity.upper()}] Line {self.line}:{self.column} - {self.message}"
    
    def __post_init__(self):
        Exception.__init__(self, str(self))


@dataclass
class CompileResult:
    """编译结果"""
    success: bool
    code: Optional[str] = None
    errors: List[CompileError] = field(default_factory=list)
    warnings: List[CompileError] = field(default_factory=list)
    
    def has_errors(self) -> bool:
        return len(self.errors) > 0
    
    def has_warnings(self) -> bool:
        return len(self.warnings) > 0
