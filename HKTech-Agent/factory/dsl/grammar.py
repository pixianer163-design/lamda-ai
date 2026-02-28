"""
DSL Grammar Definition for Trading Strategy
基于TextX的交易策略DSL语法定义
"""

try:
    from textx import metamodel_from_str
except ImportError:
    raise ImportError(
        "textx is required for DSL parsing. "
        "Install it with: pip install textx"
    )
from typing import List


# DSL Grammar Definition
GRAMMAR = r'''
StrategyModel:
    imports*=Import
    strategy=Strategy
;

Import:
    'import' import_uri=STRING
;

Strategy:
    'strategy' name=ID
    ('description' ':' description=STRING)?
    '{' 
        parameters*=Parameter
        indicators*=Indicator
        conditions*=Condition
        position_mgmt=PositionManagement
        exit_rules*=ExitRule
    '}'
;

Parameter:
    'param' name=ID ':' type=ParamType ('=' default_value=Value)?
;

ParamType:
    'int' | 'float' | 'bool' | 'string' | 'period'
;

Indicator:
    'indicator' name=ID ':' indicator_type=IndicatorType 
    ('(' params+=IndicatorParam (',' params+=IndicatorParam)* ')')?
;

IndicatorParam:
    name=ID '=' value=IndicatorParamValue
;

IndicatorParamValue:
    ID | INT | FLOAT | STRING | BOOL
;

IndicatorType:
    'SMA' | 'EMA' | 'RSI' | 'MACD' | 'BB' | 'ATR' | 'KDJ' | 'VWAP' | 'ADX' | 'CCI'
;

Condition:
    'when' name=ID? description=STRING?
    '{' 
        triggers+=Trigger
        action=Action
        (('priority' | 'priority:') ('=' | ':')? priority=INT)?
    '}'
;

Trigger:
    'trigger' ':' expr=TriggerExpr
;

TriggerExpr:
    BoolFuncCall BoolOp?
    | BoolExpr
;

BoolOp:
    ('and' | 'or') BoolExpr
;

Action:
    'action' ':' action_type=ActionType
    (('size' | 'size:') ('=' | ':')? position_size=SimpleExpr)?
    (('confidence' | 'confidence:') ('=' | ':')? confidence=SimpleExpr)?
;

SimpleExpr:
    Number | ID | FuncCall | '(' Expr ')'
;

ActionType:
    'BUY' | 'SELL' | 'HOLD' | 'CLOSE'
;

BoolExpr:
    CompareExpr | AndExpr | OrExpr | NotExpr
;

AndExpr:
    'and' '(' left=BoolExpr ',' right=BoolExpr ')'  
    | left=CompareExpr 'and' right=BoolExpr
;

OrExpr:
    'or' '(' left=BoolExpr ',' right=BoolExpr ')'
    | left=CompareExpr 'or' right=BoolExpr
;

NotExpr:
    'not' '(' expr=BoolExpr ')'
    | '!' expr=CompareExpr
;

CompareExpr:
    left=Expr op=CompOp right=Expr
;

CompOp:
    '>' | '<' | '>=' | '<=' | '==' | '!='
;

Expr:
    AddSubExpr
;

AddSubExpr:
    MulDivExpr (('+' | '-') MulDivExpr)*
;

MulDivExpr:
    PrimaryExpr (('*' | '/') PrimaryExpr)*
;

PrimaryExpr:
    Number | IndicatorRef | FuncCall | '(' Expr ')' | Variable | BoolFuncCall
;

BoolFuncCall:
    name=BuiltinFunc '(' (args+=Expr (',' args+=Expr)*) ')'
;

BuiltinFunc:
    'crossover' | 'crossunder' | 'above' | 'below' | 'highest' | 'lowest' | 'change_pct'
;

IndicatorRef:
    indicator=ID ('.' field=ID)?
;

FuncCall:
    name=ID '(' (args+=Expr (',' args+=Expr)*)? ')'
;

Variable:
    'close' | 'open' | 'high' | 'low' | 'volume' | ID
;

Number:
    FLOAT | INT
;

FLOAT:
    /-?\d+\.\d+/
;

INT:
    /-?\d+/
;

Value:
    STRING | INT | FLOAT | BOOL
;

BOOL:
    'true' | 'false'
;

PositionManagement:
    'position' '{' 
        max_position=MaxPosition
        risk_per_trade=RiskPerTrade
        (trailing_stop=TrailingStop)?
        (take_profit=TakeProfit)?
    '}'
;

MaxPosition:
    'max_position' '=' value=Expr ('%' | 'shares')?
;

RiskPerTrade:
    'risk_per_trade' '=' value=Expr '%'
;

TrailingStop:
    'trailing_stop' '=' value=Expr '%'
;

TakeProfit:
    'take_profit' ('=' value=Expr '%' | 'ratio' '=' ratio=Expr)
;

ExitRule:
    'exit' exit_name=ExitName '{' 
        condition=BoolExpr
        (('type' | 'type:') ('=' | ':')? exit_type=ExitType)?
    '}'
;

ExitName:
    name=ID | name=STRING
;

ExitType:
    'STOP_LOSS' | 'TAKE_PROFIT' | 'TIMEOUT' | 'SIGNAL'
;

Comment:
    /\/\/.*$/ | /\/\*[\s\S]*?\*\// | /#.*$/
;
'''


class DSLValidator:
    """DSL语义验证器"""
    
    VALID_INDICATORS = {
        'SMA': {'period': 20},
        'EMA': {'period': 20},
        'RSI': {'period': 14},
        'MACD': {'fast': 12, 'slow': 26, 'signal': 9},
        'BB': {'period': 20, 'std': 2},
        'ATR': {'period': 14},
        'KDJ': {'k_period': 9, 'd_period': 3},
        'VWAP': {},
        'ADX': {'period': 14},
        'CCI': {'period': 20}
    }
    
    def __init__(self):
        self.errors = []
        self.warnings = []
    
    def validate(self, model) -> List[str]:
        """验证DSL模型"""
        self.errors = []
        self.warnings = []
        
        # 验证策略名称
        if not model.strategy.name:
            self.errors.append("Strategy name is required")
        
        # 验证指标参数
        for indicator in model.strategy.indicators:
            self._validate_indicator(indicator)
        
        # 验证条件
        for condition in model.strategy.conditions:
            self._validate_condition(condition)
        
        # 验证仓位管理
        self._validate_position_mgmt(model.strategy.position_mgmt)
        
        return self.errors
    
    def _validate_indicator(self, indicator):
        """验证指标定义"""
        ind_type = indicator.indicator_type
        if ind_type not in self.VALID_INDICATORS:
            self.errors.append(f"Unknown indicator type: {ind_type}")
            return
        
        required_params = self.VALID_INDICATORS[ind_type]
        provided_params = {p.name: p.value for p in indicator.params}
        
        # 检查必需参数
        for param_name in required_params:
            if param_name not in provided_params:
                self.warnings.append(
                    f"Indicator {indicator.name}: using default {param_name}={required_params[param_name]}"
                )
    
    def _validate_condition(self, condition):
        """验证条件表达式"""
        if not condition.triggers:
            self.errors.append(f"Condition {condition.name}: at least one trigger required")
        
        for trigger in condition.triggers:
            if not trigger.expr:
                self.errors.append(f"Condition {condition.name}: trigger expression required")
    
    def _validate_position_mgmt(self, pos_mgmt):
        """验证仓位管理"""
        if not pos_mgmt.max_position:
            self.errors.append("Position management: max_position required")
        if not pos_mgmt.risk_per_trade:
            self.errors.append("Position management: risk_per_trade required")


def create_metamodel():
    """创建TextX元模型"""
    global metamodel_from_str
    
    builtins = {
        'crossover': lambda x, y: x[-2] < y[-2] and x[-1] >= y[-1],
        'crossunder': lambda x, y: x[-2] > y[-2] and x[-1] <= y[-1],
        'above': lambda x, y: x[-1] > y[-1],
        'below': lambda x, y: x[-1] < y[-1],
        'highest': lambda x, n: max(x[-n:]),
        'lowest': lambda x, n: min(x[-n:]),
        'change_pct': lambda x, n: (x[-1] - x[-n-1]) / x[-n-1] * 100,
    }
    
    mm = metamodel_from_str(GRAMMAR, builtins=builtins)
    return mm


# 预编译元模型
_meta_model = None

def get_metamodel():
    """获取或创建元模型（单例模式）"""
    global _meta_model
    if _meta_model is None:
        _meta_model = create_metamodel()
    return _meta_model
