"""
DSL Compiler - Transforms DSL to Python Code
DSL编译器 - 将DSL转换为Python代码
"""

import ast
import re
from typing import Dict, Any, List, Optional
from datetime import datetime

try:
    from textx.exceptions import TextXSyntaxError, TextXSemanticError
except ImportError:
    TextXSyntaxError = Exception
    TextXSemanticError = Exception

from .grammar import get_metamodel, DSLValidator
from .types import CompileError
from .error_reporter import ErrorReporter
from .cache import CompilationCache


class IndicatorCompiler:
    """技术指标编译器"""
    
    INDICATOR_IMPL = {
        'SMA': '''
    def {name}(self, data):
        period = {period}
        return data['close'].rolling(window=period).mean()
''',
        'EMA': '''
    def {name}(self, data):
        period = {period}
        return data['close'].ewm(span=period, adjust=False).mean()
''',
        'RSI': '''
    def {name}(self, data):
        period = {period}
        delta = data['close'].diff()
        gain = (delta.where(delta > 0, 0)).rolling(window=period).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=period).mean()
        rs = gain / loss
        return 100 - (100 / (1 + rs))
''',
        'MACD': '''
    def {name}(self, data):
        fast = {fast}
        slow = {slow}
        signal_period = {signal}
        ema_fast = data['close'].ewm(span=fast, adjust=False).mean()
        ema_slow = data['close'].ewm(span=slow, adjust=False).mean()
        macd_line = ema_fast - ema_slow
        signal_line = macd_line.ewm(span=signal_period, adjust=False).mean()
        histogram = macd_line - signal_line
        return {{
            'macd': macd_line,
            'signal': signal_line,
            'histogram': histogram
        }}
''',
        'BB': '''
    def {name}(self, data):
        period = {period}
        std_dev = {std}
        sma = data['close'].rolling(window=period).mean()
        std = data['close'].rolling(window=period).std()
        return {{
            'upper': sma + (std * std_dev),
            'middle': sma,
            'lower': sma - (std * std_dev),
            'bandwidth': (std * std_dev * 2) / sma,
            'percent_b': (data['close'] - (sma - std * std_dev)) / (std * std_dev * 2)
        }}
''',
        'ATR': '''
    def {name}(self, data):
        period = {period}
        high_low = data['high'] - data['low']
        high_close = np.abs(data['high'] - data['close'].shift())
        low_close = np.abs(data['low'] - data['close'].shift())
        ranges = pd.concat([high_low, high_close, low_close], axis=1)
        true_range = np.max(ranges, axis=1)
        return true_range.rolling(period).mean()
''',
        'KDJ': '''
    def {name}(self, data):
        k_period = {k_period}
        d_period = {d_period}
        low_min = data['low'].rolling(window=k_period).min()
        high_max = data['high'].rolling(window=k_period).max()
        rsv = 100 * (data['close'] - low_min) / (high_max - low_min)
        k = rsv.ewm(com=d_period-1, adjust=False).mean()
        d = k.ewm(com=d_period-1, adjust=False).mean()
        j = 3 * k - 2 * d
        return {{'k': k, 'd': d, 'j': j}}
''',
        'ADX': '''
    def {name}(self, data):
        period = {period}
        plus_dm = data['high'].diff()
        minus_dm = -data['low'].diff()
        plus_dm[plus_dm < 0] = 0
        minus_dm[minus_dm < 0] = 0
        tr = pd.concat([
            data['high'] - data['low'],
            np.abs(data['high'] - data['close'].shift()),
            np.abs(data['low'] - data['close'].shift())
        ], axis=1).max(axis=1)
        atr = tr.rolling(period).mean()
        plus_di = 100 * (plus_dm.rolling(period).mean() / atr)
        minus_di = 100 * (minus_dm.rolling(period).mean() / atr)
        dx = 100 * np.abs(plus_di - minus_di) / (plus_di + minus_di)
        adx = dx.rolling(period).mean()
        return adx
''',
        'CCI': '''
    def {name}(self, data):
        period = {period}
        tp = (data['high'] + data['low'] + data['close']) / 3
        sma_tp = tp.rolling(period).mean()
        mean_dev = tp.rolling(period).apply(lambda x: np.abs(x - x.mean()).mean())
        return (tp - sma_tp) / (0.015 * mean_dev)
''',
        'VWAP': '''
    def {name}(self, data):
        typical_price = (data['high'] + data['low'] + data['close']) / 3
        vwap = (typical_price * data['volume']).cumsum() / data['volume'].cumsum()
        return vwap
'''
    }
    
    def compile(self, indicator, strategy_params=None) -> str:
        """编译单个指标"""
        ind_type = indicator.indicator_type
        name = indicator.name
        
        if ind_type not in self.INDICATOR_IMPL:
            raise ValueError(f"Unknown indicator type: {ind_type}")
        
        # 提取参数
        params = {}
        for p in indicator.params:
            value = p.value
            # 如果参数值是字符串（参数引用），转换为 self.params['xxx']
            if isinstance(value, str):
                if strategy_params and value in strategy_params:
                    params[p.name] = f"self.params['{value}']"
                else:
                    params[p.name] = value
            else:
                params[p.name] = value
        
        # 获取默认参数
        from .grammar import DSLValidator
        defaults = DSLValidator.VALID_INDICATORS.get(ind_type, {})
        
        # 合并参数
        all_params = {**defaults, **params, 'name': name}
        
        template = self.INDICATOR_IMPL[ind_type]
        return template.format(**all_params)


class ExpressionCompiler:
    """表达式编译器"""
    
    def __init__(self, strategy_params=None, indicator_names=None):
        self.strategy_params = strategy_params or []
        self.indicator_names = indicator_names or []
    
    def compile(self, expr, context: Dict[str, Any] = None) -> str:
        """编译表达式为Python代码"""
        if expr is None:
            return "None"
        
        expr_type = expr.__class__.__name__
        
        if expr_type == 'CompareExpr':
            left = self.compile(expr.left, context)
            right = self.compile(expr.right, context)
            op = expr.op
            return f"({left} {op} {right})"
        
        elif expr_type == 'AndExpr':
            left = self.compile(expr.left, context)
            right = self.compile(expr.right, context)
            return f"({left} and {right})"
        
        elif expr_type == 'OrExpr':
            left = self.compile(expr.left, context)
            right = self.compile(expr.right, context)
            return f"({left} or {right})"
        
        elif expr_type == 'NotExpr':
            inner = self.compile(expr.expr, context)
            return f"(not {inner})"
        
        elif expr_type == 'AddSubExpr':
            # 处理加减表达式
            result = self.compile(expr.exprs[0] if hasattr(expr, 'exprs') else expr, context)
            return result
        
        elif expr_type == 'MulDivExpr':
            result = self.compile(expr.exprs[0] if hasattr(expr, 'exprs') else expr, context)
            return result
        
        elif expr_type == 'PrimaryExpr':
            if hasattr(expr, 'indicator'):
                return self._compile_indicator_ref(expr, indicator_names=self.indicator_names)
            elif hasattr(expr, 'value'):
                return str(expr.value)
            else:
                return str(expr)
        
        elif expr_type == 'IndicatorRef':
            return self._compile_indicator_ref(expr, indicator_names=self.indicator_names)
        
        elif expr_type == 'FuncCall':
            args = [self.compile(arg, context) for arg in expr.args]
            return f"self.{expr.name}({', '.join(args)})"
        
        elif expr_type == 'BoolFuncCall':
            # 处理内置布尔函数：crossover, crossunder, above, below 等
            # 对于 BoolFuncCall，参数需要是 Series 而不是单个值
            args = []
            for arg in expr.args:
                if arg.__class__.__name__ == 'IndicatorRef':
                    args.append(self._compile_indicator_ref(arg, as_series=True, indicator_names=self.indicator_names))
                else:
                    args.append(self.compile(arg, context))
            func_name = expr.name
            if func_name == 'crossover':
                return f"({args[0]}.iloc[-2] < {args[1]}.iloc[-2] and {args[0]}.iloc[-1] >= {args[1]}.iloc[-1])"
            elif func_name == 'crossunder':
                return f"({args[0]}.iloc[-2] > {args[1]}.iloc[-2] and {args[0]}.iloc[-1] <= {args[1]}.iloc[-1])"
            elif func_name == 'above':
                return f"({args[0]}.iloc[-1] > {args[1]}.iloc[-1])"
            elif func_name == 'below':
                return f"({args[0]}.iloc[-1] < {args[1]}.iloc[-1])"
            elif func_name == 'highest':
                return f"{args[0]}.iloc[-{args[1]}:].max()"
            elif func_name == 'lowest':
                return f"{args[0]}.iloc[-{args[1]}:].min()"
            elif func_name == 'change_pct':
                return f"(({args[0]}.iloc[-1] - {args[0]}.iloc[-{args[1]}-1]) / {args[0]}.iloc[-{args[1]}-1] * 100)"
            else:
                return f"self.{func_name}({', '.join(args)})"
        
        elif expr_type == 'TriggerExpr':
            # 处理 TriggerExpr: BoolFuncCall BoolOp?
            func_code = self.compile(expr.expr if hasattr(expr, 'expr') else expr, context)
            if hasattr(expr, 'bool_op') and expr.bool_op:
                bool_expr_code = self.compile(expr.bool_op.expr if hasattr(expr.bool_op, 'expr') else expr.bool_op, context)
                op = 'and' if expr.bool_op.op == 'and' else 'or'
                return f"({func_code} {op} {bool_expr_code})"
            return func_code
        
        elif expr_type == 'BoolOp':
            # 处理 BoolOp: ('and' | 'or') BoolExpr
            op = expr.op
            expr_code = self.compile(expr.expr, context)
            return f"{op} {expr_code}"
        
        elif expr_type == 'Variable':
            var_name = str(expr)
            if var_name in ['close', 'open', 'high', 'low', 'volume']:
                return f"data['{var_name}'].iloc[-1]"
            # 检查是否是策略参数
            if self.strategy_params and var_name in self.strategy_params:
                return f"self.params['{var_name}']"
            return var_name
        
        elif expr_type in ['INT', 'FLOAT']:
            return str(expr)
        
        else:
            # 尝试直接转换
            return str(expr)
    
    def _compile_indicator_ref(self, expr, as_series=False, indicator_names=None) -> str:
        """编译指标引用"""
        # indicator 现在是字符串而不是对象
        indicator_name = expr.indicator if isinstance(expr.indicator, str) else expr.indicator.name
        
        # 检查是否是参数引用（名称不在指标列表中）
        if indicator_names and indicator_name not in indicator_names:
            # 这是参数引用，不是指标引用
            if as_series:
                # 参数是标量，返回 Series 形式（重复值）
                return f"pd.Series([self.params['{indicator_name}']] * len(data))"
            return f"self.params['{indicator_name}']"
        
        if hasattr(expr, 'field') and expr.field:
            if as_series:
                return f"self.indicators['{indicator_name}']['{expr.field}']"
            return f"self.indicators['{indicator_name}']['{expr.field}'].iloc[-1]"
        if as_series:
            return f"self.indicators['{indicator_name}']"
        return f"self.indicators['{indicator_name}'].iloc[-1]"


class StrategyCompiler:
    """策略编译器"""
    
    VALID_INDICATORS = ['SMA', 'EMA', 'RSI', 'MACD', 'BB', 'ATR', 'KDJ', 'VWAP', 'ADX', 'CCI']
    VALID_BUILTINS = ['crossover', 'crossunder', 'above', 'below', 'highest', 'lowest', 'change_pct']
    VALID_ACTIONS = ['BUY', 'SELL', 'HOLD', 'CLOSE']
    
    def __init__(self, strategy_params=None, indicator_names=None):
        self.indicator_compiler = IndicatorCompiler()
        self.expr_compiler = ExpressionCompiler(strategy_params, indicator_names)
    
    def validate(self, dsl_model) -> List[CompileError]:
        """语义验证DSL模型"""
        errors = []
        strategy = dsl_model.strategy
        
        defined_indicators = {ind.name for ind in strategy.indicators}
        
        for ind in strategy.indicators:
            if ind.indicator_type not in self.VALID_INDICATORS:
                reporter = ErrorReporter("")
                suggestion = reporter.suggest_fix('indicator', ind.indicator_type, self.VALID_INDICATORS)
                errors.append(CompileError(
                    line=1,
                    column=1,
                    message=f"Unknown indicator type '{ind.indicator_type}'",
                    severity="error",
                    source_line=1,
                    source_column=1,
                    suggestion=suggestion if suggestion else "Valid indicators: " + ", ".join(self.VALID_INDICATORS),
                    valid_options=self.VALID_INDICATORS
                ))
        
        for condition in strategy.conditions:
            for trigger in condition.triggers:
                expr_errors = self._validate_expression(trigger.expr, defined_indicators)
                errors.extend(expr_errors)
        
        return errors
    
    def _validate_expression(self, expr, defined_indicators: set) -> List[CompileError]:
        """验证表达式中的指标引用"""
        errors = []
        
        if expr is None:
            return errors
        
        expr_type = expr.__class__.__name__
        
        if expr_type == 'BoolFuncCall':
            if expr.name not in self.VALID_BUILTINS:
                reporter = ErrorReporter("")
                suggestion = reporter.suggest_fix('function', expr.name, self.VALID_BUILTINS)
                errors.append(CompileError(
                    line=1,
                    column=1,
                    message=f"Unknown function '{expr.name}'",
                    severity="error",
                    suggestion=suggestion if suggestion else "Valid functions: " + ", ".join(self.VALID_BUILTINS),
                    valid_options=self.VALID_BUILTINS
                ))
        
        if expr_type == 'IndicatorRef':
            indicator_name = expr.indicator if isinstance(expr.indicator, str) else expr.indicator.name
            if indicator_name not in defined_indicators:
                errors.append(CompileError(
                    line=1,
                    column=1,
                    message=f"Undefined indicator '{indicator_name}'",
                    severity="error"
                ))
        
        for attr in ['left', 'right', 'expr']:
            if hasattr(expr, attr):
                child = getattr(expr, attr)
                if child:
                    errors.extend(self._validate_expression(child, defined_indicators))
        
        for attr in ['exprs', 'args']:
            if hasattr(expr, attr):
                for child in getattr(expr, attr):
                    if child:
                        errors.extend(self._validate_expression(child, defined_indicators))
        
        return errors
    
    def compile(self, dsl_model) -> str:
        """编译DSL模型为Python策略类"""
        strategy = dsl_model.strategy
        
        # 生成类名
        class_name = self._to_camel_case(strategy.name) + "Strategy"
        
        # 提取策略参数名称列表
        strategy_param_names = [p.name for p in strategy.parameters]

        # 提取指标名称列表
        indicator_names = [ind.name for ind in strategy.indicators]

        # 重新初始化 expr_compiler 以使用正确的参数和指标名称
        self.expr_compiler = ExpressionCompiler(strategy_param_names, indicator_names)
        
        # 编译指标
        indicator_methods = []
        indicator_calculations = []
        for ind in strategy.indicators:
            method_code = self.indicator_compiler.compile(ind, strategy_param_names)
            indicator_methods.append(method_code)
            indicator_calculations.append(f"        self.indicators['{ind.name}'] = self.{ind.name}(data)")
        
        # 编译条件
        condition_methods = []
        for i, cond in enumerate(strategy.conditions):
            method_code = self._compile_condition(cond, i)
            condition_methods.append(method_code)
        
        # 编译退出规则
        exit_methods = []
        for i, exit_rule in enumerate(strategy.exit_rules):
            method_code = self._compile_exit_rule(exit_rule, i)
            exit_methods.append(method_code)
        
        # 提取参数
        param_defs = []
        param_assignments = []
        for param in strategy.parameters:
            default = f"={param.default_value}" if hasattr(param, 'default_value') and param.default_value else ""
            param_defs.append(f"{param.name}{default}")
            param_assignments.append(f"        self.params['{param.name}'] = {param.name}")
        
        # 编译仓位管理
        position_mgmt = self._compile_position_management(strategy.position_mgmt)
        
        # 生成完整类代码
        code = f'''# Auto-generated strategy from DSL
# Generated at: {datetime.now().isoformat()}

import numpy as np
import pandas as pd
from typing import Dict, Any, Optional
from dataclasses import dataclass

@dataclass
class Signal:
    timestamp: pd.Timestamp
    action: str  # BUY, SELL, HOLD, CLOSE
    confidence: float
    position_size: float
    metadata: Dict[str, Any]


class {class_name}:
    """
    {strategy.description or f"Auto-generated strategy: {strategy.name}"}
    """
    
    def __init__(self, {', '.join(param_defs) if param_defs else ''}):
        self.name = "{strategy.name}"
        self.indicators = {{}}
        self.params = {{}}
{chr(10).join(param_assignments)}
        
        # Position management
{position_mgmt}
        
        # Track state
        self.position = 0
        self.entry_price = 0.0
        self.last_signal = None
    
    def calculate_indicators(self, data: pd.DataFrame) -> Dict[str, pd.Series]:
        """Calculate all indicators"""
{chr(10).join(indicator_calculations)}
        return self.indicators
    
{chr(10).join(indicator_methods)}
    
    def generate_signal(self, data: pd.DataFrame) -> Optional[Signal]:
        """Generate trading signal"""
        # Update indicators
        self.calculate_indicators(data)
        
        # Check exit rules first
        exit_signal = self.check_exit_rules(data)
        if exit_signal:
            return exit_signal
        
        # Check entry conditions
{self._compile_condition_calls(strategy.conditions)}
        
        return None
    
{chr(10).join(condition_methods)}
    
{chr(10).join(exit_methods)}
    
    def check_exit_rules(self, data: pd.DataFrame) -> Optional[Signal]:
        """Check exit conditions"""
{self._compile_exit_calls(strategy.exit_rules)}
        return None
    
    def calculate_position_size(self, capital: float, confidence: float, data: Optional[pd.DataFrame] = None) -> float:
        """
        Calculate position size based on risk management
        
        Args:
            capital: Total capital available
            confidence: Signal confidence (0-1)
            data: Optional market data for price lookup
        
        Returns:
            Position size (number of shares)
        """
        risk_amount = capital * self.risk_per_trade
        
        if self.position > 0:
            # Already in position, check if we should add
            max_position_value = capital * self.max_position
            current_price = data['close'].iloc[-1] if data is not None else 100.0
            current_value = self.position * current_price
            available = max_position_value - current_value
            return min(available, risk_amount * confidence * 10)
        else:
            # New position
            max_position_value = capital * self.max_position
            position_value = min(max_position_value, risk_amount * confidence * 10)
            current_price = data['close'].iloc[-1] if data is not None else 100.0
            return position_value / current_price
    
    def update_position(self, signal: Signal):
        """Update internal position tracking"""
        if signal.action == 'BUY':
            self.position += signal.position_size
            if self.entry_price == 0:
                self.entry_price = signal.price if hasattr(signal, 'price') else 0
        elif signal.action == 'SELL':
            self.position -= signal.position_size
            if self.position <= 0:
                self.position = 0
                self.entry_price = 0
        elif signal.action == 'CLOSE':
            self.position = 0
            self.entry_price = 0
        
        self.last_signal = signal
'''
        return code
    
    def _to_camel_case(self, name: str) -> str:
        """转换名称为驼峰命名法"""
        parts = name.replace('-', '_').split('_')
        return ''.join(p.capitalize() for p in parts)
    
    def _compile_condition(self, condition, index: int) -> str:
        """编译条件为方法"""
        triggers = []
        for trigger in condition.triggers:
            trigger_code = self.expr_compiler.compile(trigger.expr)
            triggers.append(trigger_code)
        
        combined_trigger = ' and '.join(triggers) if len(triggers) > 1 else triggers[0]
        
        action = condition.action
        action_type = action.action_type
        position_size = self.expr_compiler.compile(action.position_size) if hasattr(action, 'position_size') else "1.0"
        confidence = self.expr_compiler.compile(action.confidence) if hasattr(action, 'confidence') else "0.5"
        
        priority = condition.priority if hasattr(condition, 'priority') and condition.priority else 1
        
        code = f'''
    def condition_{index}(self, data: pd.DataFrame) -> Optional[Signal]:
        """{condition.description or f"Condition {index}"}"""
        if {combined_trigger}:
            return Signal(
                timestamp=data.index[-1],
                action="{action_type}",
                confidence={confidence},
                position_size={position_size},
                metadata={{
                    'condition': '{condition.name or f"condition_{index}"}',
                    'priority': {priority}
                }}
            )
        return None
'''
        return code
    
    def _compile_condition_calls(self, conditions) -> str:
        """编译条件调用代码"""
        calls = []
        for i, _ in enumerate(conditions):
            calls.append(f'''        signal = self.condition_{i}(data)
        if signal:
            signal.position_size = self.calculate_position_size(100000, signal.confidence)
            self.update_position(signal)
            return signal''')
        return '\n'.join(calls)
    
    def _compile_exit_rule(self, exit_rule, index: int) -> str:
        """编译退出规则"""
        condition_code = self.expr_compiler.compile(exit_rule.condition)
        exit_type = exit_rule.exit_type if hasattr(exit_rule, 'exit_type') else 'SIGNAL'
        
        code = f'''
    def exit_rule_{index}(self, data: pd.DataFrame) -> Optional[Signal]:
        """Exit rule: {exit_type}"""
        if self.position != 0 and {condition_code}:
            return Signal(
                timestamp=data.index[-1],
                action='CLOSE',
                confidence=1.0,
                position_size=abs(self.position),
                metadata={{'exit_type': '{exit_type}', 'rule': 'exit_{index}'}}
            )
        return None
'''
        return code
    
    def _compile_exit_calls(self, exit_rules) -> str:
        """编译退出规则调用代码"""
        calls = []
        for i, _ in enumerate(exit_rules):
            calls.append(f'''        signal = self.exit_rule_{i}(data)
        if signal:
            self.update_position(signal)
            return signal''')
        return '\n'.join(calls)
    
    def _compile_position_management(self, pos_mgmt) -> str:
        """编译仓位管理配置"""
        max_pos = self.expr_compiler.compile(pos_mgmt.max_position.value) if hasattr(pos_mgmt.max_position, 'value') else "0.95"
        risk = self.expr_compiler.compile(pos_mgmt.risk_per_trade.value) if hasattr(pos_mgmt.risk_per_trade, 'value') else "0.02"
        
        lines = [
            f"        self.max_position = {max_pos}",
            f"        self.risk_per_trade = {risk}",
        ]
        
        if pos_mgmt.trailing_stop:
            trailing = self.expr_compiler.compile(pos_mgmt.trailing_stop.value)
            lines.append(f"        self.trailing_stop = {trailing}")
        
        if pos_mgmt.take_profit:
            if hasattr(pos_mgmt.take_profit, 'value'):
                tp = self.expr_compiler.compile(pos_mgmt.take_profit.value)
                lines.append(f"        self.take_profit = {tp}")
            elif hasattr(pos_mgmt.take_profit, 'ratio'):
                ratio = self.expr_compiler.compile(pos_mgmt.take_profit.ratio)
                lines.append(f"        self.take_profit_ratio = {ratio}")
        
        return '\n'.join(lines)


class DSLParser:
    """DSL解析器"""
    
    def __init__(self, use_cache: bool = True):
        self.mm = get_metamodel()
        self.validator = DSLValidator()
        self.compiler = StrategyCompiler()
        self.source_code = ""
        self.use_cache = use_cache
        self.cache = CompilationCache() if use_cache else None
    
    def compile(self, dsl_code: str, use_cache: Optional[bool] = None) -> str:
        """编译DSL代码为Python代码（支持缓存）"""
        if use_cache is None:
            use_cache = self.use_cache
        
        cache_key = None
        if use_cache and self.cache:
            cache_key = self.cache.get_cache_key(dsl_code)
            if self.cache.is_cache_hit(cache_key):
                cached_code = self.cache.get_cached_code(cache_key)
                if cached_code:
                    return cached_code
        
        python_code = self._compile_without_cache(dsl_code)
        
        if use_cache and self.cache and cache_key:
            self.cache.cache_code(cache_key, python_code)
        
        return python_code
    
    def _compile_without_cache(self, dsl_code: str) -> str:
        """不使用缓存编译DSL代码"""
        try:
            model = self.parse(dsl_code)
            return self.compiler.compile(model)
        except (TextXSyntaxError, TextXSemanticError) as e:
            raise self._create_compile_error(
                line=1,
                column=1,
                message=str(e),
                source_code=dsl_code
            )
    
    def parse(self, dsl_code: str) -> Any:
        """解析DSL代码"""
        self.source_code = dsl_code
        try:
            model = self.mm.model_from_str(dsl_code)
            errors = self.validator.validate(model)
            if errors:
                raise self._create_validation_error(errors, dsl_code)
            return model
        except TextXSyntaxError as e:
            line, col = self._extract_line_column(dsl_code, str(e))
            raise self._create_compile_error(
                line=line,
                column=col,
                message=f"语法错误: {str(e)}",
                source_code=dsl_code
            )
        except TextXSemanticError as e:
            line, col = self._extract_line_column(dsl_code, str(e))
            raise self._create_compile_error(
                line=line,
                column=col,
                message=f"语义错误: {str(e)}",
                source_code=dsl_code
            )
    
    def _extract_line_column(self, dsl_code: str, error_msg: str) -> tuple:
        """从错误信息中提取行号和列号"""
        match = re.search(r'line:?\s*(\d+)', error_msg, re.IGNORECASE)
        if match:
            line = int(match.group(1))
            col_match = re.search(r'col:?\s*(\d+)', error_msg, re.IGNORECASE)
            col = int(col_match.group(1)) if col_match else 1
            return line, col
        return 1, 1
    
    def _create_validation_error(self, errors: List[str], source_code: str) -> CompileError:
        """创建验证错误"""
        first_error = errors[0]
        line, col = self._extract_line_column(source_code, first_error)
        return CompileError(
            line=line,
            column=col,
            message=first_error,
            severity="error",
            source_line=line,
            source_column=col,
            code_snippet=ErrorReporter(source_code).get_code_snippet(line) if source_code else ""
        )
    
    def _create_compile_error(self, line: int, column: int, message: str, source_code: str = "") -> CompileError:
        """创建编译错误"""
        reporter = ErrorReporter(source_code) if source_code else None
        return CompileError(
            line=line,
            column=column,
            message=message,
            severity="error",
            source_line=line,
            source_column=column,
            code_snippet=reporter.get_code_snippet(line) if reporter else ""
        )
    
    def compile_to_file(self, dsl_code: str, output_path: str, use_cache: Optional[bool] = None):
        """编译DSL代码并保存到文件"""
        python_code = self.compile(dsl_code, use_cache=use_cache)
        with open(output_path, 'w', encoding='utf-8') as f:
            f.write(python_code)
        return output_path
