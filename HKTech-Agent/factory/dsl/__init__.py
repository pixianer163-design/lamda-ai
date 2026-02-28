"""
DSL Module - Domain Specific Language for Trading Strategies
交易策略领域特定语言模块

Usage:
    from dsl_meta_learning.dsl import DSLParser, compile_strategy
    
    # Parse and compile DSL
    parser = DSLParser()
    python_code = parser.compile(dsl_code)
    
    # Execute compiled strategy
    exec(python_code, globals())
    strategy = globals()['StrategyName']()
"""

try:
    from .grammar import get_metamodel, DSLValidator, GRAMMAR
    from .compiler import (
        DSLParser,
        StrategyCompiler,
        IndicatorCompiler,
        ExpressionCompiler
    )
    _textx_available = True
except ImportError:
    _textx_available = False
    get_metamodel = None  # type: ignore
    DSLValidator = None  # type: ignore
    GRAMMAR = None  # type: ignore
    DSLParser = None  # type: ignore
    StrategyCompiler = None  # type: ignore
    IndicatorCompiler = None  # type: ignore
    ExpressionCompiler = None  # type: ignore


def compile_strategy(dsl_code: str) -> str:
    """
    编译DSL策略代码为Python代码
    
    Args:
        dsl_code: DSL策略代码字符串
        
    Returns:
        编译后的Python代码字符串
    """
    if not _textx_available:
        raise ImportError("textx is required. Install with: pip install textx")
    parser = DSLParser()
    return parser.compile(dsl_code)


def parse_strategy(dsl_code: str):
    """
    解析DSL策略代码为AST模型
    
    Args:
        dsl_code: DSL策略代码字符串
        
    Returns:
        DSL AST模型
    """
    if not _textx_available:
        raise ImportError("textx is required. Install with: pip install textx")
    parser = DSLParser()
    return parser.parse(dsl_code)


def validate_strategy(dsl_code: str) -> list:
    """
    验证DSL策略代码
    
    Args:
        dsl_code: DSL策略代码字符串
        
    Returns:
        错误列表，空列表表示验证通过
    """
    if not _textx_available:
        raise ImportError("textx is required. Install with: pip install textx")
    parser = DSLParser()
    try:
        parser.parse(dsl_code)
        return []
    except ValueError as e:
        return str(e).split(':')[1].strip().strip('[]').split(',')


__all__ = [
    'get_metamodel',
    'DSLValidator',
    'GRAMMAR',
    'DSLParser',
    'StrategyCompiler',
    'IndicatorCompiler',
    'ExpressionCompiler',
    'compile_strategy',
    'parse_strategy',
    'validate_strategy',
]
