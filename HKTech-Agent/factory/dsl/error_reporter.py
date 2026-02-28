"""
Error Reporter - DSL 错误报告器
提供错误定位、代码片段显示和修复建议
"""

from difflib import get_close_matches
from typing import List
from .types import CompileError


class ErrorReporter:
    """错误报告器"""
    
    def __init__(self, source_code: str):
        self.source_code = source_code
        self.lines = source_code.splitlines(keepends=True)
    
    def format_error(self, error: CompileError) -> str:
        """生成带颜色和高亮的错误信息"""
        lines = []
        
        if error.source_line > 0:
            lines.append(f"❌ Error: Line {error.source_line}:{error.source_column} - {error.message}")
        else:
            lines.append(f"❌ Error: Line {error.line}:{error.column} - {error.message}")
        
        snippet = self.get_code_snippet(error.line if error.line > 0 else 1, context=3)
        if snippet:
            lines.append("")
            lines.append(snippet)
        
        if error.suggestion:
            lines.append("")
            lines.append(f"💡 Hint: {error.suggestion}")
        
        if error.valid_options:
            lines.append(f"   Valid options: {', '.join(error.valid_options)}")
        
        return '\n'.join(lines)
    
    def get_code_snippet(self, line_no: int, context: int = 3) -> str:
        """获取错误位置前后 N 行代码"""
        if not self.lines:
            return ""
        
        start = max(0, line_no - 1 - context)
        end = min(len(self.lines), line_no - 1 + context + 1)
        
        lines = []
        for i in range(start, end):
            line_num = i + 1
            content = self.lines[i].rstrip('\n\r')
            prefix = "  " if line_num != line_no else "> "
            indicator = "   " if line_num != line_no else "   "
            
            if line_num == line_no:
                lines.append(f"{prefix}{line_num:4} | {content}")
                col = getattr(self, '_current_column', 1)
                lines.append(f"{indicator}    | {' ' * (col - 1)}^")
            else:
                lines.append(f"{prefix}{line_num:4} | {content}")
        
        return '\n'.join(lines)
    
    def suggest_fix(self, error_type: str, invalid_value: str, valid_options: List[str]) -> str:
        """基于编辑距离提供拼写纠错建议"""
        if not valid_options:
            return ""
        
        close_matches = get_close_matches(invalid_value, valid_options, n=1, cutoff=0.6)
        
        if close_matches:
            return f"Did you mean '{close_matches[0]}'?"
        
        return ""
    
    @staticmethod
    def get_valid_indicators() -> List[str]:
        """获取所有有效的指标类型"""
        return ['SMA', 'EMA', 'RSI', 'MACD', 'BB', 'ATR', 'KDJ', 'VWAP', 'ADX', 'CCI']
    
    @staticmethod
    def get_valid_builtin_funcs() -> List[str]:
        """获取所有有效的内置函数"""
        return ['crossover', 'crossunder', 'above', 'below', 'highest', 'lowest', 'change_pct']
    
    @staticmethod
    def get_valid_action_types() -> List[str]:
        """获取所有有效的动作类型"""
        return ['BUY', 'SELL', 'HOLD', 'CLOSE']


def format_compile_error(error: CompileError, source_code: str = "") -> str:
    """便捷函数：格式化编译错误"""
    if source_code:
        reporter = ErrorReporter(source_code)
        return reporter.format_error(error)
    return str(error)
