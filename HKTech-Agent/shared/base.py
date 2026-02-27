#!/usr/bin/env python3
"""
统一常量导入模块
提供 get_constants() 函数，统一所有模块的常量导入方式
"""

import os
import sys

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

_shared_path = os.path.join(PROJECT_ROOT, "shared")
for path in [_shared_path]:
    if path not in sys.path:
        sys.path.insert(0, path)

try:
    from shared import constants as _constants_module
    CONSTANTS_AVAILABLE = True
except ImportError:
    CONSTANTS_AVAILABLE = False
    _constants_module = None


DEFAULT_STOCKS = ["00700", "09988", "03690"]

_STOCK_INFO = {
    "00700": {"name": "腾讯控股", "sector": "互联网"},
    "09988": {"name": "阿里巴巴", "sector": "电商"},
    "03690": {"name": "美团-W", "sector": "本地生活"},
    "01810": {"name": "小米集团-W", "sector": "硬件"}
}


class Constants:
    """常量容器"""
    
    def __init__(self):
        self.available = CONSTANTS_AVAILABLE
        self._module = _constants_module
    
    @property
    def DEFAULT_STOCKS(self):
        if self.available and self._module is not None:
            return getattr(self._module, 'DEFAULT_STOCKS', DEFAULT_STOCKS)
        return DEFAULT_STOCKS
    
    @property
    def STOCKS(self):
        if self.available and self._module is not None:
            return getattr(self._module, 'STOCKS', _STOCK_INFO)
        return _STOCK_INFO
    
    @property
    def STOCK_NAMES(self):
        if self.available and self._module is not None:
            return getattr(self._module, 'STOCK_NAMES', {k: v["name"] for k, v in _STOCK_INFO.items()})
        return {k: v["name"] for k, v in _STOCK_INFO.items()}
    
    @property
    def ALL_STOCKS(self):
        if self.available and self._module is not None:
            return getattr(self._module, 'ALL_STOCKS', list(_STOCK_INFO.keys()))
        return list(_STOCK_INFO.keys())
    
    @property
    def DATA_DIR(self):
        if self.available and self._module is not None:
            return getattr(self._module, 'DATA_DIR', None)
        return None
    
    @property
    def LOG_DIR(self):
        if self.available and self._module is not None:
            return getattr(self._module, 'LOG_DIR', None)
        return None
    
    def get_stock_name(self, code: str) -> str:
        if self.available and self._module is not None:
            func = getattr(self._module, 'get_stock_name', None)
            if func:
                return func(code)
        return _STOCK_INFO.get(code, {}).get("name", code)
    
    def get_yf_symbol(self, code: str) -> str:
        if self.available and self._module is not None:
            func = getattr(self._module, 'get_yf_symbol', None)
            if func:
                return func(code)
        return f"{code[:4]}.HK"
    
    def get_stock_info(self, code: str, field: str = None):
        if self.available and self._module is not None:
            func = getattr(self._module, 'get_stock_info', None)
            if func:
                return func(code, field)
        info = _STOCK_INFO.get(code, {})
        if field is None:
            return info.copy()
        return info.get(field)


_constants_instance = None


def get_constants() -> Constants:
    """获取常量容器单例"""
    global _constants_instance
    if _constants_instance is None:
        _constants_instance = Constants()
    return _constants_instance


if __name__ == "__main__":
    c = get_constants()
    print(f"Constants available: {c.available}")
    print(f"DEFAULT_STOCKS: {c.DEFAULT_STOCKS}")
    print(f"STOCKS: {list(c.STOCKS.keys())}")
