"""
DSL Compilation Cache - 缓存编译结果以提升性能
"""

import hashlib
import json
import time
from pathlib import Path
from typing import Optional, Dict


class CompilationCache:
    """DSL编译缓存管理器"""
    
    def __init__(self, cache_dir: str = ".dsl_cache"):
        self.cache_dir = Path(cache_dir)
        self.metadata_file = self.cache_dir / "cache_metadata.json"
        self.cache: Dict[str, dict] = {}
        self._hits = 0
        self._misses = 0
        self._load_metadata()
    
    def _load_metadata(self):
        """加载缓存元数据"""
        if self.metadata_file.exists():
            try:
                with open(self.metadata_file, 'r') as f:
                    self.cache = json.load(f)
            except (json.JSONDecodeError, IOError):
                self.cache = {}
    
    def _save_metadata(self):
        """保存缓存元数据"""
        self.cache_dir.mkdir(parents=True, exist_ok=True)
        with open(self.metadata_file, 'w') as f:
            json.dump(self.cache, f, indent=2)
    
    def get_cache_key(self, dsl_code: str) -> str:
        """计算 DSL 代码的 SHA256 hash 作为缓存键"""
        return hashlib.sha256(dsl_code.encode('utf-8')).hexdigest()
    
    def is_cache_hit(self, cache_key: str) -> bool:
        """检查缓存是否命中"""
        if cache_key in self.cache:
            cache_file = self.cache_dir / f"{cache_key}.py"
            if cache_file.exists():
                return True
        return False
    
    def get_cached_code(self, cache_key: str) -> Optional[str]:
        """获取缓存的 Python 代码"""
        if not self.is_cache_hit(cache_key):
            return None
        
        cache_file = self.cache_dir / f"{cache_key}.py"
        try:
            with open(cache_file, 'r', encoding='utf-8') as f:
                code = f.read()
            self._hits += 1
            self.cache[cache_key]['hits'] = self.cache[cache_key].get('hits', 0) + 1
            self.cache[cache_key]['last_hit'] = time.time()
            self._save_metadata()
            return code
        except (IOError, OSError):
            return None
    
    def cache_code(self, cache_key: str, python_code: str):
        """缓存 Python 代码"""
        cache_file = self.cache_dir / f"{cache_key}.py"
        
        self.cache_dir.mkdir(parents=True, exist_ok=True)
        
        with open(cache_file, 'w', encoding='utf-8') as f:
            f.write(python_code)
        
        self.cache[cache_key] = {
            'file': f"{cache_key}.py",
            'created': time.time(),
            'last_hit': time.time(),
            'hits': 1
        }
        self._misses += 1
        self._save_metadata()
    
    def clear_cache(self) -> None:
        """
        Clear all cached code
        
        Removes all .py files from cache directory
        and resets metadata
        """
        """清空缓存"""
        for cache_key in self.cache:
            cache_file = self.cache_dir / f"{cache_key}.py"
            if cache_file.exists():
                cache_file.unlink()
        
        self.cache = {}
        self._hits = 0
        self._misses = 0
        
        if self.metadata_file.exists():
            self.metadata_file.unlink()
    
    def get_stats(self) -> Dict[str, Any]:
        """
        Get cache statistics
        
        Returns:
            Dictionary with cache stats:
            - hits: Number of cache hits
            - misses: Number of cache misses
            - hit_rate: Hit rate percentage (0-100)
            - cached_files: Number of cached files
        """
        """获取缓存统计信息"""
        total = self._hits + self._misses
        hit_rate = self._hits / total if total > 0 else 0.0
        
        return {
            'hits': self._hits,
            'misses': self._misses,
            'total': total,
            'hit_rate': hit_rate,
            'cached_count': len(self.cache)
        }
