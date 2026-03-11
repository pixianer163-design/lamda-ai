"""
统一配置管理模块
读取顺序: .env 文件 → 环境变量 → 默认值
"""

from dataclasses import dataclass
import os
from pathlib import Path


def _load_dotenv():
    """尝试加载 .env 文件（从项目根目录查找）"""
    try:
        from dotenv import load_dotenv

        base = Path(__file__).parent.parent
        env_file = base / ".env"
        if env_file.exists():
            load_dotenv(env_file)
    except ImportError:
        pass


def _get_project_root() -> Path:
    """获取项目根目录，支持环境变量覆盖"""
    env_root = os.environ.get("HKTECH_AGENT_ROOT", "")
    if env_root:
        return Path(env_root)
    return Path(__file__).parent.parent


@dataclass
class PathConfig:
    """路径配置"""

    root: Path
    data: Path
    logs: Path
    config: Path
    web: Path
    cache: Path

    @classmethod
    def from_env(cls) -> "PathConfig":
        """从环境变量创建路径配置"""
        root = _get_project_root()
        return cls(
            root=root,
            data=Path(os.environ.get("DATA_DIR", str(root / "data"))),
            logs=Path(os.environ.get("LOG_DIR", str(root / "prod" / "logs"))),
            config=Path(os.environ.get("CONFIG_DIR", str(root / "config"))),
            web=Path(os.environ.get("WEB_DIR", str(root / "web"))),
            cache=Path(os.environ.get("CACHE_DIR", str(root / "cache"))),
        )


@dataclass
class Config:
    deepseek_api_key: str
    feishu_app_id: str
    feishu_app_secret: str
    feishu_chat_id: str
    data_dir: Path
    log_dir: Path
    paths: PathConfig


def get_config() -> Config:
    """获取统一配置对象"""
    _load_dotenv()
    base = Path(__file__).parent.parent
    paths = PathConfig.from_env()
    return Config(
        deepseek_api_key=os.environ.get("DEEPSEEK_API_KEY", ""),
        feishu_app_id=os.environ.get("FEISHU_APP_ID", ""),
        feishu_app_secret=os.environ.get("FEISHU_APP_SECRET", ""),
        feishu_chat_id=os.environ.get("FEISHU_CHAT_ID", ""),
        data_dir=paths.data,
        log_dir=paths.logs,
        paths=paths,
    )


def get_paths() -> PathConfig:
    """获取路径配置（便捷函数）"""
    return PathConfig.from_env()
