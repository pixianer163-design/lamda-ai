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


@dataclass
class Config:
    deepseek_api_key: str
    feishu_app_id: str
    feishu_app_secret: str
    feishu_chat_id: str
    data_dir: Path
    log_dir: Path


def get_config() -> Config:
    """获取统一配置对象"""
    _load_dotenv()
    base = Path(__file__).parent.parent  # HKTech-Agent/
    return Config(
        deepseek_api_key=os.environ.get("DEEPSEEK_API_KEY", ""),
        feishu_app_id=os.environ.get("FEISHU_APP_ID", ""),
        feishu_app_secret=os.environ.get("FEISHU_APP_SECRET", ""),
        feishu_chat_id=os.environ.get("FEISHU_CHAT_ID", ""),
        data_dir=Path(os.environ.get("DATA_DIR", str(base / "data"))),
        log_dir=Path(os.environ.get("LOG_DIR", str(base / "prod" / "logs"))),
    )
