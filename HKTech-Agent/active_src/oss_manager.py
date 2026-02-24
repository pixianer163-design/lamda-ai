"""
阿里云 OSS 工具类
用于上传/下载训练数据和模型文件

用法:
    from oss_manager import OSSManager
    
    oss = OSSManager()
    
    # 上传模型
    oss.upload_model('rssm_model.pt')
    
    # 下载模型
    oss.download_model('rssm_model.pt', '/local/path/')
    
    # 上传训练数据
    oss.upload_training_data('episodes_2024_01.npy')
"""

import csv
import os
import sys
from pathlib import Path
from typing import Optional
import logging

# 配置日志
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# 阿里云 OSS SDK
try:
    import oss2
except ImportError:
    oss2 = None


class OSSManager:
    """
    阿里云 OSS 管理器
    
    自动从环境变量或配置文件读取凭证
    """
    
    def __init__(self, config_path: Optional[str] = None, csv_path: Optional[str] = None):
        """
        初始化 OSS 管理器
        
        Args:
            config_path: 配置文件路径，默认从环境变量读取
            csv_path: AccessKey.csv 路径，格式为 'AccessKey ID,AccessKey Secret' 列头，优先级: 环境变量 > csv_path > config_path
        """
        self.access_key_id = None
        self.access_key_secret = None
        self.endpoint = None
        self.bucket_name = None      # 模型 Bucket
        self.data_bucket_name = None  # 训练数据 Bucket
        self.local_cache_dir = None
        
        # 尝试从环境变量加载
        self._load_from_env()

        # 如果环境变量不存在，尝试从CSV文件加载（优先级高于config_path）
        if not self.access_key_id and csv_path:
            self._load_from_csv(csv_path)

        # 如果以上都没有，尝试从配置文件加载
        if not self.access_key_id and config_path:
            self._load_from_config(config_path)
        
        # 验证配置
        if not self._validate_config():
            raise ValueError("OSS 配置不完整，请检查环境变量或配置文件")
        
        # 初始化 OSS 客户端
        self._init_oss_client()
        
        logger.info("✅ OSS 管理器初始化完成")
        logger.info(f"   模型 Bucket: {self.bucket_name}")
        logger.info(f"   数据 Bucket: {self.data_bucket_name}")
    
    def _load_from_env(self):
        """从环境变量加载配置"""
        self.access_key_id = os.getenv('ALIYUN_ACCESS_KEY_ID')
        self.access_key_secret = os.getenv('ALIYUN_ACCESS_KEY_SECRET')
        self.endpoint = os.getenv('ALIYUN_OSS_ENDPOINT', 'oss-cn-beijing.aliyuncs.com')
        self.bucket_name = os.getenv('ALIYUN_OSS_BUCKET', 'hktech-agent-models')
        self.data_bucket_name = os.getenv('ALIYUN_DATA_BUCKET', 'cloud-training')
        self.local_cache_dir = os.getenv('ALIYUN_LOCAL_CACHE_DIR',
            os.path.join(os.path.expanduser('~'), '.hktech_agent', 'oss_cache'))
        
        if self.access_key_id:
            logger.info("✅ 从环境变量加载 OSS 配置")
    
    def _load_from_config(self, config_path: str):
        """从配置文件加载"""
        try:
            with open(config_path, 'r') as f:
                for line in f:
                    line = line.strip()
                    if not line or line.startswith('#') or '=' not in line:
                        continue
                    
                    key, value = line.split('=', 1)
                    key = key.strip()
                    value = value.strip()
                    
                    if key == 'access_key_id':
                        self.access_key_id = value
                    elif key == 'access_key_secret':
                        self.access_key_secret = value
                    elif key == 'oss_endpoint':
                        self.endpoint = value
                    elif key == 'oss_bucket':
                        self.bucket_name = value
                    elif key == 'data_bucket':
                        self.data_bucket_name = value
                    elif key == 'local_cache_dir':
                        self.local_cache_dir = value
            
            logger.info(f"✅ 从配置文件加载 OSS 配置: {config_path}")
        except Exception as e:
            logger.error(f"❌ 读取配置文件失败: {e}")

    def _load_from_csv(self, csv_path: str):
        """从 AccessKey.csv 加载凭证（格式: 'AccessKey ID,AccessKey Secret' header）"""
        try:
            with open(csv_path, 'r', encoding='utf-8') as f:
                reader = csv.DictReader(f)
                for row in reader:
                    key_id = row.get('AccessKey ID', '').strip()
                    key_secret = row.get('AccessKey Secret', '').strip()
                    if key_id and key_secret:
                        self.access_key_id = key_id
                        self.access_key_secret = key_secret
                        logger.info(f"✅ 从CSV文件加载OSS凭证: {csv_path}")
                        return
            logger.warning(f"⚠️ CSV文件中未找到有效凭证: {csv_path}")
        except FileNotFoundError:
            logger.warning(f"⚠️ AccessKey CSV文件不存在: {csv_path}")
        except Exception as e:
            logger.error(f"❌ 读取CSV凭证失败: {e}")

    def _validate_config(self) -> bool:
        """验证配置是否完整"""
        required = [
            self.access_key_id,
            self.access_key_secret,
            self.endpoint,
            self.bucket_name
        ]
        return all(required)
    
    def _init_oss_client(self):
        """初始化 OSS 客户端（双 Bucket）"""
        try:
            import oss2
            
            # 创建认证对象
            self.auth = oss2.Auth(self.access_key_id, self.access_key_secret)
            
            # 创建模型 Bucket 对象
            self.bucket = oss2.Bucket(self.auth, self.endpoint, self.bucket_name)
            
            # 创建训练数据 Bucket 对象
            if self.data_bucket_name:
                self.data_bucket = oss2.Bucket(self.auth, self.endpoint, self.data_bucket_name)
            else:
                self.data_bucket = self.bucket  # 如果没有单独配置，使用同一个
            
            # 测试连接（只测试模型 Bucket）
            self.bucket.get_bucket_info()
            logger.info(f"✅ OSS 连接成功")
            
        except ImportError:
            logger.error("❌ 请先安装阿里云 OSS SDK: pip install oss2")
            raise
        except Exception as e:
            logger.error(f"❌ OSS 连接失败: {e}")
            raise
    
    def upload_model(self, local_path: str, remote_name: Optional[str] = None) -> str:
        """
        上传模型文件到 OSS
        
        Args:
            local_path: 本地模型文件路径
            remote_name: 远程文件名，默认使用本地文件名
            
        Returns:
            远程文件 URL
        """
        if remote_name is None:
            remote_name = os.path.basename(local_path)
        
        remote_path = f"models/{remote_name}"
        
        try:
            self.bucket.put_object_from_file(remote_path, local_path)
            url = f"https://{self.bucket_name}.{self.endpoint}/{remote_path}"
            logger.info(f"✅ 模型上传成功: {url}")
            return url
        except Exception as e:
            logger.error(f"❌ 模型上传失败: {e}")
            raise
    
    def download_model(self, remote_name: str, local_dir: Optional[str] = None) -> str:
        """
        从 OSS 下载模型文件
        
        Args:
            remote_name: 远程文件名
            local_dir: 本地保存目录，默认使用缓存目录
            
        Returns:
            本地文件路径
        """
        if local_dir is None:
            local_dir = self.local_cache_dir
        
        os.makedirs(local_dir, exist_ok=True)
        
        remote_path = f"models/{remote_name}"
        local_path = os.path.join(local_dir, remote_name)
        
        try:
            self.bucket.get_object_to_file(remote_path, local_path)
            logger.info(f"✅ 模型下载成功: {local_path}")
            return local_path
        except Exception as e:
            logger.error(f"❌ 模型下载失败: {e}")
            raise
    
    def upload_training_data(self, local_path: str, dataset_name: Optional[str] = None) -> str:
        """
        上传训练数据到 OSS（使用 data_bucket）
        
        Args:
            local_path: 本地数据文件路径
            dataset_name: 数据集名称
            
        Returns:
            远程文件 URL
        """
        if dataset_name is None:
            dataset_name = os.path.basename(local_path)
        
        remote_path = f"training-data/{dataset_name}"
        
        try:
            # 使用 data_bucket 上传训练数据
            self.data_bucket.put_object_from_file(remote_path, local_path)
            url = f"https://{self.data_bucket_name}.{self.endpoint}/{remote_path}"
            logger.info(f"✅ 训练数据上传成功: {url}")
            logger.info(f"   Bucket: {self.data_bucket_name}")
            return url
        except Exception as e:
            logger.error(f"❌ 训练数据上传失败: {e}")
            raise
    
    def download_training_data(self, dataset_name: str, local_dir: Optional[str] = None) -> str:
        """
        从 OSS 下载训练数据（使用 data_bucket）
        
        Args:
            dataset_name: 数据集名称
            local_dir: 本地保存目录
            
        Returns:
            本地文件路径
        """
        if local_dir is None:
            local_dir = os.path.join(self.local_cache_dir, 'training-data')
        
        os.makedirs(local_dir, exist_ok=True)
        
        remote_path = f"training-data/{dataset_name}"
        local_path = os.path.join(local_dir, dataset_name)
        
        try:
            # 使用 data_bucket 下载训练数据
            self.data_bucket.get_object_to_file(remote_path, local_path)
            logger.info(f"✅ 训练数据下载成功: {local_path}")
            logger.info(f"   来源 Bucket: {self.data_bucket_name}")
            return local_path
        except Exception as e:
            logger.error(f"❌ 训练数据下载失败: {e}")
            raise
    
    def list_models(self) -> list:
        """列出所有模型文件"""
        try:
            import oss2 as _oss2
            models = []
            for obj in _oss2.ObjectIterator(self.bucket, prefix='models/'):
                if obj.key.endswith('.pt') or obj.key.endswith('.pth'):
                    models.append({
                        'name': os.path.basename(obj.key),
                        'size': obj.size,
                        'last_modified': obj.last_modified,
                        'key': obj.key
                    })
            return models
        except Exception as e:
            logger.error(f"❌ 列出模型失败: {e}")
            return []
    
    def list_training_data(self) -> list:
        """列出所有训练数据（使用 data_bucket）"""
        try:
            import oss2 as _oss2
            datasets = []
            for obj in _oss2.ObjectIterator(self.data_bucket, prefix='training-data/'):
                if obj.key.endswith('.npy') or obj.key.endswith('.npz'):
                    datasets.append({
                        'name': os.path.basename(obj.key),
                        'size': obj.size,
                        'last_modified': obj.last_modified,
                        'key': obj.key
                    })
            return datasets
        except Exception as e:
            logger.error(f"❌ 列出训练数据失败: {e}")
            return []
    
    def delete_model(self, remote_name: str):
        """删除远程模型"""
        remote_path = f"models/{remote_name}"
        try:
            self.bucket.delete_object(remote_path)
            logger.info(f"✅ 模型删除成功: {remote_name}")
        except Exception as e:
            logger.error(f"❌ 模型删除失败: {e}")
            raise


# 便捷函数
def upload_model(local_path: str) -> str:
    """便捷上传模型函数"""
    oss = OSSManager()
    return oss.upload_model(local_path)


def download_model(remote_name: str, local_dir: Optional[str] = None) -> str:
    """便捷下载模型函数"""
    oss = OSSManager()
    return oss.download_model(remote_name, local_dir)


if __name__ == '__main__':
    # 测试
    print("🧪 测试 OSS 管理器")
    
    try:
        # 初始化
        oss = OSSManager()
        
        # 列出模型
        print("\n📦 远程模型列表:")
        models = oss.list_models()
        for model in models:
            print(f"  - {model['name']} ({model['size'] / 1024 / 1024:.2f} MB)")
        
        # 列出训练数据
        print("\n📊 远程训练数据:")
        datasets = oss.list_training_data()
        for ds in datasets:
            print(f"  - {ds['name']} ({ds['size'] / 1024 / 1024:.2f} MB)")
        
        print("\n✅ OSS 测试完成")
        
    except Exception as e:
        print(f"\n❌ 测试失败: {e}")
        import traceback
        traceback.print_exc()
