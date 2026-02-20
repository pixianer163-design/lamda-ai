#!/usr/bin/env python3
"""
上传训练数据和模型文件到阿里云OSS
支持命令行参数和环境变量配置
"""

import os
import sys
import csv
import time
import argparse
from pathlib import Path
from typing import List, Optional

# 阿里云OSS配置
# 从AccessKey.csv读取密钥
ACCESS_KEY_FILE = os.path.join(os.path.dirname(os.path.abspath(__file__)), "../AccessKey.csv")

# OSS配置 - 可以通过环境变量或命令行参数设置
OSS_ENDPOINT = os.environ.get("OSS_ENDPOINT", "oss-cn-beijing.aliyuncs.com")
OSS_BUCKET = os.environ.get("OSS_BUCKET", "hktech-agent-models")
OSS_PREFIX = os.environ.get("OSS_PREFIX", "world_model/v1/")

def load_access_keys(key_file: str = ACCESS_KEY_FILE):
    """从CSV文件加载AccessKey"""
    try:
        with open(key_file, 'r', encoding='utf-8') as f:
            reader = csv.DictReader(f)
            for row in reader:
                if 'AccessKey ID' in row and 'AccessKey Secret' in row:
                    return row['AccessKey ID'], row['AccessKey Secret']
    except Exception as e:
        print(f"❌ 读取AccessKey文件失败: {e}")
        return None, None
    
    print("❌ AccessKey文件格式不正确")
    return None, None

def upload_to_oss(file_paths: List[str], description: str = "训练数据和模型"):
    """上传文件到OSS"""
    # 加载AccessKey
    access_key_id, access_key_secret = load_access_keys()
    if not access_key_id or not access_key_secret:
        print("❌ 无法获取AccessKey，上传失败")
        return False
    
    print(f"🔑 使用AccessKey: {access_key_id[:8]}...{access_key_id[-4:]}")
    
    # 导入OSS模块
    try:
        import oss2
    except ImportError:
        print("❌ 未安装阿里云OSS SDK，请运行: pip install oss2")
        return False
    
    # 创建OSS连接
    auth = oss2.Auth(access_key_id, access_key_secret)
    bucket = oss2.Bucket(auth, OSS_ENDPOINT, OSS_BUCKET)
    
    # 检查bucket是否存在
    try:
        bucket.get_bucket_info()
        print(f"✅ OSS Bucket连接成功: {OSS_BUCKET}")
    except Exception as e:
        print(f"❌ OSS Bucket连接失败: {e}")
        print(f"   请检查:")
        print(f"   1. endpoint是否正确: {OSS_ENDPOINT}")
        print(f"   2. bucket名称是否正确: {OSS_BUCKET}")
        print(f"   3. AccessKey是否有访问权限")
        return False
    
    # 上传文件
    success_count = 0
    total_count = len(file_paths)
    
    for file_path in file_paths:
        if not os.path.exists(file_path):
            print(f"⚠️  文件不存在，跳过: {file_path}")
            continue
        
        # 生成OSS对象名
        file_name = os.path.basename(file_path)
        oss_object_name = f"{OSS_PREFIX}{file_name}"
        
        try:
            file_size = os.path.getsize(file_path)
            print(f"📤 上传: {file_name} ({file_size:,} bytes) -> {oss_object_name}")
            
            # 上传文件
            start_time = time.time()
            bucket.put_object_from_file(oss_object_name, file_path)
            upload_time = time.time() - start_time
            
            # 验证上传
            if bucket.object_exists(oss_object_name):
                success_count += 1
                speed = file_size / upload_time / 1024  # KB/s
                print(f"✅ 上传成功 ({upload_time:.1f}s, {speed:.1f} KB/s)")
            else:
                print(f"❌ 上传验证失败: {oss_object_name}")
                
        except Exception as e:
            print(f"❌ 上传失败 {file_name}: {e}")
    
    # 生成上传报告
    print(f"\n📊 上传报告:")
    print(f"   总文件数: {total_count}")
    print(f"   成功: {success_count}")
    print(f"   失败: {total_count - success_count}")
    
    if success_count > 0:
        print(f"\n🔗 OSS访问URL示例:")
        print(f"   https://{OSS_BUCKET}.{OSS_ENDPOINT}/{OSS_PREFIX}...")
        print(f"   注意: 需要设置bucket为公共读或使用签名URL访问")
    
    return success_count > 0

def get_important_files():
    """获取需要上传的重要文件"""
    # 项目根目录
    project_root = os.path.dirname(os.path.abspath(__file__))
    data_dir = os.path.join(project_root, "data")
    
    important_files = []
    
    # 模型文件
    model_files = [
        "rssm_model.pt",           # 训练好的世界模型
        "training_episodes.json",  # 训练数据
    ]
    
    for file_name in model_files:
        file_path = os.path.join(data_dir, file_name)
        if os.path.exists(file_path):
            important_files.append(file_path)
        else:
            print(f"⚠️  重要文件不存在: {file_path}")
    
    # 配置文件
    config_files = [
        os.path.join(project_root, "README.md"),
        os.path.join(project_root, "ARCHITECTURE.md"),
    ]
    
    for file_path in config_files:
        if os.path.exists(file_path):
            important_files.append(file_path)
    
    return important_files

def parse_args():
    """解析命令行参数"""
    parser = argparse.ArgumentParser(description="上传训练数据和模型文件到阿里云OSS")
    parser.add_argument("--endpoint", "-e", default=OSS_ENDPOINT,
                       help=f"OSS端点 (默认: {OSS_ENDPOINT})")
    parser.add_argument("--bucket", "-b", default=OSS_BUCKET,
                       help=f"OSS Bucket名称 (默认: {OSS_BUCKET})")
    parser.add_argument("--prefix", "-p", default=OSS_PREFIX,
                       help=f"OSS路径前缀 (默认: {OSS_PREFIX})")
    parser.add_argument("--test", "-t", action="store_true",
                       help="测试OSS连接")
    parser.add_argument("--auto", "-a", action="store_true",
                       help="自动模式，不询问确认")
    parser.add_argument("--files", "-f", nargs="+",
                       help="指定要上传的文件列表")
    parser.add_argument("--all-data", action="store_true",
                       help="上传data目录下的所有文件")
    return parser.parse_args()

def interactive_main():
    """交互式主函数"""
    print("="*60)
    print("☁️  阿里云OSS上传工具")
    print("="*60)
    print()
    
    # 检查配置
    print("📋 当前配置:")
    print(f"   端点(Endpoint): {OSS_ENDPOINT}")
    print(f"   Bucket: {OSS_BUCKET}")
    print(f"   路径前缀: {OSS_PREFIX}")
    print()
    
    # 提示用户确认配置
    print("⚠️  重要: 请确认以上配置是否正确")
    print("   可以通过以下方式修改配置:")
    print("   1. 设置环境变量: OSS_ENDPOINT, OSS_BUCKET, OSS_PREFIX")
    print("   2. 命令行参数: --endpoint, --bucket, --prefix")
    print()
    
    confirm = input("是否继续上传? (y/N): ")
    if confirm.lower() != 'y':
        print("上传取消")
        return False
    
    return True

def upload_files_with_config(endpoint: str, bucket: str, prefix: str, 
                           auto_mode: bool = False, specific_files: Optional[List[str]] = None,
                           all_data: bool = False) -> bool:
    """使用指定配置上传文件"""
    global OSS_ENDPOINT, OSS_BUCKET, OSS_PREFIX
    OSS_ENDPOINT = endpoint
    OSS_BUCKET = bucket
    OSS_PREFIX = prefix
    
    if not auto_mode and not interactive_main():
        return False
    
    # 获取要上传的文件
    print("\n📁 扫描重要文件...")
    if specific_files:
        files_to_upload = []
        for file_path in specific_files:
            if os.path.exists(file_path):
                files_to_upload.append(file_path)
            else:
                print(f"⚠️  文件不存在: {file_path}")
    elif all_data:
        data_dir = os.path.join(os.path.dirname(os.path.abspath(__file__)), "data")
        files_to_upload = []
        if os.path.exists(data_dir):
            for file_name in os.listdir(data_dir):
                file_path = os.path.join(data_dir, file_name)
                if os.path.isfile(file_path):
                    files_to_upload.append(file_path)
    else:
        files_to_upload = get_important_files()
    
    if not files_to_upload:
        print("❌ 没有找到需要上传的文件")
        return False
    
    print(f"找到 {len(files_to_upload)} 个文件:")
    total_size = 0
    for file_path in files_to_upload:
        file_size = os.path.getsize(file_path) if os.path.exists(file_path) else 0
        total_size += file_size
        print(f"  • {os.path.basename(file_path)} ({file_size:,} bytes)")
    
    print(f"总大小: {total_size:,} bytes ({total_size/1024/1024:.2f} MB)")
    
    if not auto_mode:
        confirm = input(f"\n确认上传以上文件到OSS? (y/N): ")
        if confirm.lower() != 'y':
            print("上传取消")
            return False
    
    print("\n🚀 开始上传到阿里云OSS...")
    success = upload_to_oss(files_to_upload, "HKTech Agent世界模型")
    
    if success:
        print("\n🎉 上传完成!")
        print("💡 建议:")
        print("   1. 定期上传新训练的模型")
        print("   2. 考虑设置自动备份策略")
        print("   3. 在代码中集成从OSS加载模型的功能")
        return True
    else:
        print("\n❌ 上传失败，请检查配置和网络")
        return False

def test_oss_connection(endpoint: str, bucket: str):
    """测试OSS连接"""
    print("🧪 测试OSS连接...")
    
    access_key_id, access_key_secret = load_access_keys()
    if not access_key_id or not access_key_secret:
        print("❌ AccessKey加载失败")
        return False
    
    print(f"✅ AccessKey加载成功: {access_key_id[:8]}...")
    
    try:
        import oss2
        auth = oss2.Auth(access_key_id, access_key_secret)
        bucket_obj = oss2.Bucket(auth, endpoint, bucket)
        
        # 尝试获取bucket信息
        info = bucket_obj.get_bucket_info()
        print(f"✅ OSS连接成功!")
        print(f"   Bucket: {info.name}")
        print(f"   位置: {info.location}")
        print(f"   创建时间: {info.creation_date}")
        return True
        
    except oss2.exceptions.NoSuchBucket:
        print(f"❌ Bucket不存在: {bucket}")
        print(f"   请在OSS控制台创建bucket: https://oss.console.aliyun.com/")
        print(f"   或使用以下命令创建:")
        print(f"   ossutil mb oss://{bucket} -e {endpoint}")
    except oss2.exceptions.AccessDenied:
        print("❌ 访问被拒绝，请检查AccessKey权限")
    except Exception as e:
        print(f"❌ 连接失败: {e}")
    
    return False

def main():
    """主函数"""
    args = parse_args()
    
    if args.test:
        success = test_oss_connection(args.endpoint, args.bucket)
        sys.exit(0 if success else 1)
    
    success = upload_files_with_config(
        endpoint=args.endpoint,
        bucket=args.bucket,
        prefix=args.prefix,
        auto_mode=args.auto,
        specific_files=args.files,
        all_data=args.all_data
    )
    sys.exit(0 if success else 1)

if __name__ == "__main__":
    main()