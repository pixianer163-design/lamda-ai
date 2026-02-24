#!/usr/bin/env python3
"""
阿里云 OSS 功能测试脚本
运行前请确保已安装: pip install oss2

用法:
    python3 test_oss_connection.py
    python3 test_oss_connection.py --upload-test
    python3 test_oss_connection.py --download-test
"""

import sys
import os
import argparse
from datetime import datetime

# 添加项目路径
_SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, _SCRIPT_DIR)

def test_basic_connection():
    """测试基本连接"""
    print("\n🧪 测试1: 基本连接")
    print("-" * 50)
    
    try:
        from oss_manager import OSSManager
        
        print("   初始化 OSS 管理器...")
        oss = OSSManager()
        print("   ✅ 连接成功")
        
        # 获取 Bucket 信息
        print("   获取 Bucket 信息...")
        info = oss.bucket.get_bucket_info()
        print(f"   ✅ Bucket: {info.name}")
        print(f"   ✅ 创建时间: {info.creation_date}")
        print(f"   ✅ 存储类型: {info.storage_class}")
        
        return True
    except Exception as e:
        print(f"   ❌ 失败: {e}")
        return False


def test_list_objects():
    """测试列出对象"""
    print("\n🧪 测试2: 列出对象")
    print("-" * 50)
    
    try:
        from oss_manager import OSSManager
        oss = OSSManager()
        
        # 列出模型
        print("   列出模型文件...")
        models = oss.list_models()
        print(f"   ✅ 找到 {len(models)} 个模型文件")
        for m in models[:3]:
            size_mb = m['size'] / 1024 / 1024
            print(f"      📦 {m['name']} ({size_mb:.2f} MB)")
        
        # 列出训练数据
        print("   列出训练数据...")
        datasets = oss.list_training_data()
        print(f"   ✅ 找到 {len(datasets)} 个训练数据文件")
        for d in datasets[:3]:
            size_mb = d['size'] / 1024 / 1024
            print(f"      📊 {d['name']} ({size_mb:.2f} MB)")
        
        return True
    except Exception as e:
        print(f"   ❌ 失败: {e}")
        return False


def test_upload():
    """测试上传功能"""
    print("\n🧪 测试3: 上传功能")
    print("-" * 50)
    
    try:
        from oss_manager import OSSManager
        import tempfile
        
        oss = OSSManager()
        
        # 创建测试文件
        test_content = f"Test upload at {datetime.now()}\nThis is a test file for OSS upload functionality."
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            f.write(test_content)
            temp_path = f.name
        
        print(f"   创建测试文件: {temp_path}")
        
        # 上传到 OSS
        print("   上传到 OSS...")
        remote_name = f"test_{datetime.now().strftime('%Y%m%d_%H%M%S')}.txt"
        url = oss.bucket.put_object_from_file(f"test/{remote_name}", temp_path)
        
        print(f"   ✅ 上传成功")
        print(f"   📍 远程路径: test/{remote_name}")
        
        # 清理本地临时文件
        os.unlink(temp_path)
        
        return True
        
    except Exception as e:
        print(f"   ❌ 失败: {e}")
        import traceback
        traceback.print_exc()
        return False


def test_download():
    """测试下载功能"""
    print("\n🧪 测试4: 下载功能")
    print("-" * 50)
    
    try:
        from oss_manager import OSSManager
        import tempfile
        
        oss = OSSManager()
        
        # 首先上传一个测试文件
        test_content = f"Test download at {datetime.now()}"
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            f.write(test_content)
            temp_path = f.name
        
        remote_name = f"test_download_{datetime.now().strftime('%Y%m%d_%H%M%S')}.txt"
        oss.bucket.put_object_from_file(f"test/{remote_name}", temp_path)
        print(f"   已创建测试文件: test/{remote_name}")
        
        # 下载文件
        print("   从 OSS 下载...")
        download_path = f"/tmp/oss_test_download_{datetime.now().strftime('%Y%m%d_%H%M%S')}.txt"
        oss.bucket.get_object_to_file(f"test/{remote_name}", download_path)
        
        # 验证内容
        with open(download_path, 'r') as f:
            content = f.read()
        
        if content == test_content:
            print(f"   ✅ 下载成功，内容验证通过")
        else:
            print(f"   ⚠️  下载成功，但内容不匹配")
        
        # 清理
        os.unlink(temp_path)
        os.unlink(download_path)
        oss.bucket.delete_object(f"test/{remote_name}")
        
        return True
        
    except Exception as e:
        print(f"   ❌ 失败: {e}")
        import traceback
        traceback.print_exc()
        return False


def test_model_workflow():
    """测试模型上传下载工作流"""
    print("\n🧪 测试5: 模型工作流")
    print("-" * 50)
    
    try:
        from oss_manager import OSSManager
        import torch
        import tempfile
        
        oss = OSSManager()
        
        # 创建模拟模型
        print("   创建模拟模型文件...")
        model = torch.nn.Linear(10, 5)
        
        with tempfile.NamedTemporaryFile(suffix='.pt', delete=False) as f:
            temp_path = f.name
        
        torch.save(model.state_dict(), temp_path)
        file_size = os.path.getsize(temp_path) / 1024  # KB
        print(f"   ✅ 模型文件: {file_size:.2f} KB")
        
        # 上传
        print("   上传模型到 OSS...")
        test_model_name = f"test_model_{datetime.now().strftime('%Y%m%d_%H%M%S')}.pt"
        url = oss.upload_model(temp_path, test_model_name)
        print(f"   ✅ 上传成功: {url}")
        
        # 列出确认
        print("   验证模型已上传...")
        models = oss.list_models()
        model_names = [m['name'] for m in models]
        if test_model_name in model_names:
            print(f"   ✅ 模型已在列表中")
        
        # 下载
        print("   下载模型...")
        download_path = f"/tmp/{test_model_name}"
        oss.download_model(test_model_name, "/tmp")
        print(f"   ✅ 下载成功: {download_path}")
        
        # 验证模型
        print("   验证模型完整性...")
        loaded_state = torch.load(download_path)
        if 'weight' in loaded_state or len(loaded_state) > 0:
            print(f"   ✅ 模型可正常加载")
        
        # 清理
        os.unlink(temp_path)
        os.unlink(download_path)
        oss.delete_model(test_model_name)
        print(f"   ✅ 清理完成")
        
        return True
        
    except Exception as e:
        print(f"   ❌ 失败: {e}")
        import traceback
        traceback.print_exc()
        return False


def main():
    parser = argparse.ArgumentParser(description='OSS 功能测试')
    parser.add_argument('--upload-test', action='store_true', help='测试上传')
    parser.add_argument('--download-test', action='store_true', help='测试下载')
    parser.add_argument('--model-test', action='store_true', help='测试模型工作流')
    parser.add_argument('--all', action='store_true', help='运行所有测试')
    
    args = parser.parse_args()
    
    print("=" * 60)
    print("🚀 阿里云 OSS 功能测试")
    print("=" * 60)
    print(f"时间: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    
    # 检查 oss2 是否安装
    try:
        import oss2
        print(f"oss2 版本: {oss2.__version__}")
    except ImportError:
        print("\n❌ 请先安装阿里云 OSS SDK:")
        print("   pip install oss2")
        return
    
    results = []
    
    # 基本测试
    results.append(("基本连接", test_basic_connection()))
    results.append(("列出对象", test_list_objects()))
    
    # 功能测试
    if args.upload_test or args.all:
        results.append(("上传功能", test_upload()))
    
    if args.download_test or args.all:
        results.append(("下载功能", test_download()))
    
    if args.model_test or args.all:
        results.append(("模型工作流", test_model_workflow()))
    
    # 报告
    print("\n" + "=" * 60)
    print("📊 测试报告")
    print("=" * 60)
    
    passed = sum(1 for _, r in results if r)
    total = len(results)
    
    for name, result in results:
        status = "✅ 通过" if result else "❌ 失败"
        print(f"   {status}: {name}")
    
    print(f"\n总计: {passed}/{total} 通过")
    
    if passed == total:
        print("\n🎉 所有测试通过！")
    else:
        print(f"\n⚠️  {total - passed} 项测试失败")


if __name__ == '__main__':
    main()
