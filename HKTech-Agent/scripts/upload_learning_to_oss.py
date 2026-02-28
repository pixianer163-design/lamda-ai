#!/usr/bin/env python3
"""
学习内容上传到 OSS

功能:
1. 自动上传学习报告到 OSS
2. 按日期分类存储
3. 支持多种报告类型（盘前/午间/盘后）
4. 生成访问链接
"""

import os
import sys
import json
from pathlib import Path
from datetime import datetime

# 添加父目录到路径
sys.path.insert(0, str(Path(__file__).parent.parent))

try:
    import oss2
except ImportError:
    print("❌ oss2 未安装，请运行：pip install oss2")
    sys.exit(1)


class LearningReportUploader:
    """学习报告 OSS 上传器"""
    
    def __init__(self):
        """初始化 OSS 客户端"""
        self.access_key_id = os.getenv('ALIYUN_ACCESS_KEY_ID')
        self.access_key_secret = os.getenv('ALIYUN_ACCESS_KEY_SECRET')
        self.endpoint = os.getenv('ALIYUN_OSS_ENDPOINT', 'oss-cn-beijing.aliyuncs.com')
        self.bucket_name = os.getenv('ALIYUN_OSS_BUCKET', 'hktech-agent-models')
        
        # 验证配置
        if not all([self.access_key_id, self.access_key_secret]):
            raise ValueError("OSS 配置缺失，请设置环境变量")
        
        # 初始化 OSS
        self.auth = oss2.Auth(self.access_key_id, self.access_key_secret)
        self.bucket = oss2.Bucket(self.auth, self.endpoint, self.bucket_name)
        
        print(f"✅ OSS 初始化成功：{self.bucket_name}")
    
    def upload_report(
        self,
        report_content: str,
        report_type: str,
        date: str = None
    ) -> str:
        """
        上传学习报告
        
        Args:
            report_content: 报告内容（Markdown/JSON）
            report_type: 报告类型 (pre_market/noon/after_market)
            date: 日期 (YYYY-MM-DD)，默认今天
        
        Returns:
            OSS 访问 URL
        """
        if date is None:
            date = datetime.now().strftime('%Y-%m-%d')
        
        # 生成 OSS 路径
        oss_path = f"learning_reports/{date}/{report_type}_{date}.md"
        
        # 上传文件
        try:
            self.bucket.put_object(oss_path, report_content.encode('utf-8'))
            print(f"✅ 报告已上传：{oss_path}")
            
            # 生成访问 URL
            url = self._generate_url(oss_path)
            return url
            
        except Exception as e:
            print(f"❌ 上传失败：{e}")
            raise
    
    def upload_json_report(
        self,
        report_data: dict,
        report_type: str,
        date: str = None
    ) -> str:
        """
        上传 JSON 格式学习报告
        
        Args:
            report_data: 报告数据（字典）
            report_type: 报告类型
            date: 日期
        
        Returns:
            OSS 访问 URL
        """
        if date is None:
            date = datetime.now().strftime('%Y-%m-%d')
        
        # 生成 OSS 路径
        oss_path = f"learning_reports/{date}/{report_type}_{date}.json"
        
        # 转换为 JSON
        json_content = json.dumps(report_data, ensure_ascii=False, indent=2)
        
        # 上传文件
        try:
            self.bucket.put_object(oss_path, json_content.encode('utf-8'))
            print(f"✅ JSON 报告已上传：{oss_path}")
            
            # 生成访问 URL
            url = self._generate_url(oss_path)
            return url
            
        except Exception as e:
            print(f"❌ 上传失败：{e}")
            raise
    
    def _generate_url(self, oss_path: str, expires: int = 3600) -> str:
        """
        生成临时访问 URL
        
        Args:
            oss_path: OSS 路径
            expires: 过期时间（秒），默认 1 小时
        
        Returns:
            签名 URL
        """
        url = self.bucket.sign_url('GET', oss_path, expires)
        return url
    
    def list_reports(self, date: str = None) -> list:
        """
        列出指定日期的学习报告
        
        Args:
            date: 日期 (YYYY-MM-DD)，默认今天
        
        Returns:
            报告列表
        """
        if date is None:
            date = datetime.now().strftime('%Y-%m-%d')
        
        prefix = f"learning_reports/{date}/"
        reports = []
        
        try:
            for obj in oss2.ObjectIterator(self.bucket, prefix=prefix):
                reports.append({
                    'key': obj.key,
                    'size': obj.size,
                    'last_modified': datetime.fromtimestamp(obj.last_modified / 1000)
                })
            
            return reports
            
        except Exception as e:
            print(f"❌ 列出失败：{e}")
            return []
    
    def save_to_local_and_upload(
        self,
        report_content: str,
        report_type: str,
        local_dir: str = None,
        date: str = None
    ) -> dict:
        """
        先保存到本地，再上传到 OSS
        
        Args:
            report_content: 报告内容
            report_type: 报告类型
            local_dir: 本地目录
            date: 日期
        
        Returns:
            {'local_path': ..., 'oss_url': ...}
        """
        if date is None:
            date = datetime.now().strftime('%Y-%m-%d')
        
        if local_dir is None:
            local_dir = Path(__file__).parent.parent / 'learning_reports'
        
        # 创建本地目录
        local_path = Path(local_dir) / date
        local_path.mkdir(parents=True, exist_ok=True)
        
        # 保存本地文件
        local_file = local_path / f"{report_type}_{date}.md"
        with open(local_file, 'w', encoding='utf-8') as f:
            f.write(report_content)
        
        print(f"✅ 本地保存：{local_file}")
        
        # 上传到 OSS
        oss_url = self.upload_report(report_content, report_type, date)
        
        return {
            'local_path': str(local_file),
            'oss_url': oss_url,
            'date': date,
            'type': report_type
        }


def main():
    """测试上传功能"""
    print("=" * 60)
    print("📤 学习报告 OSS 上传测试")
    print("=" * 60)
    
    # 初始化上传器
    uploader = LearningReportUploader()
    
    # 测试报告
    test_report = f"""
# 学习报告测试

**日期**: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}
**类型**: 测试报告

## 内容

这是一个测试学习报告，用于验证 OSS 上传功能。

## 测试结果

- ✅ OSS 连接成功
- ✅ 文件上传成功
- ✅ URL 生成成功

---
*自动生成*
"""
    
    # 上传测试
    result = uploader.save_to_local_and_upload(
        report_content=test_report,
        report_type='test',
        local_dir='/tmp/learning_reports'
    )
    
    print("\n" + "=" * 60)
    print("📊 上传结果")
    print("=" * 60)
    print(f"本地路径：{result['local_path']}")
    print(f"OSS URL: {result['oss_url']}")
    print(f"日期：{result['date']}")
    print(f"类型：{result['type']}")
    
    # 列出报告
    print("\n" + "=" * 60)
    print("📋 今日报告列表")
    print("=" * 60)
    reports = uploader.list_reports()
    for report in reports:
        print(f"- {report['key']} ({report['size']} bytes)")


if __name__ == '__main__':
    main()
