#!/usr/bin/env python3
"""
恒生 Agent 每日健康检查

每天 08:50 自动运行，检查系统状态，确保 09:00 盘前学习正常执行

检查项目：
1. 数据文件新鲜度
2. 模拟账户状态
3. 网络连接
4. API 可用性
5. 磁盘空间
6. 定时任务配置
"""

import json
import os
import sys
import requests
from datetime import datetime, timedelta
from typing import Dict, List, Tuple

# 配置
DATA_DIR = "/opt/hktech-agent/data"
LOG_DIR = "/opt/hktech-agent/logs"
WEBHOOK_URL = "https://open.feishu.cn/open-apis/bot/v2/hook/7a7dbe38-9181-4311-8094-ebaf6cf0f378"


class HealthChecker:
    """健康检查器"""
    
    def __init__(self):
        self.issues = []
        self.warnings = []
        self.ok_count = 0
    
    def check_data_freshness(self) -> bool:
        """检查数据新鲜度"""
        print("\n📊 检查数据新鲜度...")
        
        files_to_check = [
            ("market_data_latest.json", 24),  # 24 小时
            ("portfolio.json", 48),  # 48 小时
        ]
        
        all_ok = True
        for filename, max_age_hours in files_to_check:
            filepath = os.path.join(DATA_DIR, filename)
            
            if not os.path.exists(filepath):
                self.issues.append(f"❌ 数据文件缺失：{filename}")
                all_ok = False
                continue
            
            file_mtime = datetime.fromtimestamp(os.path.getmtime(filepath))
            file_age = datetime.now() - file_mtime
            
            if file_age.total_seconds() > max_age_hours * 3600:
                self.warnings.append(f"⚠️ 数据文件过时：{filename} ({file_age})")
            else:
                self.ok_count += 1
                print(f"   ✅ {filename}: {file_age}")
        
        return all_ok
    
    def check_account_status(self) -> bool:
        """检查账户状态"""
        print("\n💼 检查账户状态...")
        
        account_file = os.path.join(DATA_DIR, "paper_account.json")
        
        if not os.path.exists(account_file):
            self.issues.append("❌ 模拟账户文件缺失")
            return False
        
        with open(account_file, 'r') as f:
            account = json.load(f)
        
        cash = account.get('cash', 0)
        positions = account.get('positions', {})
        
        if cash < 0:
            self.issues.append(f"❌ 现金异常：{cash}")
            return False
        
        positions_count = sum(1 for p in positions.values() if p.get('shares', 0) > 0)
        
        self.ok_count += 1
        print(f"   ✅ 现金：{cash:,.0f} 元")
        print(f"   ✅ 持仓：{positions_count} 只")
        
        return True
    
    def check_network(self) -> bool:
        """检查网络连接"""
        print("\n🌐 检查网络连接...")
        
        urls = [
            ("新浪财经", "https://hq.sinajs.cn/list=hk00700"),
            ("飞书 API", "https://open.feishu.cn/open-apis/cli/v2/health"),
        ]
        
        all_ok = True
        for name, url in urls:
            try:
                resp = requests.get(url, timeout=5)
                if resp.status_code == 200:
                    self.ok_count += 1
                    print(f"   ✅ {name}: 正常")
                else:
                    self.warnings.append(f"⚠️ {name}: HTTP {resp.status_code}")
            except Exception as e:
                self.warnings.append(f"⚠️ {name}: {str(e)}")
                all_ok = False
        
        return all_ok
    
    def check_disk_space(self) -> bool:
        """检查磁盘空间"""
        print("\n💾 检查磁盘空间...")
        
        import shutil
        total, used, free = shutil.disk_usage("/opt")
        
        free_gb = free / (1024**3)
        free_pct = free / total * 100
        
        if free_pct < 10:
            self.issues.append(f"❌ 磁盘空间不足：{free_gb:.1f}GB ({free_pct:.1f}%)")
            return False
        elif free_pct < 20:
            self.warnings.append(f"⚠️ 磁盘空间紧张：{free_gb:.1f}GB ({free_pct:.1f}%)")
        else:
            self.ok_count += 1
            print(f"   ✅ 磁盘空间：{free_gb:.1f}GB ({free_pct:.1f}%)")
        
        return True
    
    def check_cron_config(self) -> bool:
        """检查定时任务配置"""
        print("\n⏰ 检查定时任务配置...")
        
        import subprocess
        try:
            result = subprocess.run(['crontab', '-l'], capture_output=True, text=True, timeout=5)
            
            if result.returncode == 0:
                cron_content = result.stdout
                
                # 检查关键任务
                required_tasks = [
                    "noon_learning",
                    "afternoon_learning",
                ]
                
                for task in required_tasks:
                    if task in cron_content:
                        self.ok_count += 1
                        print(f"   ✅ 任务配置：{task}")
                    else:
                        self.warnings.append(f"⚠️ 任务配置缺失：{task}")
                
                return True
            else:
                self.warnings.append("⚠️ 无法读取 crontab")
                return False
                
        except Exception as e:
            self.warnings.append(f"⚠️ 检查 crontab 失败：{e}")
            return False
    
    def generate_report(self) -> str:
        """生成健康报告"""
        timestamp = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
        
        status = "✅ 正常" if not self.issues else "❌ 异常"
        
        report = f"""🏥 恒生 Agent 健康检查

⏰ 时间：{timestamp}
📊 状态：{status}

✅ 检查通过：{self.ok_count} 项"""
        
        if self.warnings:
            report += f"\n\n⚠️ 警告 ({len(self.warnings)}):\n"
            for w in self.warnings:
                report += f"• {w}\n"
        
        if self.issues:
            report += f"\n❌ 问题 ({len(self.issues)}):\n"
            for i in self.issues:
                report += f"• {i}\n"
        
        if not self.issues:
            report += "\n\n✅ 系统状态良好，可以开始今日交易！"
        else:
            report += "\n\n⚠️ 请先解决上述问题！"
        
        return report
    
    def push_report(self, report: str) -> bool:
        """推送健康报告"""
        try:
            resp = requests.post(
                WEBHOOK_URL,
                json={"msg_type": "text", "content": {"text": report}},
                timeout=10
            )
            
            result = resp.json()
            return result.get("StatusCode") == 0 or result.get("code") == 0
            
        except Exception as e:
            print(f"❌ 推送失败：{e}")
            return False
    
    def run(self, push: bool = True) -> int:
        """运行所有检查"""
        print("="*60)
        print("🏥 恒生 Agent 健康检查")
        print("="*60)
        print(f"⏰ 检查时间：{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        print("="*60)
        
        # 执行检查
        self.check_data_freshness()
        self.check_account_status()
        self.check_network()
        self.check_disk_space()
        self.check_cron_config()
        
        # 生成报告
        report = self.generate_report()
        
        print("\n" + "="*60)
        print(report)
        print("="*60)
        
        # 推送报告
        if push and (self.issues or self.warnings):
            print("\n📱 推送健康报告...")
            if self.push_report(report):
                print("✅ 推送成功")
            else:
                print("❌ 推送失败")
        
        # 返回状态码
        return 0 if not self.issues else 1


def main():
    """主函数"""
    import argparse
    
    parser = argparse.ArgumentParser(description='恒生 Agent 健康检查')
    parser.add_argument('--no-push', action='store_true', help='不推送报告')
    
    args = parser.parse_args()
    
    checker = HealthChecker()
    exit_code = checker.run(push=not args.no_push)
    
    exit(exit_code)


if __name__ == "__main__":
    main()
