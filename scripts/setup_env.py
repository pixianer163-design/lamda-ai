#!/usr/bin/env python3
"""
环境配置脚本 - 设置API Keys
"""

import os

# DeepSeek API Key
DEEPSEEK_API_KEY = "sk-5ff142845d464bca9c382a4b4d37e803"

# 写入环境变量（当前会话）
os.environ["DEEPSEEK_API_KEY"] = DEEPSEEK_API_KEY

# 写入 .bashrc（持久化）
bashrc_path = os.path.expanduser("~/.bashrc")

# 检查是否已存在
with open(bashrc_path, "r") as f:
    content = f.read()

if "DEEPSEEK_API_KEY" not in content:
    with open(bashrc_path, "a") as f:
        f.write(f"\n# HK Tech Agent API Keys\n")
        f.write(f"export DEEPSEEK_API_KEY={DEEPSEEK_API_KEY}\n")
    print("✅ DEEPSEEK_API_KEY 已添加到 ~/.bashrc")
else:
    print("✅ DEEPSEEK_API_KEY 已存在于 ~/.bashrc")

print(f"✅ API Key 配置完成: {DEEPSEEK_API_KEY[:10]}...{DEEPSEEK_API_KEY[-4:]}")
