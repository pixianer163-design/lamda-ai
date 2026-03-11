#!/bin/bash
# 加载 OSS 环境变量

CONFIG_FILE="/opt/hktech-agent/config/aliyun_config.env"

if [ -f "$CONFIG_FILE" ]; then
    echo "📥 加载 OSS 配置：$CONFIG_FILE"
    export $(grep -v '^#' "$CONFIG_FILE" | xargs)
    echo "✅ OSS 环境变量已加载"
else
    echo "⚠️  配置文件不存在：$CONFIG_FILE"
    exit 1
fi
