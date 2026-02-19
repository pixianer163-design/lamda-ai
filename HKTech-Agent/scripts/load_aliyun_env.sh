#!/bin/bash
# 加载阿里云 OSS 环境变量
# 用法: source load_aliyun_env.sh

CONFIG_FILE="/opt/hktech-agent/config/aliyun_oss.conf"

if [ ! -f "$CONFIG_FILE" ]; then
    echo "❌ 配置文件不存在: $CONFIG_FILE"
    return 1
fi

# 解析配置文件并导出环境变量
while IFS='=' read -r key value; do
    # 跳过注释和空行
    [[ "$key" =~ ^[[:space:]]*# ]] && continue
    [[ -z "$key" ]] && continue
    
    # 去除空格
    key=$(echo "$key" | xargs)
    value=$(echo "$value" | xargs)
    
    # 转换为大写并添加前缀
    env_key="ALIYUN_${key^^}"
    export "$env_key=$value"
done < "$CONFIG_FILE"

echo "✅ 阿里云 OSS 环境变量已加载"
echo "   Access Key ID: ${ALIYUN_ACCESS_KEY_ID:0:8}..."
echo "   Endpoint: $ALIYUN_OSS_ENDPOINT"
echo "   Bucket: $ALIYUN_OSS_BUCKET"
