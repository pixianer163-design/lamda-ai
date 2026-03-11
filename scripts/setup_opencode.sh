#!/bin/bash
# setup_opencode.sh - 配置OpenCode使用DeepSeek

echo "🤖 配置OpenCode使用DeepSeek API..."

# 设置环境变量
export OPENAI_API_KEY=sk-5ff142845d464bca9c382a4b4d37e803
export OPENAI_BASE_URL=https://api.deepseek.com/v1
export OPENAI_MODEL=deepseek-chat

# 持久化到.bashrc
if ! grep -q "OPENAI_API_KEY" ~/.bashrc; then
    echo "" >> ~/.bashrc
    echo "# OpenCode配置" >> ~/.bashrc
    echo "export OPENAI_API_KEY=sk-5ff142845d464bca9c382a4b4d37e803" >> ~/.bashrc
    echo "export OPENAI_BASE_URL=https://api.deepseek.com/v1" >> ~/.bashrc
    echo "export OPENAI_MODEL=deepseek-chat" >> ~/.bashrc
    echo "✅ 配置已写入 ~/.bashrc"
else
    echo "✅ 配置已存在"
fi

echo ""
echo "📋 OpenCode 配置完成："
echo "   API: DeepSeek (deepseek-chat)"
echo "   Key: ${OPENAI_API_KEY:0:10}...${OPENAI_API_KEY: -4}"
echo ""
echo "💡 使用方法："
echo "   cd /opt/hktech-agent"
echo "   opencode"
echo ""
echo "   然后输入自然语言指令，如："
echo "   - '帮我写一个函数计算RSI指标'"
echo "   - '优化data_collector.py的性能'"
echo "   - '生成单元测试'"
