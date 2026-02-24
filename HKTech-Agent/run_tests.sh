#!/bin/bash
# HKTech-Agent 测试运行脚本

set -e

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd $DIR

echo "=========================================="
echo "🧪 HKTech-Agent 测试套件"
echo "=========================================="
echo "时间: $(date)"
echo "Python版本: $(python3 --version)"
echo "工作目录: $DIR"
echo "=========================================="

# 创建测试目录
mkdir -p tests/logs
mkdir -p tests/fixtures

# 设置测试环境变量
export AGENT_ENV="test"
export AGENT_LOG_DIR="$DIR/tests/logs"
export AGENT_DATA_DIR="$DIR/tests/fixtures"
export PYTHONPATH="$DIR/shared:$DIR/prod/src:$DIR/active_src:$PYTHONPATH"

echo "📁 测试目录:"
echo "   日志: $AGENT_LOG_DIR"
echo "   数据: $AGENT_DATA_DIR"
echo "=========================================="

# 检查pytest是否安装
if ! python3 -c "import pytest" 2>/dev/null; then
    echo "⚠️  pytest未安装，正在安装..."
    pip3 install pytest pytest-timeout
fi

# 检查psutil是否安装（健康检查需要）
if ! python3 -c "import psutil" 2>/dev/null; then
    echo "⚠️  psutil未安装，正在安装..."
    pip3 install psutil
fi

echo "🔍 运行测试..."
echo "=========================================="

# 运行测试
python3 -m pytest \
    tests/ \
    -v \
    --tb=short \
    --strict-markers \
    -p no:warnings \
    --timeout=300 \
    --junitxml=tests/test-results.xml \
    "$@"

TEST_EXIT_CODE=$?

echo "=========================================="
echo "📊 测试完成"
echo "退出代码: $TEST_EXIT_CODE"

if [ $TEST_EXIT_CODE -eq 0 ]; then
    echo "✅ 所有测试通过!"
else
    echo "❌ 测试失败"
fi

echo "=========================================="

# 生成测试报告摘要
if [ -f "tests/test-results.xml" ]; then
    echo "📋 测试报告摘要:"
    python3 -c "
import xml.etree.ElementTree as ET
import sys

try:
    tree = ET.parse('tests/test-results.xml')
    root = tree.getroot()
    
    total = int(root.attrib.get('tests', 0))
    failures = int(root.attrib.get('failures', 0))
    errors = int(root.attrib.get('errors', 0))
    skipped = int(root.attrib.get('skipped', 0))
    passed = total - failures - errors - skipped
    
    print(f'   总测试数: {total}')
    print(f'   通过: {passed}')
    print(f'   失败: {failures}')
    print(f'   错误: {errors}')
    print(f'   跳过: {skipped}')
    print(f'   通过率: {passed/max(total,1)*100:.1f}%')
except Exception as e:
    print(f'   无法解析测试报告: {e}')
"
fi

echo "=========================================="
exit $TEST_EXIT_CODE