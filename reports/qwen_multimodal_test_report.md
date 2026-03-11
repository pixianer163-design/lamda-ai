# 阿里云百炼 qwen3.5-plus 多模态测试报告

**测试时间**: 2026-02-26 21:30  
**测试模型**: qwen3.5-plus  
**API 端点**: https://coding.dashscope.aliyuncs.com/v1

---

## ✅ 测试结果

| 测试项 | 状态 | 说明 |
|--------|------|------|
| **纯文本对话** | ✅ 成功 | API 调用正常，响应快速 |
| **股票分析** | ✅ 成功 | 专业分析能力正常 |
| **多模态 (coding 端点)** | ❌ 失败 | 图片下载失败 |
| **多模态 (dashscope 端点)** | ❌ 失败 | API Key 不匹配 |

---

## 🔍 问题分析

### 问题 1: Coding Plan 端点不支持多模态

**错误信息**:
```
Failed to download multimodal content
```

**原因**: 
- `coding.dashscope.aliyuncs.com` 是百炼 Coding Plan 专用端点
- 该端点可能**仅支持纯文本**，不支持多模态输入

### 问题 2: 多模态需要不同的 API Key

**发现**:
- Coding Plan 使用的 API Key: `sk-sp-xxxxx`
- DashScope 多模态需要：单独的 DashScope API Key

---

## 📋 解决方案

### 方案 A: 使用 qwen-vl-max 进行多模态（推荐）

**优点**:
- qwen-vl-max 是阿里官方多模态专用模型
- 支持图像 + 文本联合理解
- 可通过相同 API 端点调用

**模型配置**:
```python
model = "qwen-vl-max-latest"
```

### 方案 B: 申请 DashScope 多模态 API Key

**步骤**:
1. 访问 https://dashscope.console.aliyun.com/
2. 创建 DashScope API Key
3. 使用该 Key 调用多模态 API

### 方案 C: 仅使用文本分析（当前方案）

**说明**:
- 继续使用 qwen3.5-plus 进行文本分析
- K 线图等技术指标通过数值计算提供
- 不依赖图像识别

---

## 💡 对恒生 Agent 的建议

### 立即可用（无需修改）

| 功能 | 模型 | 状态 |
|------|------|------|
| 市场数据分析 | qwen3.5-plus | ✅ 可用 |
| 新闻情绪分析 | qwen3.5-plus | ✅ 可用 |
| 决策增强 | qwen3.5-plus | ✅ 可用 |

### 需要额外配置

| 功能 | 模型 | 需求 |
|------|------|------|
| K 线图识别 | qwen-vl-max | 需要配置多模态 API |
| 财报 OCR | qwen-vl-max | 需要配置多模态 API |
| 研报解析 | qwen-vl-max | 需要配置多模态 API |

---

## 🎯 结论

**Coding Plan (qwen3.5-plus)**:
- ✅ 纯文本能力：**完全可用**
- ❌ 多模态能力：**当前端点不支持**

**建议**:
1. 当前继续使用 qwen3.5-plus 进行文本分析
2. 如需多模态，需单独配置 qwen-vl-max 或 DashScope API
3. 恒生 Agent 核心功能不受影响

---

**报告者**: 阿莱士/Alex 🐾
**日期**: 2026-02-26
