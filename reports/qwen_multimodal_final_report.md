# 🎯 qwen3.5-plus 多模态测试最终报告

**测试时间**: 2026-02-26 21:45  
**测试者**: 阿莱士/Alex  
**测试平台**: 阿里云百炼 Coding Plan

---

## 📊 测试结果汇总

| 测试项目 | 模型 | 端点 | 结果 |
|----------|------|------|------|
| 纯文本对话 | qwen3.5-plus | coding | ✅ **成功** |
| 股票分析 | qwen3.5-plus | coding | ✅ **成功** |
| 多模态 (URL) | qwen3.5-plus | coding | ❌ 图片下载失败 |
| 多模态 (Base64) | qwen3.5-plus | coding | ⚠️ 图片尺寸限制 |
| 多模态 | qwen-vl-max | coding | ❌ 模型不支持 |

---

## 🔍 核心发现

### ✅ 确认的功能

1. **qwen3.5-plus 纯文本能力**
   - API 调用正常
   - 响应速度快（<3 秒）
   - 股票分析专业准确

2. **官网信息**
   - 阿里云百炼官网明确标注 qwen3.5-plus 支持多模态
   - 输入支持：`["text", "image"]`
   - 多模态能力"相比 Qwen3 VL 系列有显著提升"

### ❌ 遇到的问题

1. **Coding Plan 端点限制**
   - `coding.dashscope.aliyuncs.com` 可能仅支持纯文本
   - 多模态需要使用 `dashscope.aliyuncs.com` 端点

2. **图片访问问题**
   - 需要公网可访问的图片 URL
   - Base64 编码有尺寸限制

---

## 💡 解决方案

### 方案 A: 使用 DashScope 原生端点（推荐）

**配置**:
```python
client = OpenAI(
    api_key='sk-xxx',  # 需要 DashScope API Key
    base_url='https://dashscope.aliyuncs.com/compatible-mode/v1'
)
```

**优点**:
- 完整支持多模态
- 模型选择更多

**缺点**:
- 需要单独的 DashScope API Key

### 方案 B: 继续使用纯文本分析

**说明**:
- qwen3.5-plus 纯文本能力完全可用
- K 线图等技术指标通过数值提供
- 不影响核心功能

---

## 🎯 对恒生 Agent 的建议

### 立即可用（无需修改）

| 功能 | 状态 |
|------|------|
| 市场数据分析 | ✅ 正常 |
| 新闻情绪分析 | ✅ 正常 |
| 决策增强 | ✅ 正常 |
| 飞书推送 | ✅ 正常 |

### 未来升级（可选）

| 功能 | 需求 | 成本 |
|------|------|------|
| K 线图识别 | DashScope API Key | ¥10-20/月 |
| 财报 OCR | DashScope API Key | ¥10-20/月 |
| 研报解析 | DashScope API Key | ¥10-20/月 |

---

## 📝 结论

**qwen3.5-plus 在 Coding Plan**:
- ✅ **纯文本能力：完全可用，推荐使用**
- ⚠️ **多模态能力：当前端点可能不支持**

**恒生 Agent 当前状态**:
- ✅ 核心功能不受影响
- ✅ 文本分析能力正常
- ⏸️ 多模态功能暂不配置（可按需添加）

---

## 🔗 参考链接

- [阿里云百炼官网 - qwen3.5-plus](https://help.aliyun.com/zh/model-studio/qwen3-5-plus)
- [多模态 API 参考](https://help.aliyun.com/zh/model-studio/vision)
- [DashScope 文档](https://help.aliyun.com/zh/dashscope/)

---

**报告者**: 阿莱士/Alex 🐾  
**日期**: 2026-02-26 21:45
