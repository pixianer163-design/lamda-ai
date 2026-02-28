# OSS Bucket 配置更新

**更新时间**: 2026-02-28  
**变更内容**: 知识库使用独立 Bucket

---

## 📊 OSS Bucket 分配

| 用途 | Bucket | 路径 | 说明 |
|------|--------|------|------|
| **知识库** | `knowledge-base` | `learning-reports/` | ⭐ 学习报告存储 |
| 模型文件 | `hktech-agent-models` | `models/` | 模型文件存储 |
| 训练数据 | `cloud-training` | `training-data/` | 训练数据存储 |

---

## 🔧 配置更新

### 1. 环境变量 (.env)

```bash
# 知识库 Bucket ⭐
ALIYUN_KNOWLEDGE_BUCKET=knowledge-base

# 模型 Bucket
ALIYUN_OSS_BUCKET=hktech-agent-models

# 训练数据 Bucket
ALIYUN_DATA_BUCKET=cloud-training
```

### 2. 配置文件 (local_config/aliyun_oss.conf)

```ini
[knowledge]
# 知识库存储 Bucket ⭐
knowledge_bucket = knowledge-base
knowledge_prefix = learning-reports/

[models]
# 模型存储 Bucket
oss_bucket = hktech-agent-models
models_prefix = models/

[data]
# 训练数据存储
data_bucket = cloud-training
data_prefix = training-data/
```

### 3. 代码更新

**upload_learning_to_oss.py**:
```python
# 知识库使用独立的 Bucket
self.bucket_name = os.getenv('ALIYUN_KNOWLEDGE_BUCKET', 'knowledge-base')
```

**knowledge_base/builder.py**:
```python
# 知识库使用独立的 Bucket
self.bucket_name = os.getenv('ALIYUN_KNOWLEDGE_BUCKET', 'knowledge-base')
```

---

## 📁 OSS 存储结构

```
oss://knowledge-base/
└── learning-reports/
    ├── 2026-02/
    │   ├── 28/
    │   │   ├── pre_market.md
    │   │   ├── noon.md
    │   │   ├── after_market.md
    │   │   └── summary.json
    │   └── ...
    └── 2026-03/
        └── ...

oss://hktech-agent-models/
└── models/
    ├── rssm_world_model_v2.h5
    └── ...

oss://cloud-training/
└── training-data/
    └── ...
```

---

## ✅ 优势

1. **数据隔离**: 知识库、模型、训练数据分别存储
2. **权限管理**: 可以为不同 Bucket 设置不同权限
3. **成本控制**: 便于统计各用途的存储和流量费用
4. **备份策略**: 可以为不同 Bucket 设置不同备份策略

---

## 🔒 安全提示

**配置文件保护**:
- `.env` 和 `local_config/` 已添加到 `.gitignore`
- 文件权限设置为 600（仅所有者可读写）
- 不要将真实密钥提交到 Git

**AccessKey 安全**:
- 定期轮换 AccessKey（建议每 3-6 个月）
- 使用 RAM 子账号而非主账号
- 限制 Bucket 权限（最小权限原则）

---

**最后更新**: 2026-02-28  
**维护者**: Alex 🐾
