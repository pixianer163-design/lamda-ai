# 阿里云 OSS 配置指南

用于存储训练数据和模型文件的云存储方案。

## ⚠️ 安全警告

**Access Key 是敏感信息！**

- ✅ 配置文件已添加到 `.gitignore`，不会被提交到 GitHub
- ✅ 建议使用环境变量而非硬编码
- ✅ 定期轮换 Access Key
- ✅ 限制 OSS Bucket 的权限（只读/只写）

---

## 📋 配置信息

### Access Key（已记录在本地）

**⚠️ 敏感信息已保护，不在本文档中显示**

| 项目 | 说明 |
|------|-----|
| Access Key ID | 见本地配置文件 |
| Access Key Secret | 见本地配置文件 |
| Endpoint | `oss-cn-beijing.aliyuncs.com` |
| Bucket | `hktech-agent-models` |

**存储位置**: `/opt/hktech-agent/config/aliyun_oss.conf`（已添加到 `.gitignore`）

---

## 🚀 快速开始

### 1. 安装依赖

```bash
pip install oss2
```

### 2. 加载环境变量

```bash
# 方法1: 使用脚本
source /opt/hktech-agent/scripts/load_aliyun_env.sh

# 方法2: 手动导出（从配置文件获取真实值）
export ALIYUN_ACCESS_KEY_ID=your_access_key_id
export ALIYUN_ACCESS_KEY_SECRET=your_access_key_secret
export ALIYUN_OSS_ENDPOINT=oss-cn-beijing.aliyuncs.com
export ALIYUN_OSS_BUCKET=hktech-agent-models
```

### 3. 使用 OSS 管理器

```python
from oss_manager import OSSManager

# 初始化（自动从环境变量读取配置）
oss = OSSManager()

# 上传模型
url = oss.upload_model('/opt/hktech-agent/models/rssm_model.pt')
print(f"模型已上传: {url}")

# 下载模型
local_path = oss.download_model('rssm_model.pt', '/local/path/')
print(f"模型已下载: {local_path}")

# 列出所有模型
models = oss.list_models()
for model in models:
    print(f"{model['name']}: {model['size'] / 1024 / 1024:.2f} MB")
```

---

## 📁 存储结构

```
oss://hktech-agent-models/
├── models/
│   ├── rssm_model.pt
│   ├── rssm_model_v2.pt
│   └── actor_critic.pt
├── training-data/
│   ├── episodes_2024_01.npy
│   ├── episodes_2024_02.npy
│   └── market_data_2024.csv
└── backups/
    └── rssm_model_2024_01_15.pt
```

---

## 🔧 使用场景

### 场景1: 训练后上传模型

```python
from rssm_world_model import WorldModelTrainer
from oss_manager import OSSManager

# 训练模型
trainer = WorldModelTrainer()
trainer.train_world_model(episodes, epochs=50)
trainer.save()

# 上传到 OSS
oss = OSSManager()
url = oss.upload_model('/opt/hktech-agent/data/rssm_model.pt')
print(f"✅ 模型已上传到: {url}")
```

### 场景2: 下载历史模型进行回测

```python
from oss_manager import OSSManager

oss = OSSManager()

# 下载特定日期训练的模型
model_path = oss.download_model(
    'rssm_model_2024_02_01.pt',
    local_dir='/opt/hktech-agent/backtest_models/'
)

# 加载并回测
trainer.load(model_path)
# ... 回测代码 ...
```

### 场景3: 备份训练数据

```python
import numpy as np
from oss_manager import OSSManager

# 保存训练数据
np.save('/opt/hktech-agent/data/episodes_batch_001.npy', episodes)

# 上传到 OSS
oss = OSSManager()
url = oss.upload_training_data('episodes_batch_001.npy')
print(f"✅ 训练数据已备份: {url}")
```

---

## 🔐 安全最佳实践

### 1. 配置文件权限

```bash
# 限制配置文件的读取权限
chmod 600 /opt/hktech-agent/config/aliyun_oss.conf

# 确保只有所有者可以读取
ls -la /opt/hktech-agent/config/aliyun_oss.conf
# 输出: -rw------- 1 root root ... aliyun_oss.conf
```

### 2. 使用环境变量（推荐）

```bash
# 在生产环境中，使用环境变量而非配置文件
export ALIYUN_ACCESS_KEY_ID=your_key_id
export ALIYUN_ACCESS_KEY_SECRET=your_secret

# 然后在代码中
oss = OSSManager()  # 自动从环境变量读取
```

### 3. 定期轮换密钥

```bash
# 1. 在阿里云控制台创建新的 Access Key
# 2. 更新本地配置
# 3. 测试新密钥
# 4. 删除旧密钥
```

### 4. 最小权限原则

在阿里云 OSS 中配置 Bucket 策略：
- 训练节点: 只写权限（上传模型）
- 推理节点: 只读权限（下载模型）
- 管理节点: 读写权限

---

## 🐛 故障排除

### 问题1: AccessDenied

```
oss2.exceptions.AccessDenied: {'status': 403, 'code': 'AccessDenied'}
```

**解决**:
1. 检查 Access Key 是否正确
2. 检查 Bucket 权限设置
3. 检查是否开启 RAM 权限控制

### 问题2: NoSuchBucket

```
oss2.exceptions.NoSuchBucket: {'status': 404, 'code': 'NoSuchBucket'}
```

**解决**:
```bash
# 在阿里云控制台创建 Bucket
# 或使用命令行工具
ossutil mb oss://hktech-agent-models
```

### 问题3: 连接超时

```
ConnectTimeoutError: 
```

**解决**:
- 检查网络连接
- 确认 Endpoint 正确（如 `oss-cn-beijing.aliyuncs.com`）
- 检查防火墙设置

---

## 📊 成本估算

| 项目 | 费用 | 说明 |
|------|------|------|
| 存储 | ¥0.12/GB/月 | 标准存储 |
| 外网流出流量 | ¥0.80/GB | 下载到本地 |
| API 请求 | ¥0.01/万次 | PUT/GET 请求 |

**示例**:
- 存储 10 个模型（每个 11MB）: ¥0.013/月
- 每月下载 100 次: ¥0.88
- **总计: ¥0.9/月**

---

## 🔗 相关链接

- [阿里云 OSS Python SDK 文档](https://help.aliyun.com/document_detail/32026.html)
- [OSS 控制台](https://oss.console.aliyun.com/)
- [Access Key 管理](https://ram.console.aliyun.com/manage/ak)

---

**重要提醒**: Access Key 已记录在本地 `/opt/hktech-agent/config/aliyun_oss.conf`，**请勿提交到 GitHub！** 🔒
