# OSS 模型同步配置

**配置日期**: 2026-02-24  
**状态**: ✅ 已配置

---

## 📦 架构设计

```
阿里云 OSS (hktech-agent-models)
         │
         │ 每日 9:25 自动同步
         ▼
本地缓存 (/opt/hktech-agent/data/rssm_model.pt)
         │
         │ 9:30 AM 运行时加载
         ▼
恒生 Agent (世界模型预测)
```

---

## 🔧 配置文件

### 1. OSS 凭证配置
**位置**: `/opt/hktech-agent/config/aliyun_config.env`

```bash
ALIYUN_ACCESS_KEY_ID=LTAI5tQwKzKzKzKzKzKzKzKz
ALIYUN_ACCESS_KEY_SECRET=your_access_key_secret_here
ALIYUN_OSS_ENDPOINT=oss-cn-beijing.aliyuncs.com
ALIYUN_OSS_BUCKET=hktech-agent-models
ALIYUN_DATA_BUCKET=cloud-training
ALIYUN_LOCAL_CACHE_DIR=/opt/hktech-agent/.oss_cache
```

### 2. 环境变量加载脚本
**位置**: `/opt/hktech-agent/scripts/load_oss_env.sh`

```bash
#!/bin/bash
CONFIG_FILE="/opt/hktech-agent/config/aliyun_config.env"
export $(grep -v '^#' "$CONFIG_FILE" | xargs)
```

### 3. 定时同步脚本
**位置**: `/opt/hktech-agent/cron/90_sync_oss_model.sh`

```bash
#!/bin/bash
# 每日 9:25 从 OSS 同步最新世界模型
source venv/bin/activate
python3 -c "from oss_manager import OSSManager; oss.download_model('rssm_model.pt', '/opt/hktech-agent/data')"
```

---

## ⏰ Cron 配置

### 已添加任务
```bash
# OSS 模型同步（工作日 9:25）
25 9 * * 1-5 /opt/hktech-agent/cron/90_sync_oss_model.sh >> /opt/hktech-agent/logs/oss_sync.log 2>&1
```

### 执行时间线
| 时间 | 任务 | 说明 |
|------|------|------|
| 9:25 AM | OSS 模型同步 | 下载最新世界模型 |
| 9:30 AM | 恒生 Agent 运行 | 使用最新模型预测 |

---

## 📝 代码集成

### llm_enhanced_agent.py
启动时自动从 OSS 下载最新模型：

```python
# 加载 OSS 配置
CONFIG_PATH = "/opt/hktech-agent/config/aliyun_config.env"
if os.path.exists(CONFIG_PATH):
    with open(CONFIG_PATH, 'r') as f:
        for line in f:
            key, value = line.split('=', 1)
            os.environ[key.strip()] = value.strip()

# 启动时同步
trainer = WorldModelTrainer(device='cpu')
trainer.load(force_download=True)
```

### rssm_world_model.py
支持强制从 OSS 下载：

```python
def load(self, force_download: bool = False):
    if force_download:
        from oss_manager import OSSManager
        oss = OSSManager()
        oss.download_model('rssm_model.pt', ...)
```

---

## 📊 日志位置

| 日志类型 | 文件路径 |
|---------|----------|
| OSS 同步日志 | `/opt/hktech-agent/logs/oss_sync.log` |
| Agent 运行日志 | `/opt/hktech-agent/prod/logs/run_*.log` |
| Cron 执行日志 | `/opt/hktech-agent/logs/cron/` |

---

## 🔍 监控命令

```bash
# 查看 OSS 同步状态
tail -f /opt/hktech-agent/logs/oss_sync.log

# 手动触发同步
/opt/hktech-agent/cron/90_sync_oss_model.sh

# 检查模型文件
ls -lh /opt/hktech-agent/data/rssm_model.pt

# 验证 OSS 配置
python3 -c "from oss_manager import OSSManager; oss = OSSManager(); print('✅ OSS 配置正常')"
```

---

## ⚠️ 故障处理

### 场景 1: OSS 配置缺失
**现象**: `OSS 配置不完整`  
**解决**: 
```bash
# 检查配置文件
cat /opt/hktech-agent/config/aliyun_config.env

# 重新加载环境变量
source /opt/hktech-agent/scripts/load_oss_env.sh
```

### 场景 2: OSS 下载失败
**现象**: `下载失败：AccessDenied`  
**解决**:
1. 检查 AccessKey 是否有效
2. 检查 Bucket 权限
3. 使用本地缓存模型（自动降级）

### 场景 3: 模型文件损坏
**现象**: `模型加载失败：KeyError`  
**解决**:
```bash
# 强制重新下载
rm /opt/hktech-agent/data/rssm_model.pt
/opt/hktech-agent/cron/90_sync_oss_model.sh
```

---

## 📋 配置清单

- [x] OSS 配置文件创建
- [x] 环境变量加载脚本
- [x] 定时同步脚本
- [x] crontab 配置
- [x] 代码集成（llm_enhanced_agent.py）
- [x] 代码集成（rssm_world_model.py）
- [x] 日志目录创建
- [ ] OSS 凭证验证（待老板提供真实密钥）

---

## 🔐 安全提示

1. **AccessKey 保密**: 不要提交到 Git
2. **最小权限原则**: OSS Bucket 只授予读写权限
3. **定期轮换**: 建议每 90 天更换 AccessKey

---

**配置者**: 阿莱士/Alex  
**最后更新**: 2026-02-24 11:30 AM
