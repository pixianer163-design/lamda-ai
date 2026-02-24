# OSS 快速配置指南

**目标**: 5 分钟内完成 OSS 配置并验证

---

## 🚀 快速步骤

### 1. 获取 AccessKey（2 分钟）

1. 访问：https://ram.console.aliyun.com/manage/ak
2. 登录阿里云账号
3. 创建 AccessKey（或复制现有的）
4. 保存 AccessKey ID 和 Secret

### 2. 配置凭证（1 分钟）

```bash
nano /opt/hktech-agent/config/aliyun_config.env
```

替换以下内容：
```bash
ALIYUN_ACCESS_KEY_ID=你的真实 AccessKey ID
ALIYUN_ACCESS_KEY_SECRET=你的真实 AccessKey Secret
```

保存退出（Ctrl+O, Enter, Ctrl+X）

### 3. 验证配置（1 分钟）

```bash
cd /opt/hktech-agent
source venv/bin/activate
python3 -c "
from oss_manager import OSSManager
try:
    oss = OSSManager()
    print('✅ OSS 配置成功！')
    print(f'   Bucket: {oss.bucket_name}')
except Exception as e:
    print(f'❌ 配置失败：{e}')
"
```

### 4. 测试下载（1 分钟）

```bash
python3 -c "
from oss_manager import OSSManager
oss = OSSManager()
try:
    path = oss.download_model('rssm_model.pt', '/opt/hktech-agent/data')
    print('✅ 模型下载成功！')
    print(f'   路径：{path}')
    import os
    size = os.path.getsize(path)
    print(f'   大小：{size/1024:.1f} KB')
except Exception as e:
    print(f'⚠️ 下载失败：{e}')
    print('   将使用本地缓存模型')
"
```

---

## ✅ 验证清单

- [ ] OSS 配置文件已编辑
- [ ] AccessKey 已替换为真实值
- [ ] 验证命令输出 "✅ OSS 配置成功"
- [ ] 模型下载成功（或确认使用本地缓存）

---

## 🆘 常见问题

### Q1: 找不到 AccessKey
**A**: 联系阿里云账号管理员，或在 RAM 控制台创建新的 AccessKey

### Q2: 提示 "AccessDenied"
**A**: 检查 AccessKey 是否正确，确认 OSS Bucket 权限

### Q3: 下载失败但配置正确
**A**: 检查网络连接，确认 OSS Endpoint 正确（oss-cn-beijing.aliyuncs.com）

---

## 📞 需要帮助？

配置完成后，明天 9:25 AM 会自动同步，9:30 AM 使用最新模型运行。

如有问题，查看日志：
```bash
tail -f /opt/hktech-agent/logs/oss_sync.log
```

