# 阿里云 OSS AccessKey 配置指南

**创建时间**: 2026-02-28  
**状态**: ⚠️ 需要配置真实密钥

---

## 🔐 当前配置状态

✅ 配置文件已创建:
- `local_config/aliyun_oss.conf`
- `.env`

⚠️ 需要替换真实密钥:
```bash
ALIYUN_ACCESS_KEY_ID = LTAI5tQwKzKzKzKzKzKzKzKz  # ✅ 已配置
ALIYUN_ACCESS_KEY_SECRET = your_access_key_secret_here  # ⚠️ 需要替换
```

---

## 📝 配置步骤

### Step 1: 获取阿里云 AccessKey

1. **登录阿里云控制台**
   ```
   https://ram.console.aliyun.com/manage/ak
   ```

2. **创建 AccessKey**（如果没有）
   - 点击 "创建 AccessKey"
   - 选择 "RAM 用户"（推荐）或 "阿里云账号"
   - 下载或复制 AccessKey ID 和 Secret

3. **保存密钥**
   - AccessKey ID: `LTAI5tQwKzKzKzKzKzKzKzKz` (已有)
   - AccessKey Secret: `xxxxxxxxxxxxxxxx` (需要填入)

---

### Step 2: 编辑配置文件

**方法 1: 使用 nano 编辑**
```bash
nano /root/.openclaw/workspace/Lamda-ai/HKTech-Agent/.env
```

替换这一行:
```bash
ALIYUN_ACCESS_KEY_SECRET=你的真实密钥
```

**方法 2: 使用 sed 替换**
```bash
sed -i 's/ALIYUN_ACCESS_KEY_SECRET=.*/ALIYUN_ACCESS_KEY_SECRET=你的真实密钥/' \
  /root/.openclaw/workspace/Lamda-ai/HKTech-Agent/.env
```

**方法 3: 使用 echo 追加**
```bash
cat > /root/.openclaw/workspace/Lamda-ai/HKTech-Agent/.env << EOF
ALIYUN_ACCESS_KEY_ID=LTAI5tQwKzKzKzKzKzKzKzKz
ALIYUN_ACCESS_KEY_SECRET=你的真实密钥
ALIYUN_OSS_ENDPOINT=oss-cn-beijing.aliyuncs.com
ALIYUN_OSS_BUCKET=hktech-agent-models
EOF
```

---

### Step 3: 加载环境变量

```bash
# 加载环境变量
source /root/.openclaw/workspace/Lamda-ai/HKTech-Agent/.env

# 验证配置
echo "AccessKey ID: ${ALIYUN_ACCESS_KEY_ID:0:12}..."
echo "OSS Bucket: $ALIYUN_OSS_BUCKET"
```

---

### Step 4: 测试 OSS 连接

```bash
cd /root/.openclaw/workspace/Lamda-ai/HKTech-Agent
python3 << 'PYEOF'
import oss2
import os

# 初始化 OSS
auth = oss2.Auth(
    os.getenv('ALIYUN_ACCESS_KEY_ID'),
    os.getenv('ALIYUN_ACCESS_KEY_SECRET')
)
bucket = oss2.Bucket(
    auth,
    os.getenv('ALIYUN_OSS_ENDPOINT'),
    os.getenv('ALIYUN_OSS_BUCKET')
)

# 测试连接
try:
    bucket.get_bucket_info()
    print("✅ OSS 连接成功！")
    print(f"Bucket: {bucket.bucket_name}")
    print(f"Endpoint: {bucket.endpoint}")
except Exception as e:
    print(f"❌ OSS 连接失败：{e}")
    print("请检查 AccessKey 配置是否正确")
PYEOF
```

---

### Step 5: 测试上传功能

```bash
python3 scripts/upload_learning_to_oss.py
```

预期输出:
```
✅ OSS 初始化成功：hktech-agent-models
✅ 报告已上传：learning_reports/2026-02-28/test_2026-02-28.md
============================================================
📊 上传结果
============================================================
本地路径：/tmp/learning_reports/2026-02-28/test_2026-02-28.md
OSS URL: https://...
```

---

## 🔒 安全提示

### 1. 文件权限

配置文件权限已设置为 600:
```bash
chmod 600 .env local_config/aliyun_oss.conf
```

### 2. Git 安全

确保 `.env` 已添加到 `.gitignore`:
```bash
echo ".env" >> .gitignore
git add .gitignore
git commit -m "Add .env to gitignore"
```

### 3. 密钥轮换

建议每 3-6 个月轮换一次 AccessKey:
1. 在阿里云控制台创建新 AccessKey
2. 更新配置文件
3. 删除旧 AccessKey

---

## ❓ 常见问题

### Q1: AccessKey 在哪里获取？

**A**: 阿里云 RAM 控制台
```
https://ram.console.aliyun.com/manage/ak
```

### Q2: 提示 "InvalidAccessKeyId"？

**A**: 检查 AccessKey ID 是否正确，注意大小写。

### Q3: 提示 "AccessDenied"？

**A**: 检查 RAM 用户权限，确保有 OSS 读写权限。

### Q4: 内网访问慢？

**A**: 使用内网 Endpoint:
```bash
ALIYUN_OSS_ENDPOINT=oss-cn-beijing-internal.aliyuncs.com
```

---

## 📞 需要帮助？

查看完整文档:
- [OSS 快速开始](OSS_QUICK_START.md)
- [OSS 配置指南](ALIYUN_OSS_GUIDE.md)
- [学习 OSS 指南](LEARNING_OSS_GUIDE.md)

---

**最后更新**: 2026-02-28  
**维护者**: Alex 🐾
