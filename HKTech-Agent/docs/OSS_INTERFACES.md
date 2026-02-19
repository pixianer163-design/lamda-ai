# 阿里云 OSS 接口大全

阿里云 OSS（Object Storage Service）提供的完整接口参考。

---

## 📚 核心接口分类

### 1. Bucket 操作接口

| 接口 | 说明 | Python SDK 方法 |
|------|------|-----------------|
| 创建 Bucket | 创建存储空间 | `create_bucket(bucket_name)` |
| 删除 Bucket | 删除存储空间 | `delete_bucket(bucket_name)` |
| 列出 Bucket | 列出所有存储空间 | `list_buckets()` |
| 获取 Bucket 信息 | 获取元数据 | `get_bucket_info()` |
| 设置 Bucket ACL | 设置访问权限 | `put_bucket_acl(acl)` |
| 获取 Bucket ACL | 获取访问权限 | `get_bucket_acl()` |

**示例**:
```python
import oss2

auth = oss2.Auth('access_key_id', 'access_key_secret')
bucket = oss2.Bucket(auth, 'oss-cn-beijing.aliyuncs.com', 'hktech-agent-models')

# 获取 Bucket 信息
info = bucket.get_bucket_info()
print(f"Bucket: {info.name}")
print(f"创建时间: {info.creation_date}")
print(f"存储类型: {info.storage_class}")
```

---

### 2. 对象上传接口

| 接口 | 说明 | 适用场景 |
|------|------|----------|
| `put_object` | 上传字符串/字节 | 小文件、内存数据 |
| `put_object_from_file` | 上传本地文件 | 大文件、模型文件 |
| `put_object_from_file_object` | 上传文件对象 | 流式上传 |
| `append_object` | 追加上传 | 日志追加 |
| `resumable_upload` | 断点续传 | 超大文件 (>100MB) |
| `upload_part` / `complete_multipart_upload` | 分片上传 | 大文件并行上传 |

**示例**:
```python
# 1. 上传字符串
bucket.put_object('test/hello.txt', b'Hello OSS')

# 2. 上传本地文件
bucket.put_object_from_file('models/rssm.pt', '/local/path/rssm.pt')

# 3. 断点续传（推荐用于大模型）
oss2.resumable_upload(
    bucket, 
    'models/large_model.pt',
    '/local/path/large_model.pt',
    store=oss2.ResumableStore(root='/tmp/.oss_checkpoint'),
    multipart_threshold=100*1024*1024,  # 100MB
    part_size=10*1024*1024              # 10MB每片
)
```

---

### 3. 对象下载接口

| 接口 | 说明 | 适用场景 |
|------|------|----------|
| `get_object` | 获取对象内容 | 小文件、内存处理 |
| `get_object_to_file` | 下载到本地文件 | 保存模型文件 |
| `get_object_with_url` | URL 签名访问 | 临时下载链接 |
| `resumable_download` | 断点续传下载 | 超大文件下载 |
| `select_object_content` | Select 查询 | CSV/JSON 数据分析 |

**示例**:
```python
# 1. 下载到内存
result = bucket.get_object('models/rssm.pt')
content = result.read()

# 2. 下载到本地文件
bucket.get_object_to_file('models/rssm.pt', '/local/rssm.pt')

# 3. 生成临时 URL（有效期1小时）
url = bucket.sign_url('GET', 'models/rssm.pt', 3600)
print(f"下载链接: {url}")

# 4. 断点续传下载
oss2.resumable_download(
    bucket,
    'models/large_model.pt',
    '/local/large_model.pt',
    store=oss2.ResumableStore(root='/tmp/.oss_checkpoint'),
    multiget_threshold=100*1024*1024,
    part_size=10*1024*1024
)
```

---

### 4. 对象管理接口

| 接口 | 说明 | Python SDK 方法 |
|------|------|-----------------|
| 列出对象 | 列出 Bucket 内对象 | `list_objects(prefix='')` |
| 删除对象 | 删除单个对象 | `delete_object(key)` |
| 批量删除 | 删除多个对象 | `batch_delete_objects(keys)` |
| 复制对象 | Bucket 内或跨 Bucket | `copy_object(dest_key, src_bucket, src_key)` |
| 移动/重命名 | 复制后删除 | `copy_object` + `delete_object` |
| 获取元数据 | 获取对象信息 | `head_object(key)` |
| 设置元数据 | 自定义元数据 | `put_object(key, data, headers={'x-oss-meta-xxx': 'value'})` |

**示例**:
```python
# 列出所有模型
for obj in oss2.ObjectIterator(bucket, prefix='models/'):
    print(f"{obj.key}: {obj.size} bytes, 修改时间: {obj.last_modified}")

# 获取对象元数据
meta = bucket.head_object('models/rssm.pt')
print(f"大小: {meta.content_length}")
print(f"类型: {meta.content_type}")
print(f"ETag: {meta.etag}")

# 复制对象
bucket.copy_object('models/rssm_backup.pt', 'hktech-agent-models', 'models/rssm.pt')

# 批量删除
keys_to_delete = ['models/old1.pt', 'models/old2.pt']
result = bucket.batch_delete_objects(keys_to_delete)
print(f"删除成功: {result.deleted_keys}")
```

---

### 5. 生命周期管理接口

| 接口 | 说明 | 适用场景 |
|------|------|----------|
| `put_bucket_lifecycle` | 设置生命周期规则 | 自动清理旧数据 |
| `get_bucket_lifecycle` | 获取生命周期规则 | 查看规则 |
| `delete_bucket_lifecycle` | 删除生命周期规则 | 取消自动清理 |

**示例**:
```python
from oss2.models import LifecycleExpiration, LifecycleRule, BucketLifecycle

# 自动删除30天前的临时文件
rule = LifecycleRule(
    'delete_temp',
    'test/',
    status='Enabled',
    expiration=LifecycleExpiration(days=30)
)

lifecycle = BucketLifecycle([rule])
bucket.put_bucket_lifecycle(lifecycle)
```

---

### 6. 访问控制接口 (ACL)

| 接口 | 说明 | 权限级别 |
|------|------|----------|
| `put_bucket_acl` | 设置 Bucket ACL | private/public-read/public-read-write |
| `get_bucket_acl` | 获取 Bucket ACL | - |
| `put_object_acl` | 设置对象 ACL | private/public-read |
| `get_object_acl` | 获取对象 ACL | - |

**示例**:
```python
# Bucket 设置为私有（最安全）
bucket.put_bucket_acl(oss2.BUCKET_ACL_PRIVATE)

# Bucket 设置为公共读（允许匿名下载）
bucket.put_bucket_acl(oss2.BUCKET_ACL_PUBLIC_READ)

# 检查 ACL
acl = bucket.get_bucket_acl()
print(f"当前 ACL: {acl.acl}")
```

---

### 7. 跨域配置接口 (CORS)

| 接口 | 说明 |
|------|------|
| `put_bucket_cors` | 设置跨域规则 |
| `get_bucket_cors` | 获取跨域规则 |
| `delete_bucket_cors` | 删除跨域规则 |

**示例**:
```python
from oss2.models import BucketCors, CorsRule

# 允许 Web 前端直接访问
rule = CorsRule(
    allowed_origins=['https://your-website.com'],
    allowed_methods=['GET', 'POST'],
    allowed_headers=['*'],
    max_age_seconds=3000
)

cors = BucketCors([rule])
bucket.put_bucket_cors(cors)
```

---

### 8. 图片处理接口 (IMG)

| 接口 | 说明 |
|------|------|
| `get_object` + 处理参数 | 图片缩放/裁剪/水印 |

**示例**:
```python
# 图片缩放 (OSS 图片处理)
style = 'image/resize,m_fixed,w_100,h_100'
result = bucket.get_object('images/photo.jpg', process=style)
```

---

### 9. 数据安全接口

| 接口 | 说明 |
|------|------|
| 服务端加密 (SSE) | 上传时自动加密 |
| 客户端加密 | 上传前本地加密 |
| 防盗链 (Referer) | 防止恶意盗用 |
| 访问日志 | 记录访问记录 |

**示例**:
```python
# 服务端加密上传
headers = {'x-oss-server-side-encryption': 'AES256'}
bucket.put_object('secret.txt', b'secret data', headers=headers)
```

---

### 10. 监控与日志接口

| 接口 | 说明 |
|------|------|
| `put_bucket_logging` | 设置访问日志 |
| `get_bucket_logging` | 获取日志配置 |
| `put_bucket_website` | 静态网站托管 |
| `get_bucket_website` | 获取网站配置 |

---

## 🎯 实际应用场景

### 场景1: 模型版本管理

```python
from datetime import datetime
import shutil

def save_model_version(model, version_name):
    """保存模型版本到 OSS"""
    # 本地保存
    local_path = f'/tmp/{version_name}.pt'
    torch.save(model.state_dict(), local_path)
    
    # 上传到 OSS (带时间戳)
    timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
    remote_name = f'models/v2/{timestamp}_{version_name}.pt'
    
    oss.bucket.put_object_from_file(remote_name, local_path)
    
    # 清理本地
    os.unlink(local_path)
    
    return f'oss://{oss.bucket_name}/{remote_name}'
```

### 场景2: 训练数据自动备份

```python
def backup_training_data():
    """自动备份训练数据到 OSS"""
    data_dir = '/opt/hktech-agent/data'
    
    for filename in os.listdir(data_dir):
        if filename.endswith('.npy'):
            local_path = os.path.join(data_dir, filename)
            remote_path = f'training-data/{datetime.now().strftime("%Y/%m")}/{filename}'
            
            oss.bucket.put_object_from_file(remote_path, local_path)
            print(f'✅ 已备份: {filename}')
```

### 场景3: 模型自动同步

```python
def sync_model_to_local(model_name):
    """同步远程模型到本地"""
    local_path = f'/opt/hktech-agent/models/{model_name}'
    remote_path = f'models/{model_name}'
    
    # 检查远程是否存在
    try:
        meta = oss.bucket.head_object(remote_path)
        print(f'📦 远程模型: {meta.content_length / 1024 / 1024:.2f} MB')
        
        # 检查本地是否需要更新
        if not os.path.exists(local_path):
            need_download = True
        else:
            local_size = os.path.getsize(local_path)
            need_download = local_size != meta.content_length
        
        if need_download:
            print('⬇️  下载中...')
            oss.bucket.get_object_to_file(remote_path, local_path)
            print('✅ 同步完成')
        else:
            print('✅ 本地已是最新')
            
    except oss2.exceptions.NoSuchKey:
        print(f'❌ 远程模型不存在: {model_name}')
```

---

## 📊 性能与限制

| 项目 | 限制 | 说明 |
|------|------|------|
| 单个文件大小 | 最大 48.8 TB | 实际上传限制取决于方法 |
| 单次上传 | 最大 5 GB | 超过需用分片上传 |
| Bucket 数量 | 每个账号 100 个 | 可申请增加 |
| 对象数量 | 无限制 | - |
| 请求频率 | 无限制 | - |
| 带宽 | 无限制 | - |

---

## 🔗 相关文档

- [阿里云 OSS Python SDK 文档](https://help.aliyun.com/document_detail/32026.html)
- [OSS API 参考](https://help.aliyun.com/document_detail/31947.html)
- [OSS 最佳实践](https://help.aliyun.com/document_detail/32021.html)

---

**提示**: 本项目的 OSS 封装见 `oss_manager.py`，提供了更简洁的上层接口。
