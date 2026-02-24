# OSS 训练数据与模型自动上传 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 训练结束后自动把模型文件和训练数据上传到阿里云OSS，凭证从 `AccessKey.csv` 读取。

**Architecture:** `oss_manager.py` 新增 CSV 凭证读取方法；`train_world_model.py` 训练成功后调用 `OSSManager`；`upload_to_oss.py` 修复路径和 endpoint 以保留手动上传能力。

**Tech Stack:** Python 3, oss2 (aliyun-oss-python-sdk), pandas, argparse

---

## Context

- AccessKey.csv 位置: `/home/huawei/project_opencode/lamda-ai/AccessKey.csv`
- AccessKey CSV 格式: `AccessKey ID,AccessKey Secret` header + 一行数据
- 训练输出:
  - `HKTech-Agent/data/models/rssm_model.pt`
  - `HKTech-Agent/data/models/scaler.pkl`
  - `HKTech-Agent/data/training_episodes.json`
- OSS 配置（公网，因为是WSL2本地机器，非ECS）:
  - Endpoint: `oss-cn-beijing.aliyuncs.com`
  - 模型 Bucket: `hktech-agent-models`，路径前缀: `models/`
  - 数据 Bucket: `cloud-training`，路径前缀: `training-data/`
- 现有代码: `active_src/oss_manager.py` 已实现 `upload_model()` / `upload_training_data()`，只支持 env var 和 `.conf` 文件，需要增加 CSV 支持
- oss2 当前未安装，需要 `pip install oss2`

---

### Task 1: 给 oss_manager.py 增加 CSV 凭证读取

**Files:**
- Modify: `HKTech-Agent/active_src/oss_manager.py`

**Step 1: 修改 `__init__` 接受 `csv_path` 参数**

在 `OSSManager.__init__` 签名改为:
```python
def __init__(self, config_path: Optional[str] = None, csv_path: Optional[str] = None):
```

**Step 2: 在 `_load_from_env()` 之后增加 CSV 加载分支**

```python
# 尝试从CSV文件加载（优先级高于config_path）
if not self.access_key_id and csv_path:
    self._load_from_csv(csv_path)
```

**Step 3: 新增 `_load_from_csv()` 方法**

```python
def _load_from_csv(self, csv_path: str):
    """从 AccessKey.csv 文件加载凭证（格式: AccessKey ID,AccessKey Secret）"""
    import csv
    try:
        with open(csv_path, 'r', encoding='utf-8') as f:
            reader = csv.DictReader(f)
            for row in reader:
                key_id = row.get('AccessKey ID', '').strip()
                key_secret = row.get('AccessKey Secret', '').strip()
                if key_id and key_secret:
                    self.access_key_id = key_id
                    self.access_key_secret = key_secret
                    logger.info(f"✅ 从CSV文件加载OSS凭证: {csv_path}")
                    return
        logger.warning(f"⚠️ CSV文件中未找到有效凭证: {csv_path}")
    except FileNotFoundError:
        logger.warning(f"⚠️ AccessKey CSV文件不存在: {csv_path}")
    except Exception as e:
        logger.error(f"❌ 读取CSV凭证失败: {e}")
```

**Step 4: 修改 `_validate_config()` 中 endpoint 默认值**

确保 `_load_from_env()` 中 endpoint 的默认值为公网Beijing:
```python
self.endpoint = os.getenv('ALIYUN_OSS_ENDPOINT', 'oss-cn-beijing.aliyuncs.com')
```

**Step 5: 修改 `__init__` 中的验证逻辑**

```python
# 加载顺序: env var → csv → config file
self._load_from_env()
if not self.access_key_id and csv_path:
    self._load_from_csv(csv_path)
if not self.access_key_id and config_path:
    self._load_from_config(config_path)
```

**Step 6: 运行现有测试确保不破坏**

```bash
cd HKTech-Agent && python3 -m pytest tests/unit/test_shared_coverage.py -v -k "strategy" 2>&1 | tail -5
```

**Step 7: Commit**

```bash
git add HKTech-Agent/active_src/oss_manager.py
git commit -m "feat(oss): add CSV credential loading to OSSManager"
```

---

### Task 2: train_world_model.py 训练后自动上传

**Files:**
- Modify: `HKTech-Agent/prod/src/train_world_model.py`

**Step 1: 在文件末尾 `if __name__ == '__main__':` 块中，`model_path` 赋值后添加上传逻辑**

找到如下位置（`train_world_model.py` 约第 200 行的 `__main__` 块末尾）:
```python
model_path = train_gru_model(X, y, data_dir)
```

在其后添加:
```python
# ── 训练成功后自动上传到OSS ──────────────────────────────────────────────
if model_path:
    _upload_to_oss_after_training(data_dir, model_path)
```

**Step 2: 在 `__main__` 块之前添加 `_upload_to_oss_after_training()` 函数**

```python
def _upload_to_oss_after_training(data_dir: str, model_path: str):
    """训练完成后将模型和训练数据上传到阿里云OSS。oss2未安装时静默跳过。"""
    # 查找 AccessKey.csv（相对于本文件向上两级，即 lamda-ai/）
    _script_dir = os.path.dirname(os.path.abspath(__file__))
    csv_path = os.path.normpath(os.path.join(_script_dir, "../../../AccessKey.csv"))

    try:
        import sys as _sys
        _active_src = os.path.join(_script_dir, "../../active_src")
        _sys.path.insert(0, _active_src)
        from oss_manager import OSSManager
    except ImportError:
        print("⚠️  oss_manager 未找到，跳过OSS上传")
        return

    try:
        oss = OSSManager(csv_path=csv_path)
    except ImportError:
        print("⚠️  oss2 未安装，跳过OSS上传。安装: pip install oss2")
        return
    except ValueError as e:
        print(f"⚠️  OSS配置不完整，跳过上传: {e}")
        return
    except Exception as e:
        print(f"⚠️  OSS初始化失败，跳过上传: {e}")
        return

    # 上传模型文件
    models_dir = os.path.join(data_dir, "models")
    for fname in ("rssm_model.pt", "scaler.pkl"):
        fpath = os.path.join(models_dir, fname)
        if os.path.exists(fpath):
            try:
                oss.upload_model(fpath, fname)
            except Exception as e:
                print(f"⚠️  上传 {fname} 失败: {e}")

    # 上传训练数据
    episodes_path = os.path.join(data_dir, "training_episodes.json")
    if os.path.exists(episodes_path):
        try:
            oss.upload_training_data(episodes_path, "training_episodes.json")
        except Exception as e:
            print(f"⚠️  上传 training_episodes.json 失败: {e}")

    print("✅ OSS上传完成")
```

**Step 3: 验证语法**

```bash
cd HKTech-Agent && python3 -c "import prod.src.train_world_model" 2>&1 | head -5
# 或:
python3 -c "
import sys; sys.path.insert(0, 'prod/src')
import ast, open as _open
ast.parse(open('prod/src/train_world_model.py').read())
print('syntax OK')
"
```

**Step 4: Commit**

```bash
git add HKTech-Agent/prod/src/train_world_model.py
git commit -m "feat(train): auto-upload model and training data to OSS after training"
```

---

### Task 3: 修复 upload_to_oss.py 的路径和 endpoint

**Files:**
- Modify: `HKTech-Agent/upload_to_oss.py`

**Step 1: 修复 `ACCESS_KEY_FILE` 路径**

将:
```python
ACCESS_KEY_FILE = "/mnt/d/project/AccessKey.csv"
```
改为:
```python
# AccessKey.csv 位于项目根目录 (lamda-ai/)
ACCESS_KEY_FILE = os.path.join(os.path.dirname(os.path.abspath(__file__)), "../AccessKey.csv")
```
这样不管从哪里运行脚本都能找到文件。

**Step 2: 修复 endpoint 默认值**

将:
```python
OSS_ENDPOINT = os.environ.get("OSS_ENDPOINT", "oss-cn-hangzhou.aliyuncs.com")
```
改为:
```python
OSS_ENDPOINT = os.environ.get("OSS_ENDPOINT", "oss-cn-beijing.aliyuncs.com")
```

**Step 3: 验证**

```bash
cd HKTech-Agent && python3 upload_to_oss.py --test 2>&1 | head -10
```

期望输出: `✅ AccessKey加载成功` 或具体的连接错误（而非"读取AccessKey文件失败"）

**Step 4: Commit**

```bash
git add HKTech-Agent/upload_to_oss.py
git commit -m "fix(upload): correct AccessKey.csv path and OSS endpoint"
```

---

### Task 4: 安装 oss2 并端到端验证

**Step 1: 安装 oss2**

```bash
cd HKTech-Agent && source venv/bin/activate && pip install oss2
```

**Step 2: 测试 OSS 连接**

```bash
python3 upload_to_oss.py --test
```
期望: `✅ OSS连接成功!` 且显示 bucket 信息

**Step 3: 测试 OSSManager CSV 读取**

```python
import sys; sys.path.insert(0, 'active_src')
from oss_manager import OSSManager
oss = OSSManager(csv_path="../AccessKey.csv")
models = oss.list_models()
print(f"远程模型数: {len(models)}")
```

**Step 4: 触发一次小训练并验证上传**

```bash
# 直接运行训练脚本（会用 mock 数据，快速完成）
python3 prod/src/train_world_model.py 2>&1 | tail -15
```
期望末尾出现:
```
✅ 模型上传成功: ...
✅ 训练数据上传成功: ...
✅ OSS上传完成
```

**Step 5: 更新 requirements.txt**

```bash
echo "oss2>=2.18.0" >> HKTech-Agent/requirements.txt
```

**Step 6: Commit**

```bash
git add HKTech-Agent/requirements.txt
git commit -m "chore: add oss2 dependency and verify end-to-end OSS upload"
git push origin main
```
