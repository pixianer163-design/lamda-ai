# OSS Auto-Upload Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 训练结束后自动把模型文件和训练数据上传到阿里云OSS，凭证从 `AccessKey.csv` 读取。

**Architecture:** 给现有 `OSSManager` 新增 CSV 凭证读取；`train_world_model.py` 训练成功后调用 `OSSManager` 上传三个文件；修复 `upload_to_oss.py` 的 Windows 硬编码路径和错误 endpoint，保留手动上传能力。

**Tech Stack:** Python 3, oss2 (aliyun-oss-python-sdk), csv module

---

## Context (Read Before Starting)

- `AccessKey.csv` 位置: `/home/huawei/project_opencode/lamda-ai/AccessKey.csv`
  - CSV 格式: header `AccessKey ID,AccessKey Secret` + 一行数据
- 训练输出文件（训练后产生，需要上传）:
  - `HKTech-Agent/data/models/rssm_model.pt`
  - `HKTech-Agent/data/models/scaler.pkl`
  - `HKTech-Agent/data/training_episodes.json`
- OSS 配置（公网endpoint，本机是WSL2非ECS）:
  - Endpoint: `oss-cn-beijing.aliyuncs.com`
  - 模型 Bucket: `hktech-agent-models`，路径前缀: `models/`
  - 数据 Bucket: `cloud-training`，路径前缀: `training-data/`
- 现有代码:
  - `HKTech-Agent/active_src/oss_manager.py` — 完整的 `OSSManager` 类，已实现 `upload_model()` / `upload_training_data()`，但只支持 env var 和 `.conf` 文件读取凭证
  - `HKTech-Agent/upload_to_oss.py` — CLI 工具，有 Windows 硬编码路径和错误 endpoint
  - `HKTech-Agent/prod/src/train_world_model.py` — 训练脚本，`if __name__ == '__main__':` 块约在第 196 行
- `oss2` 当前**未安装**，需要先安装

---

### Task 1: 给 OSSManager 增加 CSV 凭证读取

**Files:**
- Modify: `HKTech-Agent/active_src/oss_manager.py`
- Test: `HKTech-Agent/tests/unit/test_oss_manager.py` (create)

**Step 1: 写失败测试**

创建 `HKTech-Agent/tests/unit/test_oss_manager.py`:

```python
"""
Tests for OSSManager CSV credential loading.
Does NOT require oss2 or network access.
"""
import sys
import csv
import pytest
import tempfile
import os
from pathlib import Path
from unittest.mock import patch, MagicMock

project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root / "active_src"))


def _make_csv(tmp_path, key_id="TESTID123", key_secret="TESTSECRET456"):
    """Helper: write a valid AccessKey.csv to tmp_path."""
    csv_file = tmp_path / "AccessKey.csv"
    with open(csv_file, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=["AccessKey ID", "AccessKey Secret"])
        writer.writeheader()
        writer.writerow({"AccessKey ID": key_id, "AccessKey Secret": key_secret})
    return str(csv_file)


class TestOSSManagerCSV:
    """OSSManager._load_from_csv() tests — no oss2 / network needed."""

    def test_load_from_csv_sets_credentials(self, tmp_path):
        csv_path = _make_csv(tmp_path)
        with patch("oss_manager.oss2") as mock_oss2:
            mock_oss2.Auth.return_value = MagicMock()
            mock_bucket = MagicMock()
            mock_oss2.Bucket.return_value = mock_bucket
            mock_bucket.get_bucket_info.return_value = MagicMock()

            from oss_manager import OSSManager
            mgr = OSSManager(csv_path=csv_path)

        assert mgr.access_key_id == "TESTID123"
        assert mgr.access_key_secret == "TESTSECRET456"

    def test_load_from_csv_missing_file_warns(self, tmp_path):
        with patch("oss_manager.oss2") as mock_oss2:
            mock_oss2.Auth.return_value = MagicMock()
            mock_bucket = MagicMock()
            mock_oss2.Bucket.return_value = mock_bucket
            mock_bucket.get_bucket_info.return_value = MagicMock()
            # Set env vars so validation passes even without CSV
            with patch.dict(os.environ, {
                "ALIYUN_ACCESS_KEY_ID": "ENV_ID",
                "ALIYUN_ACCESS_KEY_SECRET": "ENV_SECRET",
            }):
                from oss_manager import OSSManager
                # should not raise
                mgr = OSSManager(csv_path=str(tmp_path / "nonexistent.csv"))
        assert mgr.access_key_id == "ENV_ID"  # fell back to env var

    def test_load_from_csv_takes_priority_over_config_file(self, tmp_path):
        """CSV credentials take priority over .conf file."""
        csv_path = _make_csv(tmp_path, key_id="CSV_ID", key_secret="CSV_SECRET")
        conf_path = tmp_path / "oss.conf"
        conf_path.write_text("access_key_id = CONF_ID\naccess_key_secret = CONF_SECRET\n"
                              "oss_endpoint = oss-cn-beijing.aliyuncs.com\n"
                              "oss_bucket = hktech-agent-models\n")

        with patch("oss_manager.oss2") as mock_oss2:
            mock_oss2.Auth.return_value = MagicMock()
            mock_bucket = MagicMock()
            mock_oss2.Bucket.return_value = mock_bucket
            mock_bucket.get_bucket_info.return_value = MagicMock()

            from oss_manager import OSSManager
            mgr = OSSManager(config_path=str(conf_path), csv_path=csv_path)

        assert mgr.access_key_id == "CSV_ID"
```

**Step 2: 运行测试确认失败**

```bash
cd /home/huawei/project_opencode/lamda-ai/HKTech-Agent
python3 -m pytest tests/unit/test_oss_manager.py -v 2>&1 | tail -15
```

期望: `ImportError` 或 `TypeError` — `OSSManager.__init__` 还不接受 `csv_path` 参数。

**Step 3: 实现 `_load_from_csv()` 并修改 `__init__`**

打开 `HKTech-Agent/active_src/oss_manager.py`，做以下精确修改:

3a. **修改 `__init__` 签名**（第 44 行附近）:

```python
# 原来:
def __init__(self, config_path: Optional[str] = None):
# 改为:
def __init__(self, config_path: Optional[str] = None, csv_path: Optional[str] = None):
```

3b. **修改 `__init__` 内的加载顺序**（在 `self._load_from_env()` 调用之后，`if not self.access_key_id and config_path:` 之前插入 CSV 加载）:

```python
        # 尝试从环境变量加载
        self._load_from_env()

        # 如果环境变量不存在，尝试从CSV文件加载（优先级高于config_path）
        if not self.access_key_id and csv_path:
            self._load_from_csv(csv_path)

        # 如果以上都没有，尝试从配置文件加载
        if not self.access_key_id and config_path:
            self._load_from_config(config_path)
```

3c. **在 `_load_from_config()` 方法之后（第 116 行附近）新增 `_load_from_csv()` 方法**:

```python
    def _load_from_csv(self, csv_path: str):
        """从 AccessKey.csv 加载凭证（格式: 'AccessKey ID,AccessKey Secret' header）"""
        import csv as _csv
        try:
            with open(csv_path, 'r', encoding='utf-8') as f:
                reader = _csv.DictReader(f)
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

3d. **确认 `_load_from_env()` 中 endpoint 默认值为 Beijing 公网**（第 80 行附近）:

```python
self.endpoint = os.getenv('ALIYUN_OSS_ENDPOINT', 'oss-cn-beijing.aliyuncs.com')
```

**Step 4: 运行测试确认通过**

```bash
cd /home/huawei/project_opencode/lamda-ai/HKTech-Agent
python3 -m pytest tests/unit/test_oss_manager.py -v 2>&1 | tail -15
```

期望: `3 passed`

**Step 5: Commit**

```bash
cd /home/huawei/project_opencode/lamda-ai
git add HKTech-Agent/active_src/oss_manager.py HKTech-Agent/tests/unit/test_oss_manager.py
git commit -m "feat(oss): add CSV credential loading to OSSManager"
```

---

### Task 2: train_world_model.py 训练后自动上传

**Files:**
- Modify: `HKTech-Agent/prod/src/train_world_model.py`

**Step 1: 阅读文件末尾确认结构**

```bash
tail -30 /home/huawei/project_opencode/lamda-ai/HKTech-Agent/prod/src/train_world_model.py
```

确认 `if __name__ == '__main__':` 块末尾有 `model_path = train_gru_model(X, y, data_dir)` 这行。

**Step 2: 在 `if __name__ == '__main__':` 块之前（约第 195 行）插入上传函数**

在 `if __name__ == '__main__':` 这行**之前**添加以下函数（注意：是 module 级别函数，非嵌套）:

```python
def _upload_to_oss_after_training(data_dir: str, model_path: str) -> None:
    """训练完成后将模型和训练数据上传到阿里云OSS。oss2未安装时静默跳过。"""
    import sys as _sys

    # AccessKey.csv 在项目根目录 (lamda-ai/)，本文件在 prod/src/ 下，向上三级
    _script_dir = os.path.dirname(os.path.abspath(__file__))
    csv_path = os.path.normpath(os.path.join(_script_dir, "../../../AccessKey.csv"))

    # 导入 OSSManager（在 active_src/）
    _active_src = os.path.normpath(os.path.join(_script_dir, "../../active_src"))
    if _active_src not in _sys.path:
        _sys.path.insert(0, _active_src)

    try:
        from oss_manager import OSSManager
    except ImportError:
        print("⚠️  oss_manager 未找到，跳过OSS上传")
        return

    try:
        oss = OSSManager(csv_path=csv_path)
    except ImportError:
        print("⚠️  oss2 未安装，跳过OSS上传。运行: pip install oss2")
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
        else:
            print(f"⚠️  文件不存在，跳过: {fpath}")

    # 上传训练数据
    episodes_path = os.path.join(data_dir, "training_episodes.json")
    if os.path.exists(episodes_path):
        try:
            oss.upload_training_data(episodes_path, "training_episodes.json")
        except Exception as e:
            print(f"⚠️  上传 training_episodes.json 失败: {e}")

    print("✅ OSS上传流程完成")
```

**Step 3: 在 `__main__` 块末尾添加调用**

在 `model_path = train_gru_model(X, y, data_dir)` 之后添加:

```python
    # 训练成功后自动上传到OSS
    if model_path:
        _upload_to_oss_after_training(data_dir, model_path)
```

**Step 4: 验证语法无误**

```bash
cd /home/huawei/project_opencode/lamda-ai/HKTech-Agent
python3 -c "
import ast, sys
with open('prod/src/train_world_model.py') as f:
    src = f.read()
ast.parse(src)
print('✅ syntax OK')
"
```

期望: `✅ syntax OK`

**Step 5: 验证 import 正常**

```bash
python3 -c "
import sys
sys.path.insert(0, 'prod/src')
sys.path.insert(0, 'shared')
import train_world_model
print('✅ import OK')
print('_upload_to_oss_after_training:', callable(train_world_model._upload_to_oss_after_training))
"
```

期望: `✅ import OK` 且 `True`

**Step 6: Commit**

```bash
cd /home/huawei/project_opencode/lamda-ai
git add HKTech-Agent/prod/src/train_world_model.py
git commit -m "feat(train): auto-upload model and training data to OSS after training"
```

---

### Task 3: 修复 upload_to_oss.py

**Files:**
- Modify: `HKTech-Agent/upload_to_oss.py:17-21`

**Step 1: 修改两处**

打开 `HKTech-Agent/upload_to_oss.py`，做以下精确修改:

修改第 17 行 `ACCESS_KEY_FILE`:
```python
# 原来:
ACCESS_KEY_FILE = "/mnt/d/project/AccessKey.csv"
# 改为（相对于本文件向上一级，即 lamda-ai/）:
ACCESS_KEY_FILE = os.path.join(os.path.dirname(os.path.abspath(__file__)), "../AccessKey.csv")
```

修改第 20 行 `OSS_ENDPOINT` 默认值:
```python
# 原来:
OSS_ENDPOINT = os.environ.get("OSS_ENDPOINT", "oss-cn-hangzhou.aliyuncs.com")
# 改为:
OSS_ENDPOINT = os.environ.get("OSS_ENDPOINT", "oss-cn-beijing.aliyuncs.com")
```

**Step 2: 验证 AccessKey 路径可读**

```bash
python3 -c "
import os, sys
sys.path.insert(0, 'HKTech-Agent')
# 模拟 upload_to_oss.py 的路径计算
script_dir = os.path.dirname(os.path.abspath('HKTech-Agent/upload_to_oss.py'))
csv_path = os.path.normpath(os.path.join(script_dir, '../AccessKey.csv'))
print('CSV path:', csv_path)
print('Exists:', os.path.exists(csv_path))
"
```

期望: `Exists: True`

**Step 3: Commit**

```bash
cd /home/huawei/project_opencode/lamda-ai
git add HKTech-Agent/upload_to_oss.py
git commit -m "fix(upload): correct AccessKey.csv path and OSS endpoint region"
```

---

### Task 4: 安装 oss2 并端到端验证

**Step 1: 安装 oss2**

```bash
cd /home/huawei/project_opencode/lamda-ai/HKTech-Agent
source venv/bin/activate
pip install oss2
```

期望末尾: `Successfully installed oss2-...`

**Step 2: 测试 OSS 连接（upload_to_oss.py --test）**

```bash
python3 upload_to_oss.py --test
```

期望:
```
✅ AccessKey加载成功: LTAI5tGR...
✅ OSS连接成功!
   Bucket: hktech-agent-models
   位置: oss-cn-beijing
```

如果 bucket 不存在，期望提示 `❌ Bucket不存在: hktech-agent-models` — 这说明路径和凭证OK，只需在OSS控制台建 bucket。

**Step 3: 测试 OSSManager CSV 读取**

```bash
python3 -c "
import sys
sys.path.insert(0, 'active_src')
from oss_manager import OSSManager
oss = OSSManager(csv_path='../AccessKey.csv')
print('access_key_id:', oss.access_key_id[:8] + '...')
models = oss.list_models()
print('远程模型数:', len(models))
"
```

期望: `access_key_id: LTAI5tGR...` 且无异常

**Step 4: 运行训练脚本验证端到端**

```bash
python3 prod/src/train_world_model.py 2>&1 | tail -20
```

期望末尾出现:
```
✅ 模型上传成功: https://hktech-agent-models.oss-cn-beijing.aliyuncs.com/models/rssm_model.pt
✅ 模型上传成功: https://hktech-agent-models.oss-cn-beijing.aliyuncs.com/models/scaler.pkl
✅ 训练数据上传成功: https://cloud-training.oss-cn-beijing.aliyuncs.com/training-data/training_episodes.json
✅ OSS上传流程完成
```

如果出现 `NoSuchBucket`，需在阿里云OSS控制台（北京地域）创建 `hktech-agent-models` 和 `cloud-training` 两个 bucket，然后重跑。

**Step 5: 运行所有测试确认无回归**

```bash
python3 -m pytest tests/unit/ -v --tb=short 2>&1 | tail -20
```

期望: 所有测试 pass，无新失败。

**Step 6: 更新 requirements.txt**

```bash
echo "oss2>=2.18.0" >> requirements.txt
```

**Step 7: Commit 并推送**

```bash
cd /home/huawei/project_opencode/lamda-ai
git add HKTech-Agent/requirements.txt
git commit -m "chore: add oss2 dependency; OSS auto-upload verified end-to-end"
git push origin main
```
