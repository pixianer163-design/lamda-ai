# Code Agent 世界模型测试指南

为 Code Agent 准备的 RSSM World Model 开发和测试指南。

## 🎯 测试目标

验证世界模型的正确性：
1. ✅ 模型结构正确（参数、维度）
2. ✅ 前向传播正常（编码/解码/预测）
3. ✅ 想象力功能正常（预测未来）
4. ✅ 训练流程正常（损失下降）
5. ✅ 保存/加载一致（权重不丢失）
6. ✅ 梯度流动正常（可训练）

---

## 🚀 快速开始

### 1. 运行完整测试

```bash
cd /opt/hktech-agent/active_src

# 运行所有测试
python3 test_rssm_world_model.py

# 详细输出
python3 test_rssm_world_model.py --verbose

# 只生成测试数据
python3 test_rssm_world_model.py --generate-data
```

### 2. 运行单个测试

```bash
# 测试模型初始化
python3 test_rssm_world_model.py --test init

# 测试前向传播
python3 test_rssm_world_model.py --test forward

# 测试想象力
python3 test_rssm_world_model.py --test imagine

# 测试训练流程
python3 test_rssm_world_model.py --test train

# 测试保存/加载
python3 test_rssm_world_model.py --test save

# 测试梯度流
python3 test_rssm_world_model.py --test gradient
```

---

## 📋 测试详解

### 测试1: 模型初始化
验证模型能正确创建，参数数量合理。

```python
rssm = RSSM(
    obs_dim=15,        # 观测维度
    action_dim=3,      # 动作维度
    hidden_dim=64,     # 隐藏层
    latent_dim=32,     # 潜变量维度
    latent_classes=32  # 离散类别
)
```

**预期输出**:
```
✅ RSSM 模型: ~290K 参数
✅ Actor-Critic: ~150K 参数
✅ 总计: ~440K 参数
```

---

### 测试2: 前向传播
验证输入输出维度匹配。

**测试流程**:
1. 创建随机输入 (batch=4, obs=15)
2. 编码观测 → 潜变量
3. 动力学预测 → 下一状态
4. 解码 → 重建观测
5. 预测奖励

**预期输出**:
```
输入观测 shape: torch.Size([4, 15])
潜变量 shape: torch.Size([4, 1024])
下一隐藏状态 shape: torch.Size([4, 64])
重建观测 shape: torch.Size([4, 15])
奖励预测 shape: torch.Size([4, 1])
```

---

### 测试3: 想象力（核心）
测试世界模型预测未来的能力。

```python
result = trainer.imagine_future(
    initial_obs,     # 初始观测
    initial_action,  # 初始动作
    horizon=5        # 预测5步
)
```

**预期输出**:
```
✅ 想象 horizon: 5
✅ 预测轨迹长度: 5
✅ 预测累积奖励: 0.1234
```

---

### 测试4: 训练流程
验证模型能正常训练，损失下降。

```python
# 生成模拟数据
episodes = generate_mock_episodes(n_episodes=5, seq_len=10)

# 训练
losses = trainer.train_world_model(episodes, epochs=10)
```

**预期输出**:
```
Epoch 1/10, Loss: 2.3456
Epoch 5/10, Loss: 1.2345
Epoch 10/10, Loss: 0.8765

✅ Loss 下降: 62.7%
✅ 模型已保存
✅ 模型已加载
```

---

### 测试5: 保存/加载一致性
验证模型权重正确保存和恢复。

**测试方法**:
1. 训练并保存模型
2. 用模型预测（得到结果A）
3. 重新加载模型
4. 再次预测（得到结果B）
5. 比较 A 和 B

**预期输出**:
```
✅ 保存前预测奖励: 0.1234
✅ 加载后预测奖励: 0.1234
✅ 差异: 0.000001 (应接近0)
✅ 一致性验证通过
```

---

### 测试6: 梯度流检查
验证所有参数都能接收梯度（可训练）。

```python
# 前向 + 反向传播
loss.backward()

# 检查每个参数的梯度
for param in model.parameters():
    assert param.grad is not None
```

**预期输出**:
```
✅ 有梯度的参数: 50
✅ 无梯度的参数: 0
✅ 最大梯度范数: 1.2345
✅ 所有参数都有梯度
```

---

## 🔧 开发调试技巧

### 1. 检查模型结构

```python
from rssm_world_model import RSSM

rssm = RSSM()
print(rssm)

# 查看各层参数
for name, param in rssm.named_parameters():
    print(f"{name}: {param.shape}")
```

### 2. 可视化训练过程

```python
import matplotlib.pyplot as plt

losses = trainer.train_world_model(episodes, epochs=50)

plt.plot(losses)
plt.xlabel('Epoch')
plt.ylabel('Loss')
plt.title('Training Loss')
plt.savefig('/opt/hktech-agent/test_data/training_loss.png')
```

### 3. 检查中间输出

```python
# 在 imagination 中添加调试信息
def imagine_future(self, ...):
    print(f"Step {t}: h_mean={h.mean():.4f}, z_mean={z.mean():.4f}")
    print(f"Step {t}: obs_pred_range=[{obs_pred.min():.4f}, {obs_pred.max():.4f}]")
```

### 4. 使用模拟数据快速迭代

```python
# 生成可控的测试数据
def generate_deterministic_data():
    """生成确定性的测试数据，便于调试"""
    np.random.seed(42)
    # ... 生成数据
```

---

## 🐛 常见问题

### 问题1: 维度不匹配
```
RuntimeError: size mismatch
```
**解决**: 检查 `latent_flat_dim = latent_dim * latent_classes` 计算是否正确。

### 问题2: Loss 不下降
```
Epoch 1/10, Loss: 2.5
Epoch 10/10, Loss: 2.4
```
**解决**: 
- 检查学习率是否太小
- 检查数据是否正确归一化
- 检查梯度是否裁剪过严

### 问题3: NaN 值
```
Loss: nan
```
**解决**:
- 检查输入数据是否有 NaN
- 添加梯度裁剪: `torch.nn.utils.clip_grad_norm_`
- 检查除零操作

### 问题4: 想象力输出不变
```
Step 0: reward=0.1
Step 1: reward=0.1
Step 2: reward=0.1  # 所有步骤相同
```
**解决**:
- 检查 hidden state 是否在更新
- 检查 action 是否在变化
- 检查 prior 预测是否有效

---

## 📊 性能基准

在 CPU 上运行测试的预期时间：

| 测试 | 预期时间 | 说明 |
|------|----------|------|
| 模型初始化 | < 1s | 创建模型实例 |
| 前向传播 | < 1s | 单次前向 |
| 想象力 | < 2s | 5步预测 |
| 训练流程 | 10-30s | 5 episodes, 10 epochs |
| 保存/加载 | < 2s | 文件 I/O |
| 梯度检查 | < 1s | 单次反向传播 |
| **总计** | **15-40s** | 完整测试套件 |

---

## 🎯 开发 Workflow

### 迭代开发步骤：

1. **修改代码** → 编辑 `rssm_world_model.py`
2. **运行测试** → `python3 test_rssm_world_model.py --test forward`
3. **检查结果** → 查看输出和报告
4. **修复问题** → 如果有失败，修复代码
5. **完整测试** → 运行所有测试确认
6. **提交代码** → `git commit` 和 `git push`

### 推荐提交信息：
```bash
git commit -m "Fix RSSM encoder dimension mismatch

- Fix latent_flat_dim calculation
- Add input validation for obs_dim
- All tests passing

Test: python3 test_rssm_world_model.py"
```

---

## 📁 测试相关文件

| 文件 | 说明 |
|------|------|
| `test_rssm_world_model.py` | 主测试脚本 |
| `/opt/hktech-agent/test_data/` | 测试数据目录 |
| `mock_episodes.npy` | 生成的测试数据 |
| `test_report.json` | 测试报告 |
| `test_model.pt` | 测试保存的模型 |

---

## ✅ 验收标准

Code Agent 完成开发后，应满足：

1. ✅ 所有测试通过（6/6）
2. ✅ Loss 下降 > 50%（训练测试）
3. ✅ 保存/加载差异 < 1e-5
4. ✅ 想象力输出合理（非NaN，范围正常）
5. ✅ 所有参数有梯度
6. ✅ 训练时间 < 60s（10 episodes, 50 epochs）

---

**开始测试**: `python3 test_rssm_world_model.py` 🚀
