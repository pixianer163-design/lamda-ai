# Agent Factory 需求文档

> 本文档供 Code Agent 开发使用  
> 状态: 📝 待开发  
> 优先级: 🔴 高

---

## 1. DPML世界模型 (World Model 2.0)

### 1.1 背景
当前系统使用RSSM世界模型，但需要更强大的元学习能力来适应不同股票的个性化特征。

### 1.2 目标
实现基于MAML的DPML (Differentiable Programming Meta-Learning) 模型，支持：
- 跨股票元知识学习
- 单只股票快速适应（5-10步梯度下降）
- 显式/隐式双记忆机制

### 1.3 架构要求

```python
class DPMLModel(nn.Module):
    """
    DPML核心模型
    """
    def __init__(self, config):
        # 显式记忆 - LSTM编码长期规律
        self.explicit_memory = nn.LSTM(...)
        
        # 隐式记忆 - Transformer编码短期适应
        self.implicit_memory = nn.TransformerEncoder(...)
        
        # 自适应门控
        self.gate = nn.Sequential(...)
        
        # 预测头
        self.predictor = nn.Sequential(...)
    
    def forward(self, x):
        # 融合两种记忆
        # 输出: 预期收益、置信度、不确定性
        pass
    
    def fast_adapt(self, support_data, steps=5):
        """
        快速适应 - MAML内循环
        """
        # 从元参数出发
        # 5步梯度下降得到个性化参数
        pass
```

### 1.4 输入/输出

**输入**:
```python
{
    "price_history": Tensor[seq_len, features],  # 历史价格
    "technical_indicators": Tensor[seq_len, indicators],  # 技术指标
    "market_state": Dict  # 市场状态
}
```

**输出**:
```python
{
    "expected_return": float,  # 预期收益
    "confidence": float,       # 置信度 [0,1]
    "uncertainty": float,      # 不确定性
    "gate_weight": float       # 显式/隐式记忆权重
}
```

### 1.5 训练要求

**元学习训练**:
- 任务: 多股票预测任务
- Support Set: 最近50天数据
- Query Set: 未来5天预测
- 外循环: 元参数更新
- 内循环: 5步快速适应

**损失函数**:
- MSE损失: 预测收益 vs 实际收益
- 方向准确率: 涨跌方向判断
- 不确定性校准: 置信度与实际误差匹配

### 1.6 接口定义

```python
class WorldModel2:
    def __init__(self, model_path=None):
        """初始化，可加载预训练模型"""
        pass
    
    def predict(self, stock_code: str, data: pd.DataFrame) -> Prediction:
        """
        预测股票未来走势
        
        Args:
            stock_code: 股票代码 (如 "00700")
            data: 历史数据DataFrame
            
        Returns:
            Prediction: 包含预期收益、置信度等
        """
        pass
    
    def adapt_and_predict(self, stock_code: str, data: pd.DataFrame) -> Prediction:
        """
        快速适应并预测（用于新股票）
        """
        pass
```

### 1.7 验收标准

- [ ] 模型可加载/保存
- [ ] 单次预测 < 100ms
- [ ] 快速适应 < 2分钟
- [ ] 方向准确率 > 55%（超越随机）
- [ ] 支持1000只股票缓存

---

## 2. DSL策略引擎 (策略描述语言)

### 2.1 背景
当前策略用Python代码实现，对非程序员不友好。需要一套DSL让业务人员也能编写策略。

### 2.2 目标
设计并实现一套交易策略DSL，支持：
- 类Python语法（技术人员）
- 自然语言描述（业务人员）
- 可视化拖拽（新手）

### 2.3 DSL语法示例

```python
# 类Python语法
strategy MyStrategy {
    # 入场条件
    entry: {
        rsi(14) < 30 and
        macd.bullish_cross() and
        volume > ma(volume, 20) * 1.5
    }
    
    # 出场条件
    exit: {
        rsi(14) > 70 or
        trailing_stop(5%) or
        time_limit(5, days)
    }
    
    # 仓位管理
    position: {
        size: kelly_criterion(confidence),
        max_position: 0.3,  # 单股最大30%
        risk_per_trade: 0.02  # 单笔风险2%
    }
}
```

### 2.4 编译器架构

```
DSL源码
    ↓
Lexer (词法分析) → Token序列
    ↓
Parser (语法分析) → AST
    ↓
Type Checker (类型检查)
    ↓
Code Generator → Python/NumPy代码
    ↓
执行引擎
```

### 2.5 核心组件

**2.5.1 Lexer**
```python
class DSLLexer:
    """词法分析器"""
    
    keywords = ['strategy', 'entry', 'exit', 'position', 'and', 'or']
    
    def tokenize(self, source: str) -> List[Token]:
        pass
```

**2.5.2 Parser**
```python
class DSLParser:
    """语法分析器"""
    
    def parse(self, tokens: List[Token]) -> AST:
        """生成抽象语法树"""
        pass
```

**2.5.3 执行引擎**
```python
class DSLExecutor:
    """DSL执行引擎"""
    
    def compile(self, ast: AST) -> Callable:
        """编译AST为可执行函数"""
        pass
    
    def execute(self, strategy: Strategy, market_data: Dict) -> Signal:
        """执行策略"""
        pass
```

### 2.6 内置函数

**技术指标**:
- `rsi(period)` - RSI指标
- `macd(fast, slow, signal)` - MACD
- `bollinger(period, std)` - 布林带
- `ma(data, period)` - 移动平均
- `ema(data, period)` - 指数移动平均

**风险控制**:
- `trailing_stop(percent)` - 移动止损
- `stop_loss(percent)` - 固定止损
- `take_profit(percent)` - 止盈
- `time_limit(n, unit)` - 时间限制
- `kelly_criterion(confidence)` - 凯利公式

### 2.7 自然语言→DSL (LLM翻译)

```python
class NaturalLanguageTranslator:
    """自然语言翻译器"""
    
    def translate(self, description: str) -> str:
        """
        将自然语言翻译为DSL
        
        Example:
        Input: "RSI超卖时买入，设置5%移动止损"
        Output: strategy { entry: rsi(14) < 30; exit: trailing_stop(5%) }
        """
        pass
```

### 2.8 验收标准

- [ ] 支持类Python语法
- [ ] 支持自然语言翻译
- [ ] 编译错误有清晰提示
- [ ] 执行性能 < 10ms
- [ ] 提供10个示例策略

---

## 3. 云端训练管道

### 3.1 背景
DPML模型需要在云端GPU上预训练，然后部署到边缘设备。

### 3.2 目标
建立完整的云端训练管道：
- 数据准备 → 模型训练 → 模型验证 → 模型部署

### 3.3 训练流程

```python
class DPMLTrainingPipeline:
    def prepare_data(self):
        """准备元学习数据集"""
        pass
    
    def train(self, config: Dict):
        """云端训练"""
        pass
    
    def validate(self, model_path: str):
        """模型验证"""
        pass
    
    def deploy(self, model_path: str):
        """部署到生产"""
        pass
```

### 3.4 阿里云集成

```yaml
# 训练配置
cloud_training:
  provider: aliyun
  instance_type: ecs.gn7i-c8g1.2xlarge  # A10 GPU
  preemptible: true  # 抢占式实例
  
training:
  epochs: 100
  batch_size: 16
  learning_rate: 0.001
  
output:
  model_path: s3://hktech-models/dpml/
```

---

## 4. 当前可用资源

### 4.1 已有代码
- `/opt/hktech-agent/factory/` - Agent Factory核心框架
- `/opt/hktech-agent/world_model_2.0/` - DPML目录结构（空）
- `/opt/hktech-agent/cloud_training/` - 云端训练框架

### 4.2 参考资料
- MAML论文: https://arxiv.org/abs/1703.03400
- RSSM实现: `/opt/hktech-agent/prod/src/rssm_world_model.py`
- 配置示例: `/opt/hktech-agent/factory/configs/`

---

## 5. 下一步行动

1. **Code Agent**: 实现DPML核心模型 (world_model_2.0/models/)
2. **Code Agent**: 实现DSL编译器 (dsl/)
3. **Alex**: 集成到Agent Factory并进行A/B测试

---

*文档版本: v1.0*  
*创建日期: 2026-02-19*  
*负责人: Alex*  
*开发负责人: Code Agent*
