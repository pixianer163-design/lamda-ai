"""
DPML核心模型原型代码
供Code Agent参考和扩展
"""

import torch
import torch.nn as nn
from typing import Dict, Tuple


class ExplicitMemory(nn.Module):
    """
    显式记忆网络 - LSTM编码长期规律
    """
    def __init__(self, input_dim: int, hidden_dim: int, num_layers: int = 2):
        super().__init__()
        self.input_dim = input_dim
        self.hidden_dim = hidden_dim
        self.num_layers = num_layers
        
        self.lstm = nn.LSTM(
            input_size=input_dim,
            hidden_size=hidden_dim,
            num_layers=num_layers,
            batch_first=True,
            dropout=0.2
        )
    
    def forward(self, x: torch.Tensor) -> torch.Tensor:
        """
        Args:
            x: [batch, seq_len, input_dim]
        Returns:
            h: [batch, hidden_dim] - 最后一个时间步的隐藏状态
        """
        output, (h_n, c_n) = self.lstm(x)
        # 取最后一层的最后一个时间步
        return h_n[-1]


class ImplicitMemory(nn.Module):
    """
    隐式记忆网络 - Transformer编码短期适应
    """
    def __init__(self, hidden_dim: int, num_heads: int = 8, num_layers: int = 2):
        super().__init__()
        self.hidden_dim = hidden_dim
        
        encoder_layer = nn.TransformerEncoderLayer(
            d_model=hidden_dim,
            nhead=num_heads,
            dim_feedforward=hidden_dim * 4,
            dropout=0.1,
            batch_first=True
        )
        self.transformer = nn.TransformerEncoder(encoder_layer, num_layers=num_layers)
    
    def forward(self, x: torch.Tensor) -> torch.Tensor:
        """
        Args:
            x: [batch, seq_len, hidden_dim]
        Returns:
            out: [batch, hidden_dim] - 最后一个时间步
        """
        output = self.transformer(x)
        return output[:, -1, :]


class AdaptiveGate(nn.Module):
    """
    自适应门控 - 动态融合显式/隐式记忆
    """
    def __init__(self, hidden_dim: int):
        super().__init__()
        self.gate = nn.Sequential(
            nn.Linear(hidden_dim * 2, hidden_dim),
            nn.ReLU(),
            nn.Dropout(0.1),
            nn.Linear(hidden_dim, 1),
            nn.Sigmoid()
        )
    
    def forward(self, explicit_feat: torch.Tensor, implicit_feat: torch.Tensor) -> Tuple[torch.Tensor, torch.Tensor]:
        """
        Returns:
            fused: 融合后的特征
            gate_weight: 门控权重 [0,1]，越接近1表示显式记忆权重越高
        """
        concat = torch.cat([explicit_feat, implicit_feat], dim=-1)
        gate_weight = self.gate(concat)
        
        fused = gate_weight * explicit_feat + (1 - gate_weight) * implicit_feat
        return fused, gate_weight


class DPMLModel(nn.Module):
    """
    DPML主模型 (原型)
    
    架构:
    - 显式记忆 (LSTM): 学习长期规律
    - 隐式记忆 (Transformer): 学习短期适应
    - 自适应门控: 动态融合
    - 预测头: 输出收益、不确定性
    """
    def __init__(self, config: Dict):
        super().__init__()
        
        self.input_dim = config.get('input_dim', 64)
        self.hidden_dim = config.get('hidden_dim', 128)
        self.output_dim = config.get('output_dim', 3)
        
        # 输入投影
        self.input_proj = nn.Linear(self.input_dim, self.hidden_dim)
        
        # 双记忆网络
        self.explicit_memory = ExplicitMemory(
            input_dim=self.hidden_dim,
            hidden_dim=self.hidden_dim
        )
        self.implicit_memory = ImplicitMemory(
            hidden_dim=self.hidden_dim
        )
        
        # 自适应门控
        self.adaptive_gate = AdaptiveGate(self.hidden_dim)
        
        # 预测头
        self.predictor = nn.Sequential(
            nn.Linear(self.hidden_dim, self.hidden_dim // 2),
            nn.ReLU(),
            nn.Dropout(0.2),
            nn.Linear(self.hidden_dim // 2, self.output_dim)
        )
        
        # 不确定性估计
        self.uncertainty_head = nn.Sequential(
            nn.Linear(self.hidden_dim, self.hidden_dim // 2),
            nn.ReLU(),
            nn.Linear(self.hidden_dim // 2, 1),
            nn.Sigmoid()
        )
    
    def forward(self, x: torch.Tensor) -> Dict[str, torch.Tensor]:
        """
        标准前向传播
        
        Args:
            x: [batch, seq_len, input_dim]
        
        Returns:
            Dict: {
                'return': 预期收益,
                'uncertainty': 不确定性,
                'gate': 门控权重
            }
        """
        # 输入投影
        x = self.input_proj(x)
        
        # 双记忆编码
        explicit_feat = self.explicit_memory(x)
        implicit_feat = self.implicit_memory(x)
        
        # 自适应融合
        fused_feat, gate_weight = self.adaptive_gate(explicit_feat, implicit_feat)
        
        # 预测
        output = self.predictor(fused_feat)
        uncertainty = self.uncertainty_head(fused_feat)
        
        return {
            'return': output[:, 0],
            'uncertainty': uncertainty.squeeze(),
            'gate': gate_weight.squeeze()
        }
    
    def get_meta_params(self) -> torch.Tensor:
        """获取元参数（用于MAML外循环）"""
        return torch.cat([p.flatten() for p in self.parameters()])
    
    def set_meta_params(self, params: torch.Tensor):
        """设置元参数"""
        idx = 0
        for p in self.parameters():
            numel = p.numel()
            p.data = params[idx:idx+numel].view_as(p)
            idx += numel


class MAMLTrainer:
    """
    MAML训练器原型
    
    外循环: 更新元参数
    内循环: 针对特定任务快速适应
    """
    def __init__(self, model: DPMLModel, inner_lr: float = 0.01, inner_steps: int = 5):
        self.model = model
        self.inner_lr = inner_lr
        self.inner_steps = inner_steps
    
    def inner_loop(self, support_x: torch.Tensor, support_y: torch.Tensor) -> torch.Tensor:
        """
        内循环 - 快速适应
        
        Args:
            support_x: 支持集输入 [batch, seq_len, features]
            support_y: 支持集目标 [batch]
        
        Returns:
            adapted_params: 适应后的参数
        """
        # 克隆当前参数
        adapted_params = self.model.get_meta_params().clone().requires_grad_(True)
        
        # 内循环梯度下降
        for step in range(self.inner_steps):
            # 使用适应后的参数前向传播
            self.model.set_meta_params(adapted_params)
            pred = self.model(support_x)
            
            # 计算损失
            loss = nn.MSELoss()(pred['return'], support_y)
            
            # 计算梯度
            grads = torch.autograd.grad(loss, adapted_params, create_graph=True)
            
            # 梯度下降更新
            adapted_params = adapted_params - self.inner_lr * grads[0]
        
        return adapted_params
    
    def meta_update(self, tasks: List[Tuple]) -> float:
        """
        外循环 - 元更新
        
        Args:
            tasks: List of (support_x, support_y, query_x, query_y)
        
        Returns:
            loss: 平均查询集损失
        """
        meta_optimizer = torch.optim.Adam(self.model.parameters(), lr=0.001)
        
        total_loss = 0
        for support_x, support_y, query_x, query_y in tasks:
            # 内循环适应
            adapted_params = self.inner_loop(support_x, support_y)
            
            # 在查询集上评估
            self.model.set_meta_params(adapted_params)
            query_pred = self.model(query_x)
            query_loss = nn.MSELoss()(query_pred['return'], query_y)
            
            total_loss += query_loss
        
        # 外循环更新
        meta_optimizer.zero_grad()
        total_loss.backward()
        meta_optimizer.step()
        
        return total_loss.item() / len(tasks)


# 配置示例
CONFIG = {
    'input_dim': 64,      # 输入特征维度 (价格 + 技术指标)
    'hidden_dim': 128,    # 隐藏层维度
    'output_dim': 3,      # 输出维度 (买/卖/持有 的概率)
    'inner_lr': 0.01,     # 内循环学习率
    'inner_steps': 5,     # 内循环步数
    'meta_lr': 0.001      # 外循环学习率
}


if __name__ == '__main__':
    # 测试
    model = DPMLModel(CONFIG)
    
    # 模拟输入
    batch_size = 4
    seq_len = 50
    x = torch.randn(batch_size, seq_len, CONFIG['input_dim'])
    
    # 前向传播
    output = model(x)
    print(f"预期收益: {output['return']}")
    print(f"不确定性: {output['uncertainty']}")
    print(f"门控权重: {output['gate']}")
