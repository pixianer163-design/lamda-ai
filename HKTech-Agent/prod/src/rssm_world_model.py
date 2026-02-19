#!/usr/bin/env python3
"""
Lightweight RSSM World Model for HKTech Agent
轻量级世界模型 - 可在CPU上训练

基于 DreamerV2/V3 的简化实现
参数量: ~150K (可在CPU上快速训练)
"""

import torch
import torch.nn as nn
import torch.nn.functional as F
import numpy as np
import json
import os
from datetime import datetime
from typing import Dict, List, Tuple
import random

# 设置随机种子
def set_seed(seed=42):
    random.seed(seed)
    np.random.seed(seed)
    torch.manual_seed(seed)

set_seed()


class RSSM(nn.Module):
    """
    Recurrent State-Space Model
    核心世界模型组件
    
    结构:
    - Recurrent Model (h): GRU处理时序
    - Representation (z): 变分编码观测
    - Transition (prior): 预测下一状态
    - Observation (decoder): 重建观测
    - Reward Model: 预测收益
    """
    
    def __init__(self, 
                 obs_dim=15,      # 观测维度 (价格,技术指标,持仓等)
                 action_dim=3,    # 动作维度 (3只股票的目标仓位变化)
                 hidden_dim=64,   # 隐藏层维度 (小模型用64，大可128)
                 latent_dim=32,   # 潜变量维度
                 latent_classes=32):  # 离散潜变量类别数
        super().__init__()
        
        self.obs_dim = obs_dim
        self.action_dim = action_dim
        self.hidden_dim = hidden_dim
        self.latent_dim = latent_dim
        self.latent_classes = latent_classes
        self.latent_flat_dim = latent_dim * latent_classes
        
        # 1. Recurrent Model (h_t+1 = f(h_t, z_t, a_t))
        # 输入: [hidden + latent_flat + action]
        self.gru = nn.GRUCell(
            input_size=self.latent_flat_dim + action_dim,
            hidden_size=hidden_dim
        )
        
        # 2. Representation Model (q(z_t | h_t, o_t))
        # 从观测编码潜变量
        self.encoder = nn.Sequential(
            nn.Linear(obs_dim + hidden_dim, 128),
            nn.ReLU(),
            nn.Linear(128, latent_dim * latent_classes)
        )
        
        # 3. Transition/Prior Model (p(z_t | h_t))
        # 从隐藏状态预测潜变量（想象时用）
        self.prior = nn.Sequential(
            nn.Linear(hidden_dim, 128),
            nn.ReLU(),
            nn.Linear(128, latent_dim * latent_classes)
        )
        
        # 4. Observation Decoder (p(o_t | h_t, z_t))
        self.decoder = nn.Sequential(
            nn.Linear(hidden_dim + self.latent_flat_dim, 128),
            nn.ReLU(),
            nn.Linear(128, obs_dim)
        )
        
        # 5. Reward Predictor (p(r_t | h_t, z_t))
        self.reward_model = nn.Sequential(
            nn.Linear(hidden_dim + self.latent_flat_dim, 64),
            nn.ReLU(),
            nn.Linear(64, 1)
        )
        
        # 6. Continue Predictor (p(cont | h_t, z_t)) - 是否终止
        self.continue_model = nn.Sequential(
            nn.Linear(hidden_dim + self.latent_flat_dim, 64),
            nn.ReLU(),
            nn.Linear(64, 1),
            nn.Sigmoid()
        )
    
    def encode(self, obs: torch.Tensor, h: torch.Tensor) -> torch.Tensor:
        """
        编码观测为潜变量 (q(z|h,o))
        返回 logits
        """
        x = torch.cat([obs, h], dim=-1)
        logits = self.encoder(x)
        # reshape: [batch, latent_dim, latent_classes]
        logits = logits.view(-1, self.latent_dim, self.latent_classes)
        return logits
    
    def dynamics(self, h: torch.Tensor, z: torch.Tensor, action: torch.Tensor) -> torch.Tensor:
        """
        动力学模型: h_t+1 = GRU(h_t, [z_t, a_t])
        """
        x = torch.cat([z, action], dim=-1)
        h_next = self.gru(x, h)
        return h_next
    
    def imagine_prior(self, h: torch.Tensor) -> torch.Tensor:
        """
        先验预测: p(z|h)，用于想象未来
        """
        logits = self.prior(h)
        logits = logits.view(-1, self.latent_dim, self.latent_classes)
        return logits
    
    def decode(self, h: torch.Tensor, z: torch.Tensor) -> torch.Tensor:
        """
        解码观测: p(o|h,z)
        """
        x = torch.cat([h, z], dim=-1)
        obs = self.decoder(x)
        return obs
    
    def predict_reward(self, h: torch.Tensor, z: torch.Tensor) -> torch.Tensor:
        """
        预测奖励
        """
        x = torch.cat([h, z], dim=-1)
        reward = self.reward_model(x)
        return reward
    
    def predict_continue(self, h: torch.Tensor, z: torch.Tensor) -> torch.Tensor:
        """
        预测是否继续（非终止概率）
        """
        x = torch.cat([h, z], dim=-1)
        cont = self.continue_model(x)
        return cont
    
    def sample_z(self, logits: torch.Tensor) -> Tuple[torch.Tensor, torch.Tensor]:
        """
        从logits采样潜变量，使用Gumbel-Softmax（可微分）
        
        返回:
            z: [batch, latent_flat_dim] 采样结果
            z_dist: [batch, latent_dim, latent_classes] 分布
        """
        # 使用softmax获取分布
        z_dist = F.softmax(logits, dim=-1)
        
        # Gumbel-Softmax采样（训练时）
        if self.training:
            # 重参数化技巧
            u = torch.rand_like(logits)
            gumbel = -torch.log(-torch.log(u + 1e-8) + 1e-8)
            z_sample = F.softmax((logits + gumbel) / 0.5, dim=-1)  # temperature=0.5
        else:
            # 推理时直接用argmax
            z_sample = z_dist
        
        # 展平为 [batch, latent_dim * latent_classes]
        z_flat = z_sample.view(-1, self.latent_flat_dim)
        
        return z_flat, z_dist


class ActorCritic(nn.Module):
    """
    策略-价值网络 (SAC风格)
    """
    
    def __init__(self, hidden_dim=64, latent_flat_dim=1024, action_dim=3):
        super().__init__()
        
        input_dim = hidden_dim + latent_flat_dim
        
        # Actor (策略网络) - 输出动作分布
        self.actor = nn.Sequential(
            nn.Linear(input_dim, 128),
            nn.ReLU(),
            nn.Linear(128, 64),
            nn.ReLU(),
            nn.Linear(64, action_dim * 2)  # mean, log_std
        )
        
        # Critic (价值网络) - 双Q网络
        self.critic1 = nn.Sequential(
            nn.Linear(input_dim + action_dim, 128),
            nn.ReLU(),
            nn.Linear(128, 64),
            nn.ReLU(),
            nn.Linear(64, 1)
        )
        
        self.critic2 = nn.Sequential(
            nn.Linear(input_dim + action_dim, 128),
            nn.ReLU(),
            nn.Linear(128, 64),
            nn.ReLU(),
            nn.Linear(64, 1)
        )
    
    def get_action(self, state: torch.Tensor, deterministic=False) -> Tuple[torch.Tensor, torch.Tensor]:
        """
        采样动作
        
        返回:
            action: [batch, action_dim]
            log_prob: [batch, 1]
        """
        output = self.actor(state)
        mean, log_std = output.chunk(2, dim=-1)
        log_std = torch.clamp(log_std, -20, 2)
        std = torch.exp(log_std)
        
        if deterministic:
            action = torch.tanh(mean)
            log_prob = None
        else:
            # 重参数化采样
            noise = torch.randn_like(mean)
            raw_action = mean + std * noise
            action = torch.tanh(raw_action)
            
            # 计算log_prob (含tanh修正)
            log_prob = -0.5 * ((raw_action - mean) / (std + 1e-8)).pow(2) - log_std - 0.5 * np.log(2 * np.pi)
            log_prob = log_prob.sum(dim=-1, keepdim=True)
            log_prob -= (2 * (np.log(2) - raw_action - F.softplus(-2 * raw_action))).sum(dim=-1, keepdim=True)
        
        return action, log_prob
    
    def get_value(self, state: torch.Tensor, action: torch.Tensor) -> Tuple[torch.Tensor, torch.Tensor]:
        """
        估计Q值
        """
        x = torch.cat([state, action], dim=-1)
        q1 = self.critic1(x)
        q2 = self.critic2(x)
        return q1, q2


class WorldModelTrainer:
    """
    世界模型训练器
    """
    
    def __init__(self, data_dir="/opt/hktech-agent/data", device="cpu"):
        self.data_dir = data_dir
        self.device = torch.device(device)
        
        # 模型
        self.rssm = RSSM(obs_dim=15, action_dim=3, hidden_dim=64).to(device)
        self.actor_critic = ActorCritic(hidden_dim=64, latent_flat_dim=1024, action_dim=3).to(device)
        
        # 优化器
        self.world_optimizer = torch.optim.Adam(self.rssm.parameters(), lr=1e-3)
        self.actor_optimizer = torch.optim.Adam(self.actor_critic.parameters(), lr=3e-4)
        
        # 超参数
        self.batch_size = 16
        self.seq_len = 10  # 序列长度（10天）
        self.imagine_horizon = 5  # 想象步数
        
        self.model_path = f"{data_dir}/rssm_model.pt"
    
    def prepare_data(self, market_data: Dict, portfolio: Dict) -> np.ndarray:
        """
        将市场数据转换为观测向量
        
        观测维度 (15维):
        - 3只股票: 当前价, MA5, MA20, RSI, 涨跌幅 (15维)
        - 可选: 持仓比例, 现金比例
        """
        obs_list = []
        
        for code in ["00700", "09988", "03690"]:
            if code in market_data:
                data = market_data[code]
                obs_list.extend([
                    data.get('price', 0) / 500,  # 归一化
                    data.get('ma5', 0) / 500,
                    data.get('ma20', 0) / 500,
                    data.get('rsi', 50) / 100,
                    data.get('change_pct', 0) / 10
                ])
            else:
                obs_list.extend([0, 0, 0, 0.5, 0])
        
        return np.array(obs_list, dtype=np.float32)
    
    def train_world_model(self, episodes: List[Dict], epochs=50):
        """
        训练世界模型 (监督学习)
        
        episodes: [{'obs': [], 'action': [], 'reward': []}, ...]
        """
        print(f"🧠 训练世界模型 ({epochs} epochs)...")
        
        losses = []
        for epoch in range(epochs):
            epoch_loss = 0
            
            # 随机采样batch
            batch_episodes = random.sample(episodes, min(self.batch_size, len(episodes)))
            
            for ep in batch_episodes:
                obs_seq = torch.tensor(ep['obs'][:self.seq_len], dtype=torch.float32).to(self.device)
                action_seq = torch.tensor(ep['action'][:self.seq_len], dtype=torch.float32).to(self.device)
                reward_seq = torch.tensor(ep['reward'][:self.seq_len], dtype=torch.float32).to(self.device)
                
                # 初始化隐藏状态
                h = torch.zeros(1, self.rssm.hidden_dim).to(self.device)
                
                total_loss = 0
                kl_losses = []
                obs_losses = []
                reward_losses = []
                
                for t in range(len(obs_seq) - 1):
                    obs_t = obs_seq[t:t+1]
                    obs_next = obs_seq[t+1:t+1]
                    action_t = action_seq[t:t+1]
                    reward_t = reward_seq[t:t+1]
                    
                    # 编码当前观测
                    z_logits = self.rssm.encode(obs_t, h)
                    z, z_dist = self.rssm.sample_z(z_logits)
                    
                    # 动力学预测下一状态
                    h_next = self.rssm.dynamics(h, z, action_t)
                    
                    # 重建观测
                    obs_pred = self.rssm.decode(h, z)
                    obs_loss = F.mse_loss(obs_pred, obs_next)
                    
                    # 预测奖励
                    reward_pred = self.rssm.predict_reward(h, z)
                    reward_loss = F.mse_loss(reward_pred, reward_t)
                    
                    # KL散度 (与先验对比)
                    prior_logits = self.rssm.imagine_prior(h)
                    prior_dist = F.softmax(prior_logits, dim=-1)
                    kl_loss = F.kl_div(z_dist.log(), prior_dist, reduction='batchmean')
                    
                    # 总损失
                    loss = obs_loss + 0.1 * reward_loss + 0.001 * kl_loss
                    total_loss += loss
                    
                    kl_losses.append(kl_loss.item())
                    obs_losses.append(obs_loss.item())
                    reward_losses.append(reward_loss.item())
                    
                    h = h_next
                
                # 反向传播
                self.world_optimizer.zero_grad()
                total_loss.backward()
                torch.nn.utils.clip_grad_norm_(self.rssm.parameters(), 100)
                self.world_optimizer.step()
                
                epoch_loss += total_loss.item()
            
            losses.append(epoch_loss / len(batch_episodes))
            
            if (epoch + 1) % 10 == 0:
                print(f"  Epoch {epoch+1}/{epochs}, Loss: {losses[-1]:.4f}, "
                      f"KL: {np.mean(kl_losses):.4f}, "
                      f"Obs: {np.mean(obs_losses):.4f}, "
                      f"Reward: {np.mean(reward_losses):.4f}")
        
        print(f"✅ 世界模型训练完成，最终Loss: {losses[-1]:.4f}")
        return losses
    
    def imagine_future(self, initial_obs: np.ndarray, initial_action: np.ndarray, horizon=5) -> Dict:
        """
        想象未来 (核心功能)
        
        返回预测的未来轨迹
        """
        self.rssm.eval()
        
        with torch.no_grad():
            obs = torch.tensor(initial_obs, dtype=torch.float32).unsqueeze(0).to(self.device)
            action = torch.tensor(initial_action, dtype=torch.float32).unsqueeze(0).to(self.device)
            
            # 初始化
            h = torch.zeros(1, self.rssm.hidden_dim).to(self.device)
            z_logits = self.rssm.encode(obs, h)
            z, _ = self.rssm.sample_z(z_logits)
            
            # 想象未来
            imagined_trajectory = []
            
            for t in range(horizon):
                # 动力学预测
                h = self.rssm.dynamics(h, z, action)
                
                # 用先验预测下一潜变量
                prior_logits = self.rssm.imagine_prior(h)
                z, _ = self.rssm.sample_z(prior_logits)
                
                # 解码观测
                obs_pred = self.rssm.decode(h, z)
                
                # 预测奖励
                reward_pred = self.rssm.predict_reward(h, z)
                
                # 用actor预测下一动作
                state = torch.cat([h, z], dim=-1)
                action, _ = self.actor_critic.get_action(state, deterministic=True)
                
                imagined_trajectory.append({
                    'step': t,
                    'predicted_obs': obs_pred.cpu().numpy()[0],
                    'predicted_reward': reward_pred.cpu().numpy()[0][0],
                    'action': action.cpu().numpy()[0]
                })
            
            return {
                'horizon': horizon,
                'trajectory': imagined_trajectory,
                'cumulative_reward': sum([t['predicted_reward'] for t in imagined_trajectory])
            }
    
    def save(self):
        """保存模型"""
        torch.save({
            'rssm': self.rssm.state_dict(),
            'actor_critic': self.actor_critic.state_dict(),
            'world_optimizer': self.world_optimizer.state_dict(),
            'actor_optimizer': self.actor_optimizer.state_dict()
        }, self.model_path)
        print(f"💾 模型已保存: {self.model_path}")
    
    def load(self):
        """加载模型"""
        if os.path.exists(self.model_path):
            checkpoint = torch.load(self.model_path, map_location=self.device)
            self.rssm.load_state_dict(checkpoint['rssm'])
            self.actor_critic.load_state_dict(checkpoint['actor_critic'])
            self.world_optimizer.load_state_dict(checkpoint['world_optimizer'])
            self.actor_optimizer.load_state_dict(checkpoint['actor_optimizer'])
            print(f"📂 模型已加载: {self.model_path}")
            return True
        return False


def generate_dummy_data(num_episodes=20, seq_len=15):
    """
    生成模拟训练数据 (用于测试)
    实际使用时替换为真实历史数据
    """
    episodes = []
    
    for _ in range(num_episodes):
        obs_seq = []
        action_seq = []
        reward_seq = []
        
        # 随机游走价格
        price = 400
        for _ in range(seq_len):
            # 模拟观测 [腾讯价格/500, MA5/500, MA20/500, RSI/100, 涨跌幅/10, ...]
            obs = [
                price/500, (price*0.98)/500, (price*0.95)/500, 0.5, 0.01,
                (price*0.9)/500, (price*0.88)/500, (price*0.85)/500, 0.45, 0.02,
                (price*1.1)/500, (price*1.08)/500, (price*1.05)/500, 0.55, -0.01
            ]
            obs_seq.append(obs)
            
            # 随机动作 (3只股票的目标仓位)
            action = np.random.randn(3) * 0.1
            action_seq.append(action)
            
            # 模拟收益
            reward = np.random.randn() * 0.01
            reward_seq.append([reward])
            
            # 价格随机游走
            price *= (1 + np.random.randn() * 0.02)
        
        episodes.append({
            'obs': obs_seq,
            'action': action_seq,
            'reward': reward_seq
        })
    
    return episodes


def test_world_model():
    """测试世界模型"""
    print("="*50)
    print("🧪 测试 RSSM 世界模型")
    print("="*50)
    
    # 创建训练器
    trainer = WorldModelTrainer(device="cpu")
    
    # 生成模拟数据
    print("📊 生成模拟训练数据...")
    episodes = generate_dummy_data(num_episodes=20, seq_len=15)
    print(f"✅ 生成 {len(episodes)} 条训练序列")
    
    # 训练世界模型
    print("\n🚀 开始训练...")
    trainer.train_world_model(episodes, epochs=50)
    
    # 保存
    trainer.save()
    
    # 测试想象功能
    print("\n🔮 测试想象功能...")
    initial_obs = episodes[0]['obs'][0]
    initial_action = episodes[0]['action'][0]
    
    prediction = trainer.imagine_future(initial_obs, initial_action, horizon=5)
    
    print(f"\n预测未来 {prediction['horizon']} 步:")
    for step in prediction['trajectory']:
        print(f"  Step {step['step']}: 预测收益={step['predicted_reward']:.4f}, "
              f"动作={[f'{a:.2f}' for a in step['action']]}")
    
    print(f"\n累计预测收益: {prediction['cumulative_reward']:.4f}")
    print("\n✅ 测试完成!")
    
    return trainer


if __name__ == "__main__":
    test_world_model()
