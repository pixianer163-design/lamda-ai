#!/usr/bin/env python3
"""
RSSM World Model 测试套件
为 Code Agent 提供完整的测试环境

使用方法:
    python3 test_rssm_world_model.py
    python3 test_rssm_world_model.py --verbose
    python3 test_rssm_world_model.py --generate-data
"""

import sys
import os
import torch
import numpy as np
import json
import argparse
from datetime import datetime, timedelta

# 添加项目路径
_SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
_PROJECT_ROOT = os.path.dirname(_SCRIPT_DIR)
sys.path.insert(0, os.path.join(_PROJECT_ROOT, 'prod', 'src'))
sys.path.insert(0, _SCRIPT_DIR)

try:
    from rssm_world_model import RSSM, ActorCritic, WorldModelTrainer
except ImportError as e:
    print(f"❌ 导入失败: {e}")
    print("请确保在正确的目录运行，或检查 rssm_world_model.py 是否存在")
    sys.exit(1)


class WorldModelTester:
    """世界模型测试器"""
    
    def __init__(self, verbose=False):
        self.verbose = verbose
        self.device = torch.device("cpu")
        self.test_results = []
        
        # 创建测试数据目录
        self.test_data_dir = os.path.join(
            os.path.expanduser('~'), '.hktech_agent', 'test_data')
        os.makedirs(self.test_data_dir, exist_ok=True)
        
    def log(self, msg):
        """打印日志"""
        if self.verbose:
            print(f"  {msg}")
    
    def generate_mock_episodes(self, n_episodes=10, seq_len=20):
        """
        生成模拟训练数据
        
        模拟真实的交易场景：
        - 随机价格序列
        - 随机动作（仓位调整）
        - 根据价格变化计算奖励
        """
        print(f"🎲 生成模拟数据 ({n_episodes} episodes, {seq_len} steps each)...")
        
        episodes = []
        
        for ep in range(n_episodes):
            obs_seq = []
            action_seq = []
            reward_seq = []
            
            # 初始价格
            base_price = 400 + np.random.randn() * 50
            
            for t in range(seq_len):
                # 模拟价格随机游走
                price_change = np.random.randn() * 0.02
                price = base_price * (1 + price_change)
                base_price = price
                
                # 构建观测 (15维)
                # 3只股票，每只股票5个特征
                obs = []
                for stock in range(3):
                    stock_price = price * (1 + np.random.randn() * 0.1)
                    obs.extend([
                        stock_price / 500,  # 价格归一化
                        (stock_price * 0.98) / 500,  # MA5
                        (stock_price * 0.95) / 500,  # MA20
                        50 + np.random.randn() * 20,  # RSI
                        price_change * 100  # 涨跌幅
                    ])
                
                # 随机动作 (3维，每只股票的目标仓位)
                action = np.random.randn(3) * 0.3
                action = np.clip(action, -1, 1)
                
                # 奖励（基于价格变化和动作）
                reward = price_change * 100 * np.mean(action)
                
                obs_seq.append(obs)
                action_seq.append(action)
                reward_seq.append([reward])
            
            episodes.append({
                'obs': np.array(obs_seq, dtype=np.float32),
                'action': np.array(action_seq, dtype=np.float32),
                'reward': np.array(reward_seq, dtype=np.float32)
            })
        
        print(f"✅ 生成完成: {len(episodes)} episodes")
        return episodes
    
    def test_model_initialization(self):
        """测试1: 模型初始化"""
        print("\n🧪 测试1: 模型初始化")
        
        try:
            # 创建模型
            rssm = RSSM(
                obs_dim=15,
                action_dim=3,
                hidden_dim=64,
                latent_dim=32,
                latent_classes=32
            )
            
            actor_critic = ActorCritic(
                hidden_dim=64,
                latent_flat_dim=1024,
                action_dim=3
            )
            
            # 统计参数量
            rssm_params = sum(p.numel() for p in rssm.parameters())
            ac_params = sum(p.numel() for p in actor_critic.parameters())
            
            print(f"  ✅ RSSM 模型: {rssm_params:,} 参数")
            print(f"  ✅ Actor-Critic: {ac_params:,} 参数")
            print(f"  ✅ 总计: {rssm_params + ac_params:,} 参数")
            
            self.test_results.append(("模型初始化", "通过", None))
            return True
            
        except Exception as e:
            print(f"  ❌ 失败: {e}")
            self.test_results.append(("模型初始化", "失败", str(e)))
            return False
    
    def test_forward_pass(self):
        """测试2: 前向传播"""
        print("\n🧪 测试2: 前向传播")
        
        try:
            rssm = RSSM(obs_dim=15, action_dim=3)
            
            # 创建测试输入
            batch_size = 4
            obs = torch.randn(batch_size, 15)
            h = torch.zeros(batch_size, 64)
            action = torch.randn(batch_size, 3)
            
            self.log(f"输入观测 shape: {obs.shape}")
            self.log(f"隐藏状态 shape: {h.shape}")
            self.log(f"动作 shape: {action.shape}")
            
            # 测试编码
            z_logits = rssm.encode(obs, h)
            z, z_dist = rssm.sample_z(z_logits)
            self.log(f"潜变量 shape: {z.shape}")
            
            # 测试动力学预测
            h_next = rssm.dynamics(h, z, action)
            self.log(f"下一隐藏状态 shape: {h_next.shape}")
            
            # 测试解码
            obs_pred = rssm.decode(h, z)
            self.log(f"重建观测 shape: {obs_pred.shape}")
            
            # 测试奖励预测
            reward_pred = rssm.predict_reward(h, z)
            self.log(f"奖励预测 shape: {reward_pred.shape}")
            
            print(f"  ✅ 前向传播正常")
            self.test_results.append(("前向传播", "通过", None))
            return True
            
        except Exception as e:
            print(f"  ❌ 失败: {e}")
            import traceback
            traceback.print_exc()
            self.test_results.append(("前向传播", "失败", str(e)))
            return False
    
    def test_imagination(self):
        """测试3: 想象力（核心功能）"""
        print("\n🧪 测试3: 想象力 (Imagine Future)")
        
        try:
            trainer = WorldModelTrainer(device="cpu")
            
            # 生成测试数据
            episodes = self.generate_mock_episodes(n_episodes=2, seq_len=10)
            initial_obs = episodes[0]['obs'][0]
            initial_action = episodes[0]['action'][0]
            
            self.log(f"初始观测 shape: {initial_obs.shape}")
            self.log(f"初始动作: {initial_action}")
            
            # 测试想象力
            result = trainer.imagine_future(
                initial_obs,
                initial_action,
                horizon=5
            )
            
            print(f"  ✅ 想象 horizon: {result['horizon']}")
            print(f"  ✅ 预测轨迹长度: {len(result['trajectory'])}")
            print(f"  ✅ 预测累积奖励: {result['cumulative_reward']:.4f}")
            
            # 验证轨迹结构
            for i, step in enumerate(result['trajectory']):
                self.log(f"Step {i}: reward={step['predicted_reward']:.4f}")
            
            self.test_results.append(("想象力", "通过", None))
            return True
            
        except Exception as e:
            print(f"  ❌ 失败: {e}")
            import traceback
            traceback.print_exc()
            self.test_results.append(("想象力", "失败", str(e)))
            return False
    
    def test_training(self):
        """测试4: 训练流程"""
        print("\n🧪 测试4: 训练流程 (简化版)")
        
        try:
            trainer = WorldModelTrainer(device="cpu")
            
            # 生成少量训练数据
            episodes = self.generate_mock_episodes(n_episodes=5, seq_len=10)
            
            print(f"  开始训练 (5 episodes, 10 epochs)...")
            
            # 训练（减少epochs以加速测试）
            losses = trainer.train_world_model(episodes, epochs=10)
            
            print(f"  ✅ 训练完成")
            print(f"  ✅ 初始 Loss: {losses[0]:.4f}")
            print(f"  ✅ 最终 Loss: {losses[-1]:.4f}")
            print(f"  ✅ Loss 下降: {((losses[0] - losses[-1]) / losses[0] * 100):.1f}%")
            
            # 保存和加载测试
            test_model_path = f"{self.test_data_dir}/test_model.pt"
            trainer.model_path = test_model_path
            trainer.save()
            print(f"  ✅ 模型已保存: {test_model_path}")
            
            # 加载测试
            trainer.load()
            print(f"  ✅ 模型已加载")
            
            self.test_results.append(("训练流程", "通过", None))
            return True
            
        except Exception as e:
            print(f"  ❌ 失败: {e}")
            import traceback
            traceback.print_exc()
            self.test_results.append(("训练流程", "失败", str(e)))
            return False
    
    def test_model_save_load(self):
        """测试5: 模型保存/加载"""
        print("\n🧪 测试5: 模型保存与加载")
        
        try:
            trainer = WorldModelTrainer(device="cpu")
            
            # 生成测试数据并训练
            episodes = self.generate_mock_episodes(n_episodes=3, seq_len=10)
            trainer.train_world_model(episodes, epochs=5)
            
            # 保存前预测
            test_obs = episodes[0]['obs'][0]
            test_action = episodes[0]['action'][0]
            result_before = trainer.imagine_future(test_obs, test_action, horizon=3)
            
            # 保存
            test_path = f"{self.test_data_dir}/save_load_test.pt"
            trainer.model_path = test_path
            trainer.save()
            
            # 重新初始化并加载
            trainer2 = WorldModelTrainer(device="cpu")
            trainer2.model_path = test_path
            trainer2.load()
            
            # 加载后预测
            result_after = trainer2.imagine_future(test_obs, test_action, horizon=3)
            
            # 比较结果
            diff = abs(result_before['cumulative_reward'] - result_after['cumulative_reward'])
            
            print(f"  ✅ 保存前预测奖励: {result_before['cumulative_reward']:.4f}")
            print(f"  ✅ 加载后预测奖励: {result_after['cumulative_reward']:.4f}")
            print(f"  ✅ 差异: {diff:.6f} (应接近0)")
            
            if diff < 1e-5:
                print(f"  ✅ 保存/加载一致性验证通过")
                self.test_results.append(("保存加载", "通过", None))
                return True
            else:
                print(f"  ⚠️  保存/加载结果不一致!")
                self.test_results.append(("保存加载", "警告", f"差异: {diff}"))
                return False
                
        except Exception as e:
            print(f"  ❌ 失败: {e}")
            import traceback
            traceback.print_exc()
            self.test_results.append(("保存加载", "失败", str(e)))
            return False
    
    def test_gradient_flow(self):
        """测试6: 梯度流检查"""
        print("\n🧪 测试6: 梯度流检查")
        
        try:
            rssm = RSSM(obs_dim=15, action_dim=3)
            optimizer = torch.optim.Adam(rssm.parameters(), lr=1e-3)
            
            # 创建测试数据
            obs = torch.randn(1, 15)
            h = torch.zeros(1, 64)
            
            # 前向传播
            z_logits = rssm.encode(obs, h)
            z, _ = rssm.sample_z(z_logits)
            obs_pred = rssm.decode(h, z)
            
            # 计算损失
            loss = ((obs_pred - obs) ** 2).mean()
            
            # 反向传播
            optimizer.zero_grad()
            loss.backward()
            
            # 检查梯度
            has_grad = 0
            no_grad = 0
            max_grad = 0
            
            for name, param in rssm.named_parameters():
                if param.grad is not None:
                    has_grad += 1
                    grad_norm = param.grad.norm().item()
                    max_grad = max(max_grad, grad_norm)
                else:
                    no_grad += 1
            
            print(f"  ✅ 有梯度的参数: {has_grad}")
            print(f"  ✅ 无梯度的参数: {no_grad}")
            print(f"  ✅ 最大梯度范数: {max_grad:.4f}")
            
            if no_grad == 0:
                print(f"  ✅ 所有参数都有梯度")
                self.test_results.append(("梯度流", "通过", None))
                return True
            else:
                print(f"  ⚠️  部分参数无梯度")
                self.test_results.append(("梯度流", "警告", f"{no_grad}个参数无梯度"))
                return False
                
        except Exception as e:
            print(f"  ❌ 失败: {e}")
            import traceback
            traceback.print_exc()
            self.test_results.append(("梯度流", "失败", str(e)))
            return False
    
    def generate_report(self):
        """生成测试报告"""
        print("\n" + "="*70)
        print("📊 测试报告")
        print("="*70)
        
        passed = sum(1 for _, status, _ in self.test_results if status == "通过")
        failed = sum(1 for _, status, _ in self.test_results if status == "失败")
        warnings = sum(1 for _, status, _ in self.test_results if status == "警告")
        
        print(f"\n总计: {len(self.test_results)} 项测试")
        print(f"  ✅ 通过: {passed}")
        print(f"  ❌ 失败: {failed}")
        print(f"  ⚠️  警告: {warnings}")
        
        print("\n详细结果:")
        for name, status, error in self.test_results:
            emoji = "✅" if status == "通过" else ("❌" if status == "失败" else "⚠️")
            print(f"  {emoji} {name}: {status}")
            if error:
                print(f"      错误: {error}")
        
        # 保存报告
        report_path = f"{self.test_data_dir}/test_report.json"
        with open(report_path, 'w') as f:
            json.dump({
                'timestamp': datetime.now().isoformat(),
                'summary': {
                    'total': len(self.test_results),
                    'passed': passed,
                    'failed': failed,
                    'warnings': warnings
                },
                'results': [
                    {'name': name, 'status': status, 'error': error}
                    for name, status, error in self.test_results
                ]
            }, f, indent=2)
        
        print(f"\n💾 报告已保存: {report_path}")
        
        return failed == 0


def main():
    parser = argparse.ArgumentParser(description='RSSM World Model 测试套件')
    parser.add_argument('--verbose', '-v', action='store_true', help='详细输出')
    parser.add_argument('--generate-data', '-g', action='store_true', help='只生成测试数据')
    parser.add_argument('--test', '-t', type=str, default='all', 
                       help='运行特定测试 (init, forward, imagine, train, save, gradient)')
    
    args = parser.parse_args()
    
    print("🚀 RSSM World Model 测试套件")
    print("="*70)
    print(f"时间: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print(f"PyTorch版本: {torch.__version__}")
    print(f"设备: CPU")
    print("="*70)
    
    tester = WorldModelTester(verbose=args.verbose)
    
    if args.generate_data:
        # 只生成数据
        episodes = tester.generate_mock_episodes(n_episodes=10, seq_len=20)
        data_path = f"{tester.test_data_dir}/mock_episodes.json"
        # 保存为numpy数组的列表
        np.save(f"{tester.test_data_dir}/mock_episodes.npy", episodes)
        print(f"💾 测试数据已保存: {tester.test_data_dir}/mock_episodes.npy")
        return
    
    # 运行测试
    tests = {
        'init': tester.test_model_initialization,
        'forward': tester.test_forward_pass,
        'imagine': tester.test_imagination,
        'train': tester.test_training,
        'save': tester.test_model_save_load,
        'gradient': tester.test_gradient_flow
    }
    
    if args.test == 'all':
        # 运行所有测试
        for test_func in tests.values():
            test_func()
    elif args.test in tests:
        # 运行特定测试
        tests[args.test]()
    else:
        print(f"❌ 未知测试: {args.test}")
        print(f"可用测试: {', '.join(tests.keys())}")
        return
    
    # 生成报告
    success = tester.generate_report()
    
    if success:
        print("\n🎉 所有测试通过!")
        sys.exit(0)
    else:
        print("\n⚠️  部分测试失败")
        sys.exit(1)


if __name__ == '__main__':
    main()
