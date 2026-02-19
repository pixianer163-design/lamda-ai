# HKTech Agent

基于LLM增强的港股量化交易Agent系统。

## 项目简介

HKTech Agent是一个智能股票交易Agent，集成了：
- **RSSM世界模型**: 预测未来市场走势
- **LLM信号提取**: 从新闻中提取情绪信号
- **LLM决策增强**: 智能决策辅助
- **飞书集成**: 交互式日报推送
- **Agent Factory**: 多Agent并行A/B测试

## 核心特性

| 特性 | 说明 |
|------|------|
| 🧠 世界模型 | RSSM递归状态空间模型预测未来5天收益 |
| 📰 LLM分析 | DeepSeek分析新闻情绪，提取交易信号 |
| 🤖 智能决策 | LLM辅助决策，综合技术与基本面 |
| 📊 A/B测试 | 3个Agent并行运行（基础/保守/激进） |
| 📱 飞书推送 | 交互式卡片日报，支持按钮操作 |
| ☁️ 云端训练 | 阿里云GPU训练与边缘推理 |

## 系统架构

```
┌─────────────────────────────────────────────────────────┐
│                    HKTech Agent                         │
├─────────────────────────────────────────────────────────┤
│  数据采集 → 世界模型预测 → LLM分析 → 决策 → 执行        │
├─────────────────────────────────────────────────────────┤
│  RSSM World Model                                       │
│  ├─ 状态编码: 历史价格 → 低维状态向量                   │
│  ├─ 未来预测: 预测5天收益分布                           │
│  └─ 不确定性: 知道"自己不知道什么"                      │
├─────────────────────────────────────────────────────────┤
│  LLM增强层                                              │
│  ├─ 信号提取: 新闻情绪分析                              │
│  └─ 决策增强: 综合判断与风险提示                        │
├─────────────────────────────────────────────────────────┤
│  Agent Factory                                          │
│  ├─ Agent A: 基础版 (多因子)                            │
│  ├─ Agent B: 保守版 (价值)                              │
│  └─ Agent C: 激进版 (动量)                              │
└─────────────────────────────────────────────────────────┘
```

## 运行方式

### 自动运行（推荐）
```bash
# 工作日 9:30 自动运行
crontab -l
# 30 9 * * 1-5 cd /opt/hktech-agent/prod && ./run_prod.sh
```

### 手动运行
```bash
cd prod
./run_prod.sh
```

## 项目结构

```
HKTech-Agent/
├── prod/                   # 生产环境
│   ├── src/               # 核心代码
│   │   ├── llm_enhanced_agent.py
│   │   ├── rssm_world_model.py
│   │   ├── world_model_integration.py
│   │   └── daily_report_sender.py
│   └── run_prod.sh        # 运行脚本
├── active_src/            # 活跃开发代码
│   ├── data_collector.py  # 数据采集
│   ├── feishu_sender.py   # 飞书推送
│   └── ...
├── factory/               # Agent Factory
│   ├── agent_factory.py   # 工厂核心
│   ├── core/              # 核心模块
│   └── configs/           # Agent配置
├── web/                   # Web界面
│   └── docs/              # 文档中心
├── config/                # 配置文件
└── data/                  # 数据目录
```

## 配置说明

### 飞书配置
```json
{
  "app_id": "cli_a919022256b8dcd6",
  "chat_id": "oc_d5f6f6f591bc129e4ae9037b0acdd3a5",
  "enabled": true
}
```

### Cron定时
```bash
# 工作日 9:30 运行
30 9 * * 1-5 cd /opt/hktech-agent/prod && ./run_prod.sh
```

## Web面板

- **监控面板**: http://60.205.245.131:8080/
- **文档中心**: http://60.205.245.131:8080/web/docs/index.html

## 相关项目

- **Agent Factory**: 本仓库的 Agent_Factory/ 目录
- **DPML世界模型**: 下一代元学习世界模型（开发中）
- **DSL策略引擎**: 策略描述语言（规划中）

## 开发历史

- **2026-02-17**: 初始版本，基础Agent框架
- **2026-02-18**: Agent Factory平台化重构
- **2026-02-19**: 
  - 修复数据不更新bug（实时API获取）
  - 修复卡片按钮无反应
  - 统一Web服务到8080端口
  - 推送到GitHub

## 协作模式

```
需求 → Alex架构设计 → GitHub → Code Agent开发 → 集成验证
```

---

*创建日期: 2026-02-19*  
*架构设计: Alex*  
*核心开发: Alex + Code Agent*
