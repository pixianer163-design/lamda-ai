#!/usr/bin/env python3
"""
Agent Templates - 预定义Agent模板
"""

from dataclasses import dataclass, field
from typing import List, Dict, Any, Optional


@dataclass
class AgentTemplate:
    name: str
    description: str
    market: str
    default_stocks: List[str]
    risk_profile: str
    strategy_type: str
    llm_model: str
    schedule: str
    risk_config: Dict[str, Any]
    strategy_config: Dict[str, Any]


hktech_basic = AgentTemplate(
    name="hktech_basic",
    description="基础型HKTech Agent，适合入门用户，默认配置",
    market="HK",
    default_stocks=["0700.HK", "9988.HK", "3690.HK"],
    risk_profile="moderate",
    strategy_type="balanced",
    llm_model="gpt-4",
    schedule="daily",
    risk_config={
        "max_position_size": 0.1,
        "max_loss_per_trade": 0.02,
        "stop_loss_pct": 0.05,
        "max_daily_trades": 5
    },
    strategy_config={
        "momentum_lookback": 20,
        "volume_threshold": 1.5,
        "news_sentiment_weight": 0.3,
        "technical_weight": 0.4,
        "fundamental_weight": 0.3
    }
)


hktech_conservative = AgentTemplate(
    name="hktech_conservative",
    description="保守型HKTech Agent，注重风险控制，追求稳定收益",
    market="HK",
    default_stocks=["0700.HK", "9988.HK", "0005.HK", "939.HK"],
    risk_profile="conservative",
    strategy_type="low_volatility",
    llm_model="gpt-4",
    schedule="daily",
    risk_config={
        "max_position_size": 0.05,
        "max_loss_per_trade": 0.01,
        "stop_loss_pct": 0.03,
        "max_daily_trades": 3
    },
    strategy_config={
        "momentum_lookback": 60,
        "volume_threshold": 2.0,
        "news_sentiment_weight": 0.2,
        "technical_weight": 0.5,
        "fundamental_weight": 0.3,
        "min_dividend_yield": 0.03
    }
)


hktech_aggressive = AgentTemplate(
    name="hktech_aggressive",
    description="激进型HKTech Agent，追求高收益，能够承受较大波动",
    market="HK",
    default_stocks=["9988.HK", "3690.HK", "1024.HK", "1810.HK", "2618.HK"],
    risk_profile="aggressive",
    strategy_type="momentum",
    llm_model="gpt-4",
    schedule="intraday",
    risk_config={
        "max_position_size": 0.2,
        "max_loss_per_trade": 0.05,
        "stop_loss_pct": 0.08,
        "max_daily_trades": 15
    },
    strategy_config={
        "momentum_lookback": 10,
        "volume_threshold": 1.2,
        "news_sentiment_weight": 0.4,
        "technical_weight": 0.4,
        "fundamental_weight": 0.2,
        "leverage": 1.5
    }
)


custom = AgentTemplate(
    name="custom",
    description="自定义模板，用户可完全自定义所有参数",
    market="HK",
    default_stocks=[],
    risk_profile="moderate",
    strategy_type="balanced",
    llm_model="gpt-4",
    schedule="daily",
    risk_config={
        "max_position_size": 0.1,
        "max_loss_per_trade": 0.02,
        "stop_loss_pct": 0.05,
        "max_daily_trades": 5
    },
    strategy_config={
        "momentum_lookback": 20,
        "volume_threshold": 1.5,
        "news_sentiment_weight": 0.3,
        "technical_weight": 0.4,
        "fundamental_weight": 0.3
    }
)


_TEMPLATES = {
    "hktech_basic": hktech_basic,
    "hktech_conservative": hktech_conservative,
    "hktech_aggressive": hktech_aggressive,
    "custom": custom
}


def get_template(name: str) -> Optional[AgentTemplate]:
    return _TEMPLATES.get(name)


def list_templates() -> List[str]:
    return list(_TEMPLATES.keys())


def get_all_templates() -> Dict[str, AgentTemplate]:
    return _TEMPLATES.copy()
