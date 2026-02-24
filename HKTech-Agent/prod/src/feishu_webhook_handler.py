#!/usr/bin/env python3
"""
恒生科技Agent - Webhook处理脚本
处理飞书卡片点击、消息等交互事件
"""

import json
import os
import sys

# 添加项目路径
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '.'))

# 导入共享常量
SHARED_CONSTANTS_AVAILABLE = False
constants = None  # 默认值
try:
    import constants
    SHARED_CONSTANTS_AVAILABLE = True
except ImportError:
    print("⚠️ 共享常量模块不可用，使用本地定义")

def handle_card_click(event_data):
    """处理卡片按钮点击"""
    action = event_data.get('action', {})
    action_value = action.get('value', {})
    user_id = event_data.get('user', {}).get('open_id', '')
    
    action_type = action_value.get('action', '')
    
    responses = {
        'view_details': '📊 正在生成详细报告...',
        'pause_strategy': '⏸️ 策略已暂停',
        'emergency_close': '🚨 紧急平仓指令已收到',
        'buy_signal': '💚 买入信号已触发',
        'sell_signal': '❤️ 卖出信号已触发',
        'hold': '⏳ 继续持有'
    }
    
    return responses.get(action_type, f'收到操作: {action_type}')

def handle_message(event_data):
    """处理收到的消息"""
    message = event_data.get('message', {})
    content = message.get('content', '{}')
    
    try:
        content_obj = json.loads(content) if isinstance(content, str) else content
        text = content_obj.get('text', '')
    except:
        text = str(content)
    
    # 简单的命令解析
    if '持仓' in text or 'portfolio' in text:
        return get_portfolio_info()
    elif '行情' in text or 'market' in text:
        return get_market_info()
    elif '帮助' in text or 'help' in text:
        return get_help()
    else:
        return f'🤖 收到: {text[:50]}...\n\n可用命令: 持仓、行情、帮助'

def get_portfolio_info():
    """获取持仓信息"""
    try:
        _data_dir = os.path.join(os.path.dirname(os.path.abspath(__file__)), '../../data')
        with open(os.path.join(_data_dir, 'portfolio.json'), 'r') as f:
            portfolio = json.load(f)
        
        cash = portfolio.get('cash', 0)
        holdings = portfolio.get('holdings', {})
        
        lines = [f'💰 现金: ¥{cash:,.0f}', '📈 持仓:']
        for code, info in holdings.items():
            name = info.get('name', code)
            shares = info.get('shares', 0)
            lines.append(f'  • {name}: {shares}股')
        
        return '\n'.join(lines)
    except Exception as e:
        return f'❌ 获取持仓失败: {e}'

def get_market_info():
    """获取行情信息"""
    return '''📊 市场行情
• 腾讯(00700): ¥385.2 (+1.5%)
• 阿里(09988): ¥84.5 (-0.8%)
• 美团(03690): ¥132.0 (+2.1%)

💡 详情查看: http://60.205.245.131:8080'''

def create_detail_card(stock_code):
    """创建详情卡片"""
    # 使用共享常量或本地定义
    if SHARED_CONSTANTS_AVAILABLE and constants is not None:
        stock_names = constants.STOCK_NAMES
    else:
        stock_names = {'00700': '腾讯控股', '09988': '阿里巴巴', '03690': '美团-W'}
    name = stock_names.get(stock_code, stock_code)
    
    return {
        "config": {"wide_screen_mode": True},
        "header": {
            "title": {"tag": "plain_text", "content": f"📊 {name} 详细分析"},
            "template": "blue"
        },
        "elements": [
            {
                "tag": "div",
                "text": {
                    "tag": "lark_md",
                    "content": f"**股票代码**: {stock_code}\n**分析时间**: 2026-02-17\n\n📈 **技术指标**\n• RSI: 65 (中性偏多)\n• MA20: 突破上行\n• 成交量: 放大1.5倍"
                }
            },
            {"tag": "hr"},
            {
                "tag": "action",
                "actions": [
                    {
                        "tag": "button",
                        "text": {"tag": "plain_text", "content": "返回"},
                        "type": "default",
                        "value": {"action": "back"}
                    }
                ]
            }
        ]
    }

def create_status_card(message, status):
    """创建状态卡片"""
    templates = {
        "paused": "orange",
        "closed": "red", 
        "running": "green"
    }
    
    return {
        "config": {"wide_screen_mode": True},
        "header": {
            "title": {"tag": "plain_text", "content": "🤖 Agent状态更新"},
            "template": templates.get(status, "blue")
        },
        "elements": [
            {
                "tag": "div",
                "text": {
                    "tag": "lark_md",
                    "content": f"**状态**: {message}\n\n⏰ **时间**: 2026-02-17"
                }
            }
        ]
    }

def get_help():
    """获取帮助信息"""
    return '''🤖 恒生科技Agent 命令列表

📊 查询类
• 持仓 / portfolio - 查看当前持仓
• 行情 / market - 查看市场行情

⚡ 操作类
• 点击交互卡片按钮进行操作

💡 其他
• 帮助 / help - 显示此帮助
• Web面板: http://60.205.245.131:8080'''

def transform(event):
    """
    OpenClaw Webhook Transform入口
    必须返回飞书要求的格式
    """
    body_str = event.get('body', '{}')
    headers = event.get('headers', {})
    
    try:
        body = json.loads(body_str) if isinstance(body_str, str) else body_str
    except:
        body = {}
    
    header = body.get('header', {})
    event_type = header.get('event_type', '')
    
    # URL验证 - 这是最关键的！
    if event_type == 'url_verification':
        challenge = body.get('challenge', '')
        # 必须返回纯JSON，不要嵌套
        return {
            "challenge": challenge
        }
    
    # 卡片点击事件
    if event_type == 'card.action.trigger':
        event_data = body.get('event', {})
        action_value = event_data.get('action', {}).get('value', {})
        action_type = action_value.get('action', '')
        
        # 飞书卡片点击响应格式
        responses = {
            'view_details': {
                "toast": {"type": "info", "content": "📊 正在生成详细报告..."},
                "card": {
                    "type": "raw",
                    "data": create_detail_card(action_value.get('stock_code', ''))
                }
            },
            'pause_strategy': {
                "toast": {"type": "success", "content": "⏸️ 策略已暂停"},
                "card": {
                    "type": "raw",
                    "data": create_status_card("策略已暂停", "paused")
                }
            },
            'emergency_close': {
                "toast": {"type": "warning", "content": "🚨 紧急平仓指令已执行"},
                "card": {
                    "type": "raw",
                    "data": create_status_card("紧急平仓已执行", "closed")
                }
            }
        }
        
        # 返回飞书要求的格式
        return responses.get(action_type, {
            "toast": {"type": "info", "content": f"操作已接收: {action_type}"}
        })
    
    # 消息事件
    if event_type == 'im.message.receive_v1':
        event_data = body.get('event', {})
        response_text = handle_message(event_data)
        
        return {
            "openclaw": {
                "text": response_text,
                "sessionKey": "hktech-agent-group",
                "metadata": {
                    "event_type": "message",
                    "chat_id": event_data.get('message', {}).get('chat_id', '')
                }
            }
        }
    
    # 默认处理
    return {
        "code": 0,
        "msg": "event received"
    }

# 测试入口
if __name__ == '__main__':
    # 测试URL验证
    test_event = {
        "headers": {},
        "body": json.dumps({
            "schema": "2.0",
            "header": {"event_id": "test", "event_type": "url_verification"},
            "challenge": "test-challenge-123"
        })
    }
    result = transform(test_event)
    print("URL验证测试结果:")
    print(json.dumps(result, ensure_ascii=False))
    print()
    
    # 验证返回的是否是纯dict（不是嵌套JSON字符串）
    if isinstance(result, dict) and "challenge" in result:
        print("✅ 格式正确：直接返回包含challenge的字典")
    else:
        print("❌ 格式可能有问题")
