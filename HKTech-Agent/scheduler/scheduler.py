#!/usr/bin/env python3
"""
Agent 内置任务调度器
实现可靠的任务调度、状态追踪、错过补偿机制
"""

import json
import os
import sys
import subprocess
import logging
from datetime import datetime, timedelta
from pathlib import Path
from typing import Dict, List, Optional, Any
import croniter


class TaskConfig:
    """任务配置类"""
    
    def __init__(self, task_data: Dict):
        self.id = task_data['id']
        self.name = task_data['name']
        self.description = task_data.get('description', '')
        self.enabled = task_data.get('enabled', True)
        self.schedule = task_data.get('schedule', {})
        self.compensation = task_data.get('compensation', {})
        self.execution = task_data.get('execution', {})
        self.prompt = task_data.get('prompt', '')
    
    def get_next_run_time(self, now: datetime) -> datetime:
        """获取下次执行时间"""
        if self.schedule.get('type') == 'cron':
            expr = self.schedule.get('expression', '* * * * *')
            tz_str = self.schedule.get('timezone', 'Asia/Shanghai')
            # 使用 croniter 计算下次执行时间
            base = now.replace(second=0, microsecond=0)
            cron = croniter.croniter(expr, base)
            return cron.get_next(datetime)
        return now
    
    def is_compensation_enabled(self) -> bool:
        """是否启用补偿机制"""
        return self.compensation.get('enabled', True)
    
    def get_compensation_window(self) -> int:
        """获取补偿窗口（分钟）"""
        return self.compensation.get('window_minutes', 30)
    
    def get_simplified_threshold(self) -> int:
        """获取简化流程阈值（分钟）"""
        return self.compensation.get('simplified_threshold_minutes', 15)


class ExecutionResult:
    """执行结果类"""
    
    def __init__(self, task_id: str, status: str, **kwargs):
        self.task_id = task_id
        self.status = status  # success, failed, skipped, compensated
        self.scheduled_time = kwargs.get('scheduled_time')
        self.started_time = kwargs.get('started_time')
        self.completed_time = kwargs.get('completed_time')
        self.duration_seconds = kwargs.get('duration_seconds', 0)
        self.session_key = kwargs.get('session_key')
        self.error = kwargs.get('error')
        self.compensated = kwargs.get('compensated', False)
        self.simplified = kwargs.get('simplified', False)
    
    def to_dict(self) -> Dict:
        """转换为字典"""
        return {
            'task_id': self.task_id,
            'status': self.status,
            'scheduled_time': self.scheduled_time.isoformat() if self.scheduled_time else None,
            'started_time': self.started_time.isoformat() if self.started_time else None,
            'completed_time': self.completed_time.isoformat() if self.completed_time else None,
            'duration_seconds': self.duration_seconds,
            'session_key': self.session_key,
            'error': self.error,
            'compensated': self.compensated,
            'simplified': self.simplified
        }


class StateManager:
    """状态管理器"""
    
    def __init__(self, state_path: str, execution_log_path: str):
        self.state_path = Path(state_path)
        self.execution_log_path = Path(execution_log_path)
        
        # 确保目录存在
        self.state_path.parent.mkdir(parents=True, exist_ok=True)
        self.execution_log_path.parent.mkdir(parents=True, exist_ok=True)
    
    def load_state(self, date: datetime.date) -> Dict:
        """加载指定日期的状态"""
        if not self.state_path.exists():
            return {'date': date.isoformat(), 'executed_tasks': [], 'stats': {}}
        
        try:
            with open(self.state_path, 'r', encoding='utf-8') as f:
                state = json.load(f)
            
            # 检查是否是今天的状态
            if state.get('date') != date.isoformat():
                return {'date': date.isoformat(), 'executed_tasks': [], 'stats': {}}
            
            return state
        except Exception as e:
            logging.warning(f"加载状态文件失败：{e}")
            return {'date': date.isoformat(), 'executed_tasks': [], 'stats': {}}
    
    def save_state(self, state: Dict):
        """保存状态"""
        temp_path = self.state_path.with_suffix('.tmp')
        try:
            with open(temp_path, 'w', encoding='utf-8') as f:
                json.dump(state, f, indent=2, ensure_ascii=False)
            temp_path.replace(self.state_path)
        except Exception as e:
            logging.error(f"保存状态文件失败：{e}")
            if temp_path.exists():
                temp_path.unlink()
    
    def record_execution(self, result: ExecutionResult):
        """记录执行日志（JSONL 格式）"""
        try:
            with open(self.execution_log_path, 'a', encoding='utf-8') as f:
                f.write(json.dumps(result.to_dict(), ensure_ascii=False) + '\n')
        except Exception as e:
            logging.error(f"记录执行日志失败：{e}")
    
    def is_executed_today(self, task_id: str, state: Dict) -> bool:
        """检查任务今日是否已执行"""
        executed_tasks = state.get('executed_tasks', [])
        return task_id in executed_tasks
    
    def mark_executed(self, task_id: str, state: Dict):
        """标记任务已执行"""
        if 'executed_tasks' not in state:
            state['executed_tasks'] = []
        if task_id not in state['executed_tasks']:
            state['executed_tasks'].append(task_id)


class Scheduler:
    """任务调度器核心"""
    
    def __init__(self, config_path: str):
        self.config_path = Path(config_path)
        self.tasks: List[TaskConfig] = []
        self.state_manager: Optional[StateManager] = None
        self.logger = self._setup_logging()
        self._load_config()
    
    def _setup_logging(self) -> logging.Logger:
        """设置日志"""
        logger = logging.getLogger('scheduler')
        logger.setLevel(logging.INFO)
        
        # 控制台处理器
        console_handler = logging.StreamHandler()
        console_handler.setLevel(logging.INFO)
        formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
        console_handler.setFormatter(formatter)
        logger.addHandler(console_handler)
        
        return logger
    
    def _load_config(self):
        """加载配置文件"""
        try:
            with open(self.config_path, 'r', encoding='utf-8') as f:
                config = json.load(f)
            
            # 加载任务
            for task_data in config.get('tasks', []):
                self.tasks.append(TaskConfig(task_data))
            
            # 初始化状态管理器
            state_config = config.get('state', {})
            if state_config.get('enabled', True):
                self.state_manager = StateManager(
                    state_config.get('state_path', '/root/.openclaw/workspace/logs/scheduler/state.json'),
                    state_config.get('execution_log_path', '/root/.openclaw/workspace/logs/scheduler/executions.jsonl')
                )
            
            # 设置日志级别
            log_config = config.get('logging', {})
            if log_config.get('enabled'):
                log_path = log_config.get('log_path')
                if log_path:
                    Path(log_path).parent.mkdir(parents=True, exist_ok=True)
                    file_handler = logging.FileHandler(log_path, encoding='utf-8')
                    file_handler.setLevel(getattr(logging, log_config.get('level', 'INFO')))
                    file_handler.setFormatter(logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s'))
                    self.logger.addHandler(file_handler)
            
            self.logger.info(f"加载配置成功，共 {len(self.tasks)} 个任务")
            
        except Exception as e:
            self.logger.error(f"加载配置文件失败：{e}")
            raise
    
    def check_and_execute(self) -> List[ExecutionResult]:
        """检查并执行到期任务"""
        results = []
        now = datetime.now()
        today = now.date()
        
        self.logger.info(f"开始检查任务 (当前时间：{now})")
        
        # 加载今日状态
        state = self.state_manager.load_state(today) if self.state_manager else {}
        
        for task in self.tasks:
            if not task.enabled:
                self.logger.debug(f"任务 {task.name} 已禁用，跳过")
                continue
            
            # 检查今日是否已执行
            if self.state_manager and self.state_manager.is_executed_today(task.id, state):
                self.logger.info(f"任务 {task.name} 今日已执行，跳过")
                continue
            
            # 计算计划执行时间
            scheduled_time = task.get_next_run_time(now.replace(hour=0, minute=0, second=0, microsecond=0))
            
            # 检查是否到期
            if scheduled_time > now:
                self.logger.debug(f"任务 {task.name} 未到期 (计划：{scheduled_time})")
                continue
            
            # 检查补偿窗口
            missed_minutes = (now - scheduled_time).total_seconds() / 60
            compensation_window = task.get_compensation_window()
            
            if missed_minutes > compensation_window:
                self.logger.warning(f"任务 {task.name} 错过补偿窗口 ({missed_minutes:.0f}分钟 > {compensation_window}分钟)")
                continue
            
            # 执行任务
            self.logger.info(f"触发任务：{task.name} (错过{missed_minutes:.0f}分钟)")
            result = self._execute_task(task, missed_minutes > 0, missed_minutes > task.get_simplified_threshold())
            
            # 记录执行结果
            if self.state_manager:
                self.state_manager.record_execution(result)
                if result.status == 'success':
                    self.state_manager.mark_executed(task.id, state)
            
            results.append(result)
        
        # 保存状态
        if self.state_manager:
            self.state_manager.save_state(state)
        
        return results
    
    def _execute_task(self, task: TaskConfig, compensated: bool, simplified: bool) -> ExecutionResult:
        """执行单个任务"""
        started_time = datetime.now()
        
        try:
            # 检查是 command 模式还是 prompt 模式
            if 'command' in task.execution:
                # 直接执行命令（用于恒生 Agent 等脚本）
                cmd = task.execution['command']
                log_path = task.execution.get('log_path')
                
                # 执行命令
                if log_path:
                    with open(log_path, 'a') as log_file:
                        result = subprocess.run(
                            cmd,
                            shell=True,
                            stdout=log_file,
                            stderr=subprocess.STDOUT,
                            timeout=task.execution.get('timeout_seconds', 300) + 30
                        )
                else:
                    result = subprocess.run(
                        cmd,
                        shell=True,
                        capture_output=True,
                        text=True,
                        timeout=task.execution.get('timeout_seconds', 300) + 30
                    )
                
                session_key = None
            else:
                # sessions spawn 模式（用于 AI 任务）
                label = task.execution.get('label_template', '{name} - {date}').format(
                    name=task.name,
                    date=datetime.now().strftime('%Y-%m-%d')
                )
                
                cmd = [
                    'openclaw', 'sessions', 'spawn',
                    '--label', label,
                    '--model', task.execution.get('model', 'bailian/qwen3.5-plus'),
                    '--timeout', str(task.execution.get('timeout_seconds', 300)),
                    task.prompt
                ]
                
                # 执行命令
                result = subprocess.run(
                    cmd,
                    capture_output=True,
                    text=True,
                    timeout=task.execution.get('timeout_seconds', 300) + 30
                )
                
                # 解析 session_key
                session_key = self._parse_session_key(result.stdout)
            
            completed_time = datetime.now()
            duration = (completed_time - started_time).total_seconds()
            
            if result.returncode == 0:
                return ExecutionResult(
                    task_id=task.id,
                    status='success',
                    scheduled_time=task.get_next_run_time(datetime.now().replace(hour=0, minute=0)),
                    started_time=started_time,
                    completed_time=completed_time,
                    duration_seconds=duration,
                    session_key=session_key,
                    compensated=compensated,
                    simplified=simplified
                )
            else:
                error_msg = result.stderr if hasattr(result, 'stderr') and result.stderr else "Command failed"
                return ExecutionResult(
                    task_id=task.id,
                    status='failed',
                    scheduled_time=task.get_next_run_time(datetime.now().replace(hour=0, minute=0)),
                    started_time=started_time,
                    completed_time=completed_time,
                    duration_seconds=duration,
                    error=error_msg
                )
                
        except subprocess.TimeoutExpired:
            return ExecutionResult(
                task_id=task.id,
                status='failed',
                scheduled_time=task.get_next_run_time(datetime.now().replace(hour=0, minute=0)),
                started_time=started_time,
                completed_time=datetime.now(),
                duration_seconds=(datetime.now() - started_time).total_seconds(),
                error='执行超时'
            )
        except Exception as e:
            return ExecutionResult(
                task_id=task.id,
                status='failed',
                scheduled_time=task.get_next_run_time(datetime.now().replace(hour=0, minute=0)),
                started_time=started_time,
                completed_time=datetime.now(),
                duration_seconds=(datetime.now() - started_time).total_seconds(),
                error=str(e)
            )
    
    def _parse_session_key(self, output: str) -> Optional[str]:
        """从输出中解析 session_key"""
        # 解析 openclaw sessions spawn 的输出
        for line in output.split('\n'):
            if 'sessionKey' in line or 'childSessionKey' in line:
                # 提取 session key
                parts = line.split()
                for part in parts:
                    if part.startswith('agent:'):
                        return part
        return None
    
    def get_today_status(self) -> Dict:
        """获取今日任务执行状态"""
        today = datetime.now().date()
        state = self.state_manager.load_state(today) if self.state_manager else {}
        
        status = {
            'date': today.isoformat(),
            'tasks': []
        }
        
        for task in self.tasks:
            scheduled_time = task.get_next_run_time(datetime.now().replace(hour=0, minute=0))
            executed = self.state_manager.is_executed_today(task.id, state) if self.state_manager else False
            
            status['tasks'].append({
                'id': task.id,
                'name': task.name,
                'enabled': task.enabled,
                'scheduled_time': scheduled_time.isoformat(),
                'executed': executed
            })
        
        return status
    
    def manual_trigger(self, task_id: str) -> ExecutionResult:
        """手动触发任务执行"""
        task = next((t for t in self.tasks if t.id == task_id), None)
        if not task:
            raise ValueError(f"任务不存在：{task_id}")
        
        self.logger.info(f"手动触发任务：{task.name}")
        result = self._execute_task(task, compensated=False, simplified=False)
        
        if self.state_manager and result.status == 'success':
            state = self.state_manager.load_state(datetime.now().date())
            self.state_manager.record_execution(result)
            self.state_manager.mark_executed(task.id, state)
            self.state_manager.save_state(state)
        
        return result


def main():
    """主函数"""
    import argparse
    
    parser = argparse.ArgumentParser(description='Agent 内置任务调度器')
    parser.add_argument('--config', type=str, default='/root/.openclaw/workspace/Lamda-ai/HKTech-Agent/scheduler/task_config.json',
                       help='配置文件路径')
    parser.add_argument('--check', action='store_true', help='检查并执行到期任务')
    parser.add_argument('--status', action='store_true', help='查看今日状态')
    parser.add_argument('--trigger', type=str, help='手动触发指定任务')
    
    args = parser.parse_args()
    
    scheduler = Scheduler(args.config)
    
    if args.check:
        results = scheduler.check_and_execute()
        print(f"\n执行完成，共执行 {len(results)} 个任务")
        for result in results:
            print(f"  - {result.task_id}: {result.status}")
    
    elif args.status:
        status = scheduler.get_today_status()
        print(f"\n今日任务状态 ({status['date']}):")
        for task in status['tasks']:
            executed_mark = "✅" if task['executed'] else "⏳"
            enabled_mark = "🟢" if task['enabled'] else "🔴"
            print(f"  {enabled_mark} {task['name']}: {task['scheduled_time']} {executed_mark}")
    
    elif args.trigger:
        try:
            result = scheduler.manual_trigger(args.trigger)
            print(f"\n手动触发任务 {args.trigger}: {result.status}")
            if result.session_key:
                print(f"  Session Key: {result.session_key}")
        except Exception as e:
            print(f"\n手动触发失败：{e}")
    
    else:
        parser.print_help()


if __name__ == '__main__':
    main()
