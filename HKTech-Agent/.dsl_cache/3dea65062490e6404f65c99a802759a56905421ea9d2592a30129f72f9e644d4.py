# Auto-generated strategy from DSL
# Generated at: 2026-02-28T18:20:00.165107

import numpy as np
import pandas as pd
from typing import Dict, Any, Optional
from dataclasses import dataclass

@dataclass
class Signal:
    timestamp: pd.Timestamp
    action: str  # BUY, SELL, HOLD, CLOSE
    confidence: float
    position_size: float
    metadata: Dict[str, Any]


class RsimeanreversionStrategy:
    """
    RSI 超买超卖均值回归策略
    """
    
    def __init__(self, rsi_period=14, rsi_oversold=30, rsi_overbought=70):
        self.name = "RSIMeanReversion"
        self.indicators = {}
        self.params = {}
        self.params['rsi_period'] = rsi_period
        self.params['rsi_oversold'] = rsi_oversold
        self.params['rsi_overbought'] = rsi_overbought
        
        # Position management
        self.max_position = 0.6
        self.risk_per_trade = 2
        self.trailing_stop = 5
        
        # Track state
        self.position = 0
        self.entry_price = 0.0
        self.last_signal = None
    
    def calculate_indicators(self, data: pd.DataFrame) -> Dict[str, pd.Series]:
        """Calculate all indicators"""
        self.indicators['rsi'] = self.rsi(data)
        return self.indicators
    

    def rsi(self, data):
        period = self.params['rsi_period']
        delta = data['close'].diff()
        gain = (delta.where(delta > 0, 0)).rolling(window=period).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=period).mean()
        rs = gain / loss
        return 100 - (100 / (1 + rs))

    
    def generate_signal(self, data: pd.DataFrame) -> Optional[Signal]:
        """Generate trading signal"""
        # Update indicators
        self.calculate_indicators(data)
        
        # Check exit rules first
        exit_signal = self.check_exit_rules(data)
        if exit_signal:
            return exit_signal
        
        # Check entry conditions
        signal = self.condition_0(data)
        if signal:
            signal.position_size = self.calculate_position_size(100000, signal.confidence)
            self.update_position(signal)
            return signal
        signal = self.condition_1(data)
        if signal:
            signal.position_size = self.calculate_position_size(100000, signal.confidence)
            self.update_position(signal)
            return signal
        
        return None
    

    def condition_0(self, data: pd.DataFrame) -> Optional[Signal]:
        """RSI Oversold Entry"""
        if (self.indicators['rsi'].iloc[-1] < self.params['rsi_oversold']):
            return Signal(
                timestamp=data.index[-1],
                action="BUY",
                confidence=0.8,
                position_size=0.5,
                metadata={
                    'condition': 'condition_0',
                    'priority': 1
                }
            )
        return None


    def condition_1(self, data: pd.DataFrame) -> Optional[Signal]:
        """RSI Overbought Exit"""
        if (self.indicators['rsi'].iloc[-1] > self.params['rsi_overbought']):
            return Signal(
                timestamp=data.index[-1],
                action="SELL",
                confidence=0.8,
                position_size=0.5,
                metadata={
                    'condition': 'condition_1',
                    'priority': 1
                }
            )
        return None

    

    def exit_rule_0(self, data: pd.DataFrame) -> Optional[Signal]:
        """Exit rule: SIGNAL"""
        if self.position != 0 and (self.indicators['rsi'].iloc[-1] > 50):
            return Signal(
                timestamp=data.index[-1],
                action='CLOSE',
                confidence=1.0,
                position_size=abs(self.position),
                metadata={'exit_type': 'SIGNAL', 'rule': 'exit_0'}
            )
        return None

    
    def check_exit_rules(self, data: pd.DataFrame) -> Optional[Signal]:
        """Check exit conditions"""
        signal = self.exit_rule_0(data)
        if signal:
            self.update_position(signal)
            return signal
        return None
    
    def calculate_position_size(self, capital: float, confidence: float, data: Optional[pd.DataFrame] = None) -> float:
        """
        Calculate position size based on risk management
        
        Args:
            capital: Total capital available
            confidence: Signal confidence (0-1)
            data: Optional market data for price lookup
        
        Returns:
            Position size (number of shares)
        """
        risk_amount = capital * self.risk_per_trade
        
        if self.position > 0:
            # Already in position, check if we should add
            max_position_value = capital * self.max_position
            current_price = data['close'].iloc[-1] if data is not None else 100.0
            current_value = self.position * current_price
            available = max_position_value - current_value
            return min(available, risk_amount * confidence * 10)
        else:
            # New position
            max_position_value = capital * self.max_position
            position_value = min(max_position_value, risk_amount * confidence * 10)
            current_price = data['close'].iloc[-1] if data is not None else 100.0
            return position_value / current_price
    
    def update_position(self, signal: Signal):
        """Update internal position tracking"""
        if signal.action == 'BUY':
            self.position += signal.position_size
            if self.entry_price == 0:
                self.entry_price = signal.price if hasattr(signal, 'price') else 0
        elif signal.action == 'SELL':
            self.position -= signal.position_size
            if self.position <= 0:
                self.position = 0
                self.entry_price = 0
        elif signal.action == 'CLOSE':
            self.position = 0
            self.entry_price = 0
        
        self.last_signal = signal
