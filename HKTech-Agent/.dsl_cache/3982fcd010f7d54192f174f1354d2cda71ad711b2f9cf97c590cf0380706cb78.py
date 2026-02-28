# Auto-generated strategy from DSL
# Generated at: 2026-02-28T10:48:43.211694

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


class Test1Strategy:
    """
    Auto-generated strategy: Test1
    """
    
    def __init__(self, period=14):
        self.name = "Test1"
        self.indicators = {}
        self.params = {}
        self.params['period'] = period
        
        # Position management
        self.max_position = 0.5
        self.risk_per_trade = 2
        
        # Track state
        self.position = 0
        self.entry_price = 0.0
        self.last_signal = None
    
    def calculate_indicators(self, data: pd.DataFrame) -> Dict[str, pd.Series]:
        """Calculate all indicators"""
        self.indicators['rsi'] = self.rsi(data)
        return self.indicators
    

    def rsi(self, data):
        period = self.params['period']
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
        
        return None
    

    def condition_0(self, data: pd.DataFrame) -> Optional[Signal]:
        """Test"""
        if (self.indicators['rsi'].iloc[-1] < 30):
            return Signal(
                timestamp=data.index[-1],
                action="BUY",
                confidence=None,
                position_size=None,
                metadata={
                    'condition': 'condition_0',
                    'priority': 1
                }
            )
        return None

    

    
    def check_exit_rules(self, data: pd.DataFrame) -> Optional[Signal]:
        """Check exit conditions"""

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
