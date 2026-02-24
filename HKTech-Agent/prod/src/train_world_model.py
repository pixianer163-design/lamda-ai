#!/usr/bin/env python3
"""
GRU World Model 训练脚本
数据: 2018-至今, 6只恒科股票
模型: GRU(input=8, hidden=64, layers=2)
"""
import os
import sys
import numpy as np
import pandas as pd

_HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(_HERE, "../../shared"))
sys.path.insert(0, _HERE)

try:
    from constants import ALL_STOCKS, STOCKS as STOCK_INFO, get_data_dir
    TRAIN_STOCKS = {code: STOCK_INFO[code] for code in ALL_STOCKS if code in STOCK_INFO}
except Exception:
    TRAIN_STOCKS = {
        "00700": {"name": "腾讯控股"}, "09988": {"name": "阿里巴巴"},
        "03690": {"name": "美团-W"}, "01810": {"name": "小米集团"},
        "09618": {"name": "京东集团"}, "09999": {"name": "网易"},
    }
    def get_data_dir():
        return os.path.join(_HERE, "../../data")


def calculate_rsi(prices: pd.Series, period: int = 14) -> pd.Series:
    delta = prices.diff()
    gain = delta.clip(lower=0).rolling(period).mean()
    loss = (-delta.clip(upper=0)).rolling(period).mean()
    rs = gain / (loss + 1e-8)
    return 100 - (100 / (1 + rs))


def _generate_mock_historical() -> dict:
    """生成 mock 历史数据（用于无网络环境）"""
    result = {}
    dates = pd.date_range("2018-01-02", periods=1500, freq="B")
    for code in TRAIN_STOCKS:
        rng = np.random.RandomState(hash(code) % 2**31)
        prices = 100 * np.cumprod(1 + rng.normal(0, 0.015, 1500))
        df = pd.DataFrame({
            "Open":   prices * rng.uniform(0.99, 1.0, 1500),
            "High":   prices * rng.uniform(1.0, 1.02, 1500),
            "Low":    prices * rng.uniform(0.98, 1.0, 1500),
            "Close":  prices,
            "Volume": rng.uniform(1e7, 5e7, 1500),
        }, index=dates)
        result[code] = df
    return result


def fetch_historical_data(start_date="2018-01-01") -> dict:
    """拉取 start_date 至今的历史数据，失败时用 mock"""
    try:
        import yfinance as yf
    except ImportError:
        print("⚠️ yfinance 不可用，使用 mock 数据")
        return _generate_mock_historical()

    from datetime import datetime
    end_date = datetime.now().strftime("%Y-%m-%d")
    result = {}
    for code, info in TRAIN_STOCKS.items():
        yf_symbol = f"{int(code):04d}.HK"
        try:
            df = yf.download(yf_symbol, start=start_date, end=end_date,
                             progress=False, auto_adjust=True)
            if len(df) >= 100:
                result[code] = df
                print(f"✅ {code} {info.get('name', '')}: {len(df)} 条记录")
            else:
                print(f"⚠️ {code}: 数据不足 ({len(df)}条), 跳过")
        except Exception as e:
            print(f"❌ {code}: 下载失败 {e}")

    if not result:
        print("⚠️ 所有股票下载失败，使用 mock 数据")
        return _generate_mock_historical()
    return result


def build_feature_matrix(df: pd.DataFrame) -> pd.DataFrame:
    """构建 8 维特征: open_r, high_r, low_r, close_r, vol_r, ma5_r, rsi_norm, vol_ratio"""
    feat = pd.DataFrame(index=df.index)
    close = df["Close"]
    feat["open_r"]    = (df["Open"] / close.shift(1) - 1).fillna(0)
    feat["high_r"]    = (df["High"] / close - 1).fillna(0)
    feat["low_r"]     = (df["Low"] / close - 1).fillna(0)
    feat["close_r"]   = close.pct_change(fill_method=None).fillna(0)
    feat["vol_r"]     = df["Volume"].pct_change(fill_method=None).fillna(0)
    ma5 = close.rolling(5).mean()
    feat["ma5_r"]     = (ma5 / close - 1).fillna(0)
    rsi = calculate_rsi(close)
    feat["rsi_norm"]  = ((rsi - 50) / 50).fillna(0)
    avg_vol = df["Volume"].rolling(20).mean()
    feat["vol_ratio"] = (df["Volume"] / avg_vol - 1).fillna(0)
    return feat.clip(-3, 3)


def create_training_dataset(historical_data: dict, seq_len: int = 20) -> tuple:
    """构建滑动窗口数据集: X (N, seq_len, 8), y (N,) — 5日收益率"""
    X_list, y_list = [], []
    for code, df in historical_data.items():
        # handle both single-level and multi-level columns from yfinance
        if isinstance(df.columns, pd.MultiIndex):
            df = df.droplevel(1, axis=1)
        feat = build_feature_matrix(df).values.astype(np.float32)
        closes = df["Close"].values.astype(np.float32)
        for i in range(seq_len, len(feat) - 5):
            x = feat[i - seq_len:i]
            y = float(closes[i + 5] / closes[i] - 1)
            if not (np.isnan(x).any() or np.isnan(y) or np.isinf(y)):
                X_list.append(x)
                y_list.append(y)
    if not X_list:
        return np.zeros((0, seq_len, 8), dtype=np.float32), np.zeros(0, dtype=np.float32)
    return np.array(X_list, dtype=np.float32), np.array(y_list, dtype=np.float32)


def train_gru_model(X: np.ndarray, y: np.ndarray, data_dir: str,
                    epochs: int = 100, patience: int = 10) -> str:
    """训练 GRU 世界模型，保存到 data_dir/models/"""
    try:
        import torch
        import torch.nn as nn
        from torch.utils.data import TensorDataset, DataLoader
        import pickle
    except ImportError:
        print("❌ torch 不可用，无法训练模型")
        return ""

    from rssm_world_model import GRUWorldModel

    models_dir = os.path.join(data_dir, "models")
    os.makedirs(models_dir, exist_ok=True)

    split = int(len(X) * 0.8)
    X_train, X_val = X[:split], X[split:]
    y_train, y_val = y[:split], y[split:]

    y_mean = float(y_train.mean())
    y_std  = float(y_train.std()) + 1e-8
    y_train_n = (y_train - y_mean) / y_std
    y_val_n   = (y_val   - y_mean) / y_std

    with open(os.path.join(models_dir, "scaler.pkl"), "wb") as f:
        pickle.dump({"y_mean": y_mean, "y_std": y_std}, f)

    train_loader = DataLoader(
        TensorDataset(torch.from_numpy(X_train), torch.from_numpy(y_train_n)),
        batch_size=64, shuffle=True)
    val_loader = DataLoader(
        TensorDataset(torch.from_numpy(X_val), torch.from_numpy(y_val_n)),
        batch_size=256)

    model = GRUWorldModel(input_size=8, hidden_size=64, num_layers=2)
    optimizer = torch.optim.Adam(model.parameters(), lr=1e-3)
    criterion = nn.MSELoss()

    best_val_loss = float("inf")
    no_improve = 0
    model_path = os.path.join(models_dir, "rssm_model.pt")

    for epoch in range(epochs):
        model.train()
        for xb, yb in train_loader:
            optimizer.zero_grad()
            loss = criterion(model(xb).squeeze(), yb)
            loss.backward()
            optimizer.step()

        model.eval()
        val_losses = []
        with torch.no_grad():
            for xb, yb in val_loader:
                val_losses.append(criterion(model(xb).squeeze(), yb).item())
        val_loss = sum(val_losses) / len(val_losses)

        if val_loss < best_val_loss:
            best_val_loss = val_loss
            torch.save(model.state_dict(), model_path)
            no_improve = 0
        else:
            no_improve += 1
            if no_improve >= patience:
                print(f"早停于 epoch {epoch+1}, 最佳验证损失: {best_val_loss:.6f}")
                break

        if (epoch + 1) % 10 == 0:
            print(f"Epoch {epoch+1}/{epochs}: val_loss={val_loss:.6f}")

    print(f"✅ 模型已保存到 {model_path}")
    return model_path


def _upload_to_oss_after_training(data_dir: str, model_path: str) -> None:
    """训练完成后将模型和训练数据上传到阿里云OSS。oss2未安装时静默跳过。"""
    import sys as _sys

    # AccessKey.csv 在项目根目录 (lamda-ai/)，本文件在 prod/src/ 下，向上三级
    _script_dir = os.path.dirname(os.path.abspath(__file__))
    csv_path = os.path.normpath(os.path.join(_script_dir, "../../../AccessKey.csv"))

    # 导入 OSSManager（在 active_src/）
    _active_src = os.path.normpath(os.path.join(_script_dir, "../../active_src"))
    if _active_src not in _sys.path:
        _sys.path.insert(0, _active_src)

    try:
        from oss_manager import OSSManager
    except ImportError:
        print("⚠️  oss_manager 未找到，跳过OSS上传")
        return

    try:
        oss = OSSManager(csv_path=csv_path)
    except ImportError:
        print("⚠️  oss2 未安装，跳过OSS上传。运行: pip install oss2")
        return
    except ValueError as e:
        print(f"⚠️  OSS配置不完整，跳过上传: {e}")
        return
    except Exception as e:
        print(f"⚠️  OSS初始化失败，跳过上传: {e}")
        return

    # 上传模型文件（从 model_path 推导目录，确保与训练输出一致）
    model_dir = os.path.dirname(model_path)
    for fname in (os.path.basename(model_path), "scaler.pkl"):
        fpath = os.path.join(model_dir, fname)
        if os.path.exists(fpath):
            try:
                oss.upload_model(fpath, fname)
            except Exception as e:
                print(f"⚠️  上传 {fname} 失败: {e}")
        else:
            print(f"⚠️  文件不存在，跳过: {fpath}")

    # 上传训练数据
    episodes_path = os.path.join(data_dir, "training_episodes.json")
    if os.path.exists(episodes_path):
        try:
            oss.upload_training_data(episodes_path, "training_episodes.json")
        except Exception as e:
            print(f"⚠️  上传 training_episodes.json 失败: {e}")
    else:
        print(f"⚠️  文件不存在，跳过: {episodes_path}")

    print("✅ OSS上传流程完成")


def main():
    data_dir = str(get_data_dir())
    print("📥 拉取历史数据（2018-至今）...")
    historical_data = fetch_historical_data(start_date="2018-01-01")

    if not historical_data:
        print("❌ 无法获取历史数据，终止训练")
        return

    print("🔧 构建训练数据集 (seq_len=20)...")
    X, y = create_training_dataset(historical_data, seq_len=20)
    if len(X) == 0:
        print("❌ 样本数为 0，检查数据质量")
        return
    print(f"   样本数: {len(X)}, X shape: {X.shape}, y 均值: {y.mean():.4f}")

    print("🚀 开始训练 GRU 世界模型...")
    model_path = train_gru_model(X, y, data_dir)

    # 训练成功后自动上传到OSS
    if model_path:
        _upload_to_oss_after_training(data_dir, model_path)

    if model_path:
        print(f"✅ 训练完成！模型: {model_path}")
    else:
        print("⚠️ 训练跳过（torch 不可用）")


if __name__ == "__main__":
    main()
