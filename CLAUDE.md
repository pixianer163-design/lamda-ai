# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**λ-Py EAP (Enterprise AI Platform)** — a hybrid architecture separating:
- **Control Plane (Haskell)**: Type-safe business logic, pure functions, deterministic guarantees
- **Compute Plane (Python)**: AI/ML workloads, quantitative trading, rich ecosystem
- **Communication**: gRPC/ZeroMQ/Arrow for zero-copy IPC between planes

The primary active sub-project is **HKTech-Agent** — an LLM-enhanced Hong Kong stock quantitative trading agent (恒生科技 index stocks) with RSSM world model, LLM-driven signal extraction and decision enhancement, and Feishu push notifications.

---

## Commands

### Python / HKTech-Agent

```bash
# Install dependencies
pip install -r HKTech-Agent/requirements.txt

# Run full test suite
cd HKTech-Agent && ./run_tests.sh

# Run tests directly
cd HKTech-Agent && python3 -m pytest tests/ -v --tb=short --timeout=300

# Run a single test by name
pytest -k test_function_name

# Run tests with coverage
pytest --cov=src --cov-report=html

# Type checking / formatting / linting
mypy src/
black src/ && isort src/
ruff check src/

# Run production agent (weekdays 9:30 AM via cron)
cd HKTech-Agent/prod && ./run_prod.sh

# Start web monitoring server (port 8080)
python3 HKTech-Agent/scripts/start_web_server.py
```

### Haskell (Stack)

```bash
stack build
stack test
stack test --test-arguments="-m \"pattern\""   # Run single test by pattern
stack ghci                                       # Interactive REPL
stack build --file-watch                         # Watch mode
stack test --coverage
```

### Agent Factory CLI

```bash
python factory/agent_factory.py list-templates
python factory/agent_factory.py create <template> <agent_id>
python factory/agent_factory.py stats
```

---

## Architecture

### HKTech-Agent Trading Pipeline

Data flows through these stages in order:

1. **Data Collection** (`active_src/data_collector.py`): Yahoo Finance (primary) → Sina Finance (fallback) → mock data. Returns OHLCV + fundamentals for HK stocks.

2. **RSSM World Model** (`prod/src/rssm_world_model.py`): PyTorch RNN/State-Space model predicting 5-day returns with confidence scores. Trained offline, loaded for inference. `train_world_model.py` handles retraining.

3. **LLM Signal Extraction** (`prod/src/llm_signal_extractor.py`): Calls DeepSeek API to parse financial news into sentiment scores (0–1).

4. **LLM Decision Enhancement** (`prod/src/llm_decision_enhancer.py`): Synthesizes technical indicators + world model output + sentiment → BUY/SELL/HOLD with risk warnings.

5. **Risk Management** (embedded in agent): Max 40% single stock, 80% total position, −8% stop-loss, +15% take-profit, 20% min cash reserve.

6. **Notification** (`prod/src/feishu_webhook_handler.py`, `prod/src/daily_report_sender.py`): Interactive Feishu card messages with portfolio summary.

The top-level orchestrator is `prod/src/llm_enhanced_agent.py`.

### Agent Factory & A/B Testing

`Agent_Factory/` provides a meta-learning platform (DPML, in development) for running parallel agents with different risk profiles. Three pre-configured agent templates under `HKTech-Agent/factory/configs/`:
- `basic` — multi-factor, medium risk (40% tech / 30% fundamental / 30% sentiment)
- `conservative` — value-oriented, low risk
- `aggressive` — momentum, high risk

`factory/core/config_manager.py` handles YAML-based agent configuration. Agents track their own state and can be cloned/forked.

### VectorBT Integration

`active_src/vectorbt_integration.py` wraps VectorBT for fast strategy backtesting. Used to validate signal quality before promoting to live trading. `world_model_integration.py` bridges RSSM output to VectorBT signal format.

### Web Server

`scripts/start_web_server.py` serves:
- `/` — monitoring dashboard (real-time portfolio, positions, signals)
- `/web/docs/` — documentation center (Markdown → HTML rendered)
- `/webhook/feishu/hktech` — Feishu callback endpoint
- `/health`, `/healthz`, `/status` — health checks

---

## Code Style

### Python
- Type hints on all function signatures; use `Optional` for nullable values
- `snake_case` functions/variables, `PascalCase` classes, `UPPER_SNAKE_CASE` constants
- Catch specific exceptions only — never bare `except:`
- Structured logging with context, not bare `print()`

### Haskell
- Always provide explicit type signatures for top-level functions
- Use `Maybe`/`Either`/`ExceptT` for error handling — avoid partial functions
- `camelCase` functions, `PascalCase` types, 4-space indentation, 80–100 char lines
- Enable `-Wall -Wcompat` and the full GHC warning set (see `AGENTS.md`)

### Test Markers (pytest.ini)
`unit`, `integration`, `slow`, `external`, `performance` — use `-m` to filter.

---

## Key Configuration

- `HKTech-Agent/config/feishu_config.json` — Feishu webhook credentials
- `HKTech-Agent/factory/configs/*.yaml` — agent strategy templates
- Production runtime config loaded from `/opt/hktech-agent/config/config.env` (not in repo)
- Logs written to `HKTech-Agent/prod/logs/run_YYYYMMDD_HHMMSS.log`
