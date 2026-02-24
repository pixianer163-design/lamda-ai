# AGENTS.md - Agentic Coding Guidelines for λ-Py EAP

## Project Overview

This is λ-Py EAP (Enterprise AI Platform), an architecture that separates:
- **Control Plane (Haskell)**: Type-safe, pure functions, deterministic guarantees
- **Compute Plane (Python)**: AI/ML, rich ecosystem, isolated execution
- **Communication**: gRPC/ZeroMQ/Arrow for zero-copy IPC

## Build, Lint, and Test Commands

### Haskell Projects (using Stack)

```bash
# Build the project
stack build

# Run the executable
stack exec <project-name>-exe

# Run all tests
stack test

# Run a single test by pattern
stack test --test-arguments="-m \"adds two numbers\""

# Run tests matching a pattern
stack test --test-arguments="-m \"/Basics/add/\""

# Enter interactive REPL
stack ghci
stack repl        # Same as above

# Load specific module in REPL
stack ghci -- <module-name>

# Watch mode for continuous rebuild
stack build --file-watch

# Clean build artifacts
stack clean

# Run with profiling
stack build --profile
stack exec -- <program> +RTS -p
```

### Python Projects

```bash
# Install dependencies
pip install -r requirements.txt

# Run tests
pytest

# Run a single test
pytest -k test_function_name

# Run with coverage
pytest --cov=src --cov-report=html

# Type checking
mypy src/

# Formatting
black src/
isort src/

# Linting
ruff check src/
flake8 src/
```

## Code Style Guidelines

### Haskell

#### Imports
```haskell
-- Standard library imports first
import Control.Concurrent
import Control.Monad
import Data.Maybe (fromMaybe)

-- Third-party imports
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Project imports last
import Basics
import Types
```

#### Type Signatures
- Always provide explicit type signatures for top-level functions
- Use type aliases for complex types
- Document with Haddock-style comments `-- |`

```haskell
-- | Safe division returning Maybe
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv a b = Just (a `div` b)
```

#### Naming Conventions
- Functions: `camelCase` (e.g., `calculateTotal`)
- Types: `PascalCase` (e.g., `Transaction`)
- Type variables: lowercase single letters (e.g., `a`, `b`, `m`)
- Constants: `UPPER_SNAKE_CASE` or regular camelCase
- Modules: `PascalCase` matching filename

#### Formatting
- 4-space indentation
- Maximum line length: 80-100 characters
- Align type signatures and definitions

```haskell
-- Good
myFunction :: Int -> String -> Bool
myFunction n s = ...

-- Good (align guards)
myFunc x
    | x > 0     = ...
    | x == 0    = ...
    | otherwise = ...
```

#### Error Handling
- Use `Maybe` for computations that might fail
- Use `Either` for computations with error messages
- Use `ExceptT` for monadic error handling
- Avoid partial functions (use `head` → `safeHead` pattern)

```haskell
-- Preferred over error-prone partial functions
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x
```

#### GHC Options (from package.yaml)
Always include these warnings:
```yaml
ghc-options:
- -Wall
- -Wcompat
- -Widentities
- -Wincomplete-record-updates
- -Wincomplete-uni-patterns
- -Wmissing-export-lists
- -Wmissing-home-modules
- -Wpartial-fields
- -Wredundant-constraints
```

### Python

#### Imports
```python
# Standard library
import asyncio
from typing import Optional, List

# Third-party
import numpy as np
import grpc

# Project
from .models import Model
```

#### Type Hints
- Use type hints for all function parameters and returns
- Use `Optional` for nullable values
- Use `Union` (or `|` in Python 3.10+) for multiple types

```python
def safe_divide(a: float, b: float) -> Optional[float]:
    if b == 0:
        return None
    return a / b
```

#### Naming Conventions
- Functions/variables: `snake_case`
- Classes: `PascalCase`
- Constants: `UPPER_SNAKE_CASE`
- Private: `_leading_underscore`

#### Error Handling
- Use exceptions for truly exceptional cases
- Return `Optional` or `Result` types for expected failures
- Always catch specific exceptions, never bare `except:`

```python
# Good
try:
    result = risky_operation()
except ValueError as e:
    logger.error(f"Invalid value: {e}")
    return None

# Bad
try:
    result = risky_operation()
except:  # Never do this
    pass
```

## Architecture Principles

### Control Plane (Haskell)
1. **Pure Functions**: Business logic must be pure, push IO to edges
2. **Type Safety**: Encode business rules in types (type firewall)
3. **Free Monad**: Use for effect systems and DSLs
4. **Property Testing**: Use QuickCheck for invariant verification

### Compute Plane (Python)
1. **Isolation**: Run AI/ML in sandboxed environments
2. **Defensive**: Validate all inputs, expect failures
3. **Observability**: Structured logging, metrics, tracing
4. **Resource Limits**: Enforce memory/CPU/time constraints

### Communication
1. **Zero-Copy**: Use Arrow/Cap'n Proto for large data
2. **Versioned**: gRPC with backward compatibility
3. **Timeouts**: Always set RPC deadlines
4. **Circuit Breakers**: Fail fast on degraded dependencies

## Testing Standards

### Haskell (HSpec)
```haskell
main :: IO ()
main = hspec $ do
    describe "Module" $ do
        describe "function" $ do
            it "handles normal case" $ do
                function input `shouldBe` expected
            
            it "is commutative" $ property $
                \x y -> function x y == function y x
```

### Python (pytest)
```python
def test_function_normal_case():
    assert function(input_data) == expected

def test_function_edge_case():
    assert function(None) is None
```

## Language-Specific Tips

### For C/C++ Programmers Learning Haskell
1. Forget pointers and manual memory management (GC handles it)
2. Embrace immutability - don't modify, create new values
3. Types are documentation - read them carefully
4. Compiler is your friend - error messages are detailed
5. Start with "Haskell Tutorial for C Programmers"

## Repository Structure

```
lamda-ai/
├── haskell-learning/       # Learning materials for C→Haskell
│   ├── init-haskell-project.sh  # Creates Stack project
│   └── my-haskell-learning/     # Generated project
├── 架构.md                 # Architecture documentation
├── 需求.md                 # Requirements
└── design_proposer.md      # Design proposals
```

## Common Tasks

### Create New Haskell Project
```bash
cd haskell-learning
./init-haskell-project.sh my-project
cd my-project
stack build
```

### Add New Dependency
Edit `package.yaml` dependencies section, then:
```bash
stack build
```

### Run Tests with Coverage (Haskell)
```bash
stack test --coverage
```

### Check for Updates
```bash
stack update
```

---

*This file helps AI agents work effectively with the λ-Py codebase.*
