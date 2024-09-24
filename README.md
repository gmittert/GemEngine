# The Gem Chess Engine

A UCI compatible chess engine.

## Build

```
cargo build --release
```

The resulting binary can then be loaded by a UCI compatible interface such as
Cute Chess.

## Features

- Multi core search
- Alpha beta search
- Zobrist hashing
- Magic bit boards
- Lockless transposition tables
- Iterative deeping search
- Quiescence search
- Static Exchange Evaluation
