# hydra-fuzzy

Random action fuzzer for a running [Hydra](https://github.com/cardano-scaling/hydra) Head.

Exercises the full Head lifecycle — Init → Open (NewTx, Deposit, Decommit, WaitExpire) → Close → Fanout — with configurable rounds, parallelism, and a weighted random action picker.

## Prerequisites

A running Hydra devnet with Alice, Bob, and Carol nodes. See the [Hydra demo](https://github.com/cardano-scaling/hydra/tree/master/demo).

## Build

```sh
cabal build hydra-fuzzy
```

Or with nix:

```sh
nix build .#hydra-fuzzy
```

A dev shell with GHC, cabal, and all native dependencies is available via:

```sh
nix develop
```

## Run

```sh
cabal run hydra-fuzzy -- \
  --rounds 5 \
  --txs-per-round 20 \
  --alice-port 4001 \
  --bob-port 4002 \
  --carol-port 4003 \
  --socket devnet/node.socket \
  --output ./reports
```

## Options

| Flag | Default | Description |
|------|---------|-------------|
| `--rounds` / `-r` | 3 | Number of Init→Fanout cycles |
| `--txs-per-round` / `-n` | 50 | Max random actions in Open phase |
| `--stress` | off | Flood NewTx in parallel |
| `--parallelism` / `-p` | 3 | Sender threads in stress mode |
| `--stuck-timeout` | 30s | Seconds without SnapshotConfirmed before declaring stuck |
| `--seed` | 42 | RNG seed for reproducible runs |
| `--output` / `-o` | `.` | Directory for run reports |
| `--socket` | `devnet/node.socket` | Cardano node socket path |
| `--deposit-expire-wait` | 30s | Wait budget for WaitExpire action |

## Reports

Each run writes a human-readable `.md` report to `--output`. On failure, a separate failure report is written with the action timeline and last server events.
