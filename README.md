# rawjs

A JavaScript engine written in Rust. Hobby project.

## Why "raw"?

The JIT compiler generates and executes aarch64 machine code directly in Rust, with no external libraries or dependencies.

## Targets

- `aarch64`: interpreter + baseline JIT
- `x86_64`: interpreter + baseline JIT
- `riscv64`: interpreter + baseline JIT

## Usage

```sh
cargo run -- examples/hello.js    # run a script
cargo run -- --dump-ast file.js   # dump AST
cargo run -- --dump-bytecode file.js  # dump bytecode
cargo run                         # REPL
```

## Docs

- [`docs/jit-from-zero/00-index.mdc`](docs/jit-from-zero/00-index.mdc): a detailed, multi-part English tutorial for building a tiny JIT from zero knowledge up to a VM with functions, closures, objects, queues, and promises.
