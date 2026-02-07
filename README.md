# rawjs

A JavaScript engine written in Rust. Hobby project.

## Why "raw"?

The JIT compiler generates and executes aarch64 machine code directly in Rust — no external libraries, no dependencies.

## Usage

```sh
cargo run -- examples/hello.js    # run a script
cargo run -- --dump-ast file.js   # dump AST
cargo run -- --dump-bytecode file.js  # dump bytecode
cargo run                         # REPL
```
