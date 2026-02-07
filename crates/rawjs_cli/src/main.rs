use std::env;
use std::fs;

mod repl;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        repl::run_repl();
        return;
    }

    let mut dump_ast = false;
    let mut dump_bytecode = false;
    let mut file_path = None;

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--dump-ast" => dump_ast = true,
            "--dump-bytecode" => dump_bytecode = true,
            "--help" | "-h" => {
                print_help();
                return;
            }
            arg => {
                file_path = Some(arg.to_string());
            }
        }
        i += 1;
    }

    let file_path = match file_path {
        Some(p) => p,
        None => {
            repl::run_repl();
            return;
        }
    };

    let source = match fs::read_to_string(&file_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", file_path, e);
            std::process::exit(1);
        }
    };

    if dump_ast {
        match rawjs_parser::parse(&source) {
            Ok(program) => {
                println!("{:#?}", program);
            }
            Err(e) => {
                eprintln!("{}", e);
                std::process::exit(1);
            }
        }
        return;
    }

    if dump_bytecode {
        match rawjs_parser::parse(&source) {
            Ok(program) => match rawjs_bytecode::compile(&program) {
                Ok(chunk) => {
                    println!("{}", chunk);
                }
                Err(e) => {
                    eprintln!("{}", e);
                    std::process::exit(1);
                }
            },
            Err(e) => {
                eprintln!("{}", e);
                std::process::exit(1);
            }
        }
        return;
    }

    if let Err(e) = execute_source(&source, &file_path) {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}

fn execute_source(source: &str, file_path: &str) -> rawjs_common::Result<()> {
    let program = rawjs_parser::parse(source)?;
    let chunk = rawjs_bytecode::compile(&program)?;
    let mut vm = rawjs_vm::Vm::new();

    // Set the file directory so ESM relative imports can resolve correctly.
    if let Some(parent) = std::path::Path::new(file_path)
        .canonicalize()
        .ok()
        .and_then(|p| p.parent().map(|d| d.to_string_lossy().to_string()))
    {
        vm.current_file_dir = Some(parent);
    }

    vm.execute(chunk)?;
    Ok(())
}

fn print_help() {
    println!("Usage: rawjs [options] [file.js]");
    println!();
    println!("Options:");
    println!("  --dump-ast        Print the AST and exit");
    println!("  --dump-bytecode   Print the bytecode and exit");
    println!("  --help, -h        Show this help message");
    println!();
    println!("If no file is given, starts an interactive REPL.");
}
