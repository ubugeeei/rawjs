pub fn run_repl() {
    let prompt = format!("{GREEN}{BOLD}> {RESET}");
    println!("{BOLD}rawjs{RESET} {CYAN}v0.1.0{RESET} {GRAY}(type .exit or Ctrl-D to quit){RESET}");
    let mut vm = rawjs_vm::Vm::new();
    let mut editor = LineEditor::new();
    while let Some(line) = editor.read_line(&prompt) {
        let line = line.trim();
        if line == ".exit" {
            break;
        }
        if line.is_empty() {
            continue;
        }
        match rawjs_parser::parse(line) {
            Ok(program) => match rawjs_bytecode::compile_repl(&program) {
                Ok(chunk) => match vm.execute(chunk) {
                    Ok(value) => {
                        println!("{}", format_repl_value(&value));
                    }
                    Err(e) => {
                        eprintln!("{RED}{e}{RESET}");
                    }
                },
                Err(e) => {
                    eprintln!("{RED}{e}{RESET}");
                }
            },
            Err(e) => {
                eprintln!("{RED}{e}{RESET}");
            }
        }
    }
}

use super::*;
