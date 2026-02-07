use std::io::{self, Read, Write};

// ANSI color codes
const RESET: &str = "\x1b[0m";
const GREEN: &str = "\x1b[32m";
const CYAN: &str = "\x1b[36m";
const RED: &str = "\x1b[31m";
const YELLOW: &str = "\x1b[33m";
const GRAY: &str = "\x1b[90m";
const BOLD: &str = "\x1b[1m";

/// A line editor that supports arrow keys and history navigation in raw terminal mode.
struct LineEditor {
    history: Vec<String>,
    history_pos: usize,
}

impl LineEditor {
    fn new() -> Self {
        LineEditor {
            history: Vec::new(),
            history_pos: 0,
        }
    }

    /// Read a line of input with line editing and history support.
    /// Returns None on EOF (Ctrl-D).
    fn read_line(&mut self, prompt: &str) -> Option<String> {
        let stdout = io::stdout();
        let mut stdout = stdout.lock();

        write!(stdout, "{}", prompt).ok();
        stdout.flush().ok();

        // Try to enable raw mode
        let raw_mode = RawMode::enable();
        if raw_mode.is_none() {
            // Fallback: just read a line normally
            drop(stdout);
            let mut line = String::new();
            match io::stdin().read_line(&mut line) {
                Ok(0) => return None,
                Ok(_) => {
                    return Some(
                        line.trim_end_matches('\n')
                            .trim_end_matches('\r')
                            .to_string(),
                    )
                }
                Err(_) => return None,
            }
        }

        let mut buf = String::new();
        let mut cursor = 0;
        self.history_pos = self.history.len();
        let mut saved_input = String::new();

        let stdin = io::stdin();
        let mut stdin = stdin.lock();
        let mut byte_buf = [0u8; 1];

        loop {
            match stdin.read(&mut byte_buf) {
                Ok(0) => {
                    // EOF
                    if buf.is_empty() {
                        writeln!(stdout).ok();
                        return None;
                    }
                    break;
                }
                Ok(_) => {}
                Err(_) => return None,
            }

            match byte_buf[0] {
                // Ctrl-D
                4 => {
                    if buf.is_empty() {
                        writeln!(stdout).ok();
                        stdout.flush().ok();
                        return None;
                    }
                }
                // Ctrl-C
                3 => {
                    write!(stdout, "^C\r\n{}", prompt).ok();
                    stdout.flush().ok();
                    buf.clear();
                    cursor = 0;
                    self.history_pos = self.history.len();
                }
                // Enter
                b'\r' | b'\n' => {
                    writeln!(stdout).ok();
                    stdout.flush().ok();
                    break;
                }
                // Backspace (127 or 8)
                127 | 8 => {
                    if cursor > 0 {
                        cursor -= 1;
                        buf.remove(cursor);
                        self.redraw_line(&mut stdout, prompt, &buf, cursor);
                    }
                }
                // Escape sequence
                b'\x1b' => {
                    let mut seq = [0u8; 2];
                    if stdin.read(&mut seq[..1]).unwrap_or(0) == 0 {
                        continue;
                    }
                    if seq[0] == b'[' {
                        if stdin.read(&mut seq[1..2]).unwrap_or(0) == 0 {
                            continue;
                        }
                        match seq[1] {
                            // Up arrow
                            b'A' => {
                                if !self.history.is_empty() && self.history_pos > 0 {
                                    if self.history_pos == self.history.len() {
                                        saved_input = buf.clone();
                                    }
                                    self.history_pos -= 1;
                                    buf = self.history[self.history_pos].clone();
                                    cursor = buf.len();
                                    self.redraw_line(&mut stdout, prompt, &buf, cursor);
                                }
                            }
                            // Down arrow
                            b'B' => {
                                if self.history_pos < self.history.len() {
                                    self.history_pos += 1;
                                    if self.history_pos == self.history.len() {
                                        buf = saved_input.clone();
                                    } else {
                                        buf = self.history[self.history_pos].clone();
                                    }
                                    cursor = buf.len();
                                    self.redraw_line(&mut stdout, prompt, &buf, cursor);
                                }
                            }
                            // Right arrow
                            b'C' => {
                                if cursor < buf.len() {
                                    cursor += 1;
                                    write!(stdout, "\x1b[C").ok();
                                    stdout.flush().ok();
                                }
                            }
                            // Left arrow
                            b'D' => {
                                if cursor > 0 {
                                    cursor -= 1;
                                    write!(stdout, "\x1b[D").ok();
                                    stdout.flush().ok();
                                }
                            }
                            // Home (some terminals send \x1b[H)
                            b'H' => {
                                cursor = 0;
                                self.redraw_line(&mut stdout, prompt, &buf, cursor);
                            }
                            // End (some terminals send \x1b[F)
                            b'F' => {
                                cursor = buf.len();
                                self.redraw_line(&mut stdout, prompt, &buf, cursor);
                            }
                            // Handle extended sequences like \x1b[3~ (Delete)
                            b'3' => {
                                let mut tilde = [0u8; 1];
                                let _ = stdin.read_exact(&mut tilde);
                                if tilde[0] == b'~' && cursor < buf.len() {
                                    buf.remove(cursor);
                                    self.redraw_line(&mut stdout, prompt, &buf, cursor);
                                }
                            }
                            _ => {}
                        }
                    }
                }
                // Ctrl-A (Home)
                1 => {
                    cursor = 0;
                    self.redraw_line(&mut stdout, prompt, &buf, cursor);
                }
                // Ctrl-E (End)
                5 => {
                    cursor = buf.len();
                    self.redraw_line(&mut stdout, prompt, &buf, cursor);
                }
                // Ctrl-U (clear line)
                21 => {
                    buf.clear();
                    cursor = 0;
                    self.redraw_line(&mut stdout, prompt, &buf, cursor);
                }
                // Ctrl-K (kill to end)
                11 => {
                    buf.truncate(cursor);
                    self.redraw_line(&mut stdout, prompt, &buf, cursor);
                }
                // Ctrl-W (delete word backward)
                23 => {
                    if cursor > 0 {
                        let mut new_cursor = cursor;
                        // Skip trailing spaces
                        while new_cursor > 0 && buf.as_bytes()[new_cursor - 1] == b' ' {
                            new_cursor -= 1;
                        }
                        // Skip word chars
                        while new_cursor > 0 && buf.as_bytes()[new_cursor - 1] != b' ' {
                            new_cursor -= 1;
                        }
                        buf.drain(new_cursor..cursor);
                        cursor = new_cursor;
                        self.redraw_line(&mut stdout, prompt, &buf, cursor);
                    }
                }
                // Regular printable character
                ch if ch >= 32 => {
                    buf.insert(cursor, ch as char);
                    cursor += 1;
                    if cursor == buf.len() {
                        // Append at end - just write the char
                        write!(stdout, "{}", ch as char).ok();
                        stdout.flush().ok();
                    } else {
                        self.redraw_line(&mut stdout, prompt, &buf, cursor);
                    }
                }
                _ => {}
            }
        }

        // Add to history if non-empty and different from last entry
        let trimmed = buf.trim().to_string();
        if !trimmed.is_empty() && self.history.last().map(|s| s.as_str()) != Some(&trimmed) {
            self.history.push(trimmed);
        }

        Some(buf)
    }

    fn redraw_line(&self, stdout: &mut impl Write, prompt: &str, buf: &str, cursor: usize) {
        // Move to beginning of line, clear, rewrite
        write!(stdout, "\r\x1b[K{}{}", prompt, buf).ok();
        // Move cursor to correct position
        let back = buf.len() - cursor;
        if back > 0 {
            write!(stdout, "\x1b[{}D", back).ok();
        }
        stdout.flush().ok();
    }
}

// Platform-specific termios FFI (no libc dependency)
#[cfg(target_os = "macos")]
mod termios_ffi {
    use std::os::raw::{c_int, c_ulong};

    const NCCS: usize = 20;
    pub const ECHO: c_ulong = 0x8;
    pub const ICANON: c_ulong = 0x100;
    pub const ISIG: c_ulong = 0x80;
    pub const VMIN: usize = 16;
    pub const VTIME: usize = 17;
    pub const TCSAFLUSH: c_int = 2;
    pub const STDIN_FILENO: c_int = 0;

    #[repr(C)]
    #[derive(Copy, Clone)]
    pub struct Termios {
        pub c_iflag: c_ulong,
        pub c_oflag: c_ulong,
        pub c_cflag: c_ulong,
        pub c_lflag: c_ulong,
        pub c_cc: [u8; NCCS],
        pub c_ispeed: c_ulong,
        pub c_ospeed: c_ulong,
    }

    extern "C" {
        pub fn tcgetattr(fd: c_int, termios: *mut Termios) -> c_int;
        pub fn tcsetattr(fd: c_int, action: c_int, termios: *const Termios) -> c_int;
    }
}

#[cfg(target_os = "linux")]
mod termios_ffi {
    use std::os::raw::{c_int, c_uint};

    const NCCS: usize = 32;
    pub const ECHO: c_uint = 0x8;
    pub const ICANON: c_uint = 0x2;
    pub const ISIG: c_uint = 0x1;
    pub const VMIN: usize = 6;
    pub const VTIME: usize = 5;
    pub const TCSAFLUSH: c_int = 2;
    pub const STDIN_FILENO: c_int = 0;

    #[repr(C)]
    #[derive(Copy, Clone)]
    pub struct Termios {
        pub c_iflag: c_uint,
        pub c_oflag: c_uint,
        pub c_cflag: c_uint,
        pub c_lflag: c_uint,
        pub c_line: u8,
        pub c_cc: [u8; NCCS],
        pub c_ispeed: c_uint,
        pub c_ospeed: c_uint,
    }

    extern "C" {
        pub fn tcgetattr(fd: c_int, termios: *mut Termios) -> c_int;
        pub fn tcsetattr(fd: c_int, action: c_int, termios: *const Termios) -> c_int;
    }
}

/// RAII guard for raw terminal mode.
struct RawMode {
    original: termios_ffi::Termios,
}

impl RawMode {
    fn enable() -> Option<Self> {
        unsafe {
            let mut original: termios_ffi::Termios = std::mem::zeroed();
            if termios_ffi::tcgetattr(termios_ffi::STDIN_FILENO, &mut original) != 0 {
                return None;
            }
            let mut raw = original;
            raw.c_lflag &= !(termios_ffi::ECHO | termios_ffi::ICANON | termios_ffi::ISIG);
            raw.c_cc[termios_ffi::VMIN] = 1;
            raw.c_cc[termios_ffi::VTIME] = 0;
            if termios_ffi::tcsetattr(termios_ffi::STDIN_FILENO, termios_ffi::TCSAFLUSH, &raw) != 0
            {
                return None;
            }
            Some(RawMode { original })
        }
    }
}

impl Drop for RawMode {
    fn drop(&mut self) {
        unsafe {
            termios_ffi::tcsetattr(
                termios_ffi::STDIN_FILENO,
                termios_ffi::TCSAFLUSH,
                &self.original,
            );
        }
    }
}

/// Format a JsValue for REPL display with colors.
/// Unlike console.log, this shows the type information (e.g., strings are quoted).
fn format_repl_value(value: &rawjs_runtime::JsValue) -> String {
    match value {
        rawjs_runtime::JsValue::Undefined => format!("{GRAY}undefined{RESET}"),
        rawjs_runtime::JsValue::Null => format!("{BOLD}null{RESET}"),
        rawjs_runtime::JsValue::Boolean(b) => format!("{YELLOW}{b}{RESET}"),
        rawjs_runtime::JsValue::Number(n) => {
            format!(
                "{YELLOW}{}{RESET}",
                rawjs_runtime::value::number_to_string(*n)
            )
        }
        rawjs_runtime::JsValue::String(s) => format!("{GREEN}'{s}'{RESET}"),
        rawjs_runtime::JsValue::Symbol(sym) => {
            if let Some(ref desc) = sym.description {
                format!("{RED}Symbol({desc}){RESET}")
            } else {
                format!("{RED}Symbol(){RESET}")
            }
        }
        rawjs_runtime::JsValue::Object(ptr) => {
            let obj = ptr.borrow();
            format!("{CYAN}{obj}{RESET}")
        }
    }
}

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
            Ok(program) => match rawjs_bytecode::compile(&program) {
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
