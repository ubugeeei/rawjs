impl LineEditor {
    pub(super) fn redraw_line(
        &self,
        stdout: &mut impl Write,
        prompt: &str,
        buf: &str,
        cursor: usize,
    ) {
        write!(stdout, "\r\x1b[K{}{}", prompt, buf).ok();
        let back = buf.len() - cursor;
        if back > 0 {
            write!(stdout, "\x1b[{}D", back).ok();
        }
        stdout.flush().ok();
    }
}

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
pub(super) struct RawMode {
    original: termios_ffi::Termios,
}

impl RawMode {
    pub(super) fn enable() -> Option<Self> {
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
pub(super) fn format_repl_value(value: &rawjs_runtime::JsValue) -> String {
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

use super::*;
