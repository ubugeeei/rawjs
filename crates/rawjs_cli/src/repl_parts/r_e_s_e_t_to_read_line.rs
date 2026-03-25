use std::io::{self, Read, Write};

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
}

impl LineEditor {
    #[doc = " Read a line of input with line editing and history support."]
    #[doc = " Returns None on EOF (Ctrl-D)."]
    fn read_line(&mut self, prompt: &str) -> Option<String> {
        let stdout = io::stdout();
        let mut stdout = stdout.lock();
        write!(stdout, "{}", prompt).ok();
        stdout.flush().ok();
        let raw_mode = RawMode::enable();
        if raw_mode.is_none() {
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
                4 => {
                    if buf.is_empty() {
                        writeln!(stdout).ok();
                        stdout.flush().ok();
                        return None;
                    }
                }
                3 => {
                    write!(stdout, "^C\r\n{}", prompt).ok();
                    stdout.flush().ok();
                    buf.clear();
                    cursor = 0;
                    self.history_pos = self.history.len();
                }
                b'\r' | b'\n' => {
                    writeln!(stdout).ok();
                    stdout.flush().ok();
                    break;
                }
                127 | 8 => {
                    if cursor > 0 {
                        cursor -= 1;
                        buf.remove(cursor);
                        self.redraw_line(&mut stdout, prompt, &buf, cursor);
                    }
                }
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
                            b'C' => {
                                if cursor < buf.len() {
                                    cursor += 1;
                                    write!(stdout, "\x1b[C").ok();
                                    stdout.flush().ok();
                                }
                            }
                            b'D' => {
                                if cursor > 0 {
                                    cursor -= 1;
                                    write!(stdout, "\x1b[D").ok();
                                    stdout.flush().ok();
                                }
                            }
                            b'H' => {
                                cursor = 0;
                                self.redraw_line(&mut stdout, prompt, &buf, cursor);
                            }
                            b'F' => {
                                cursor = buf.len();
                                self.redraw_line(&mut stdout, prompt, &buf, cursor);
                            }
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
                1 => {
                    cursor = 0;
                    self.redraw_line(&mut stdout, prompt, &buf, cursor);
                }
                5 => {
                    cursor = buf.len();
                    self.redraw_line(&mut stdout, prompt, &buf, cursor);
                }
                21 => {
                    buf.clear();
                    cursor = 0;
                    self.redraw_line(&mut stdout, prompt, &buf, cursor);
                }
                11 => {
                    buf.truncate(cursor);
                    self.redraw_line(&mut stdout, prompt, &buf, cursor);
                }
                23 => {
                    if cursor > 0 {
                        let mut new_cursor = cursor;
                        while new_cursor > 0 && buf.as_bytes()[new_cursor - 1] == b' ' {
                            new_cursor -= 1;
                        }
                        while new_cursor > 0 && buf.as_bytes()[new_cursor - 1] != b' ' {
                            new_cursor -= 1;
                        }
                        buf.drain(new_cursor..cursor);
                        cursor = new_cursor;
                        self.redraw_line(&mut stdout, prompt, &buf, cursor);
                    }
                }
                ch if ch >= 32 => {
                    buf.insert(cursor, ch as char);
                    cursor += 1;
                    if cursor == buf.len() {
                        write!(stdout, "{}", ch as char).ok();
                        stdout.flush().ok();
                    } else {
                        self.redraw_line(&mut stdout, prompt, &buf, cursor);
                    }
                }
                _ => {}
            }
        }
        let trimmed = buf.trim().to_string();
        if !trimmed.is_empty() && self.history.last().map(|s| s.as_str()) != Some(&trimmed) {
            self.history.push(trimmed);
        }
        Some(buf)
    }
}
