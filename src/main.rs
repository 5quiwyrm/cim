use crossterm::{
    event::{self, Event, KeyCode, KeyModifiers},
    terminal,
};

use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};

use std::{
    env::args,
    fmt::{self, Write as fmtWrite},
    fs,
    io::{self, Write as ioWrite},
    time,
};

/*

After learning from the mistakes of last time, I have decided to leave the
Buffer struct to only deal with containing what is most important.

Locally owned qualities of a buffer:

- filepath
- contents (this should be disentangled with filename)
- content history (used for undo)
- cursor position
- indent level

Everything else you could possibly think of that we only need 1 copy of will
live in the event loop.

*/

pub struct ContentHistory {


    data: String,
    cchar: usize,
    cline: usize,
    ecchar: usize,
    ecline: usize,
    top: usize,
}

pub struct Buffer {
    filepath: String,
    contents: Vec<String>,                // TODO: use a rope data structure
    content_history: Vec<ContentHistory>, // TODO: use a static size struture
    cline: usize,
    cchar: usize,
    ecline: usize,
    ecchar: usize,
    indent_level: usize,
    top: usize,
    has_unsaved_changes: bool,
}

impl Buffer {
    pub fn new(filepath: &str) -> Result<Self, Vec<String>> {
        let contents_raw = fs::read_to_string(filepath)
            .map_err(|_| vec![format!("Problem opening file `{filepath}`")])?;
        Ok(Buffer {
            filepath: filepath.to_string(),
            contents: contents_raw
                .split('\n')
                .map(|line| line.trim_end())
                .map(|line| line.to_string())
                .collect(),
            content_history: vec![ContentHistory {
                data: contents_raw.clone(),
                cchar: 0,
                cline: 0,
                ecline: 0,
                ecchar: 1,
                top: 0,
            }],
            cline: 0,
            cchar: 0,
            ecline: 0,
            ecchar: 1,
            indent_level: 0,
            top: 0,
            has_unsaved_changes: false,
        })
    }
    pub fn new_scratch() -> Self {
        Buffer {
            filepath: String::from("*scratch"),
            contents: vec![String::from("")],
            content_history: vec![ContentHistory {
                data: String::new(),
                cchar: 0,
                cline: 0,
                ecline: 0,
                ecchar: 1,
                top: 0,
            }],
            cline: 0,
            cchar: 0,
            ecline: 0,
            ecchar: 1,
            indent_level: 0,
            top: 0,
            has_unsaved_changes: false,
        }
    }

    pub fn reload_file(&mut self) -> bool {
        let contents_raw_u = fs::read_to_string(&self.filepath);
        if contents_raw_u.is_err() {
            return false;
        }
        let contents_raw = contents_raw_u.unwrap();
        let mut write_contents = self
            .contents
            .iter()
            .map(|s| s.trim_end())
            .fold(String::new(), |a, b| a + b + "\n");
        _ = write_contents.pop();
        self.content_history.push(ContentHistory {
            data: write_contents.clone(),
            cchar: self.cchar,
            cline: self.cline,
            ecline: self.ecline,
            ecchar: self.ecchar,
            top: self.top,
        });
        if self.content_history.len() > 64 {
            _ = self.content_history.remove(0);
        }
        self.contents = contents_raw
            .split('\n')
            .map(|line| line.trim_end())
            .map(|line| line.to_string())
            .collect();
        self.ensure_cursor_inbound();
        self.adjust_top();
        true
    }
}

impl Buffer {
    pub fn type_char(&mut self, ch: char) -> Option<()> {
        let curr_line = self.contents.get_mut(self.cline)?;
        let curr_line_copy = curr_line.clone();
        curr_line.clear();
        let mut typed = false;
        for (idx, c) in curr_line_copy.chars().enumerate() {
            if idx == self.cchar {
                curr_line.push(ch);
                typed = true;
            }
            curr_line.push(c);
        }
        if !typed {
            curr_line.push(ch);
        }
        self.cchar += 1;
        self.make_selection_cursor();
        self.has_unsaved_changes = true;
        Some(())
    }

    // Ok(char) where char is the character deleted ('\n' means newline).
    // Err(()) means no work was done.
    pub fn backspace(&mut self) -> Option<char> {
        if self.cchar == 0 {
            if self.cline != 0 {
                let old_line = self.contents[self.cline].clone();
                let old_len = self.contents[self.cline - 1].chars().count();
                for c in old_line.chars() {
                    self.contents[self.cline - 1].push(c);
                }
                _ = self.contents.remove(self.cline);
                self.cline -= 1;
                self.cchar = old_len;
                self.has_unsaved_changes = true;
                self.make_selection_cursor();
                self.adjust_top();
                Some('\n')
            } else {
                None
            }
        } else if self.cchar == self.contents[self.cline].chars().count() {
            // This is an optimisation.
            let c = self.contents[self.cline]
                .pop()
                .expect("unreachable: len should not be 0");
            self.cchar -= 1;
            self.make_selection_cursor();
            self.has_unsaved_changes = true;
            self.adjust_top();
            Some(c)
        } else {
            let old_line = self.contents[self.cline].clone();
            self.contents[self.cline].clear();
            let mut removed: Option<char> = None;
            for (i, c) in old_line.chars().enumerate() {
                if i + 1 != self.cchar {
                    self.contents[self.cline].push(c);
                } else {
                    self.has_unsaved_changes = true;
                    self.adjust_top();
                    removed = Some(c);
                }
            }
            self.cchar -= 1;
            self.make_selection_cursor();
            removed
        }
    }

    pub fn enter(&mut self) {
        self.determine_indent();
        let mut newline = String::new();
        // PHASE2: Change this `4` to something based on language.
        for _ in 0..(self.indent_level * 4) {
            newline.push(' ');
        }
        let mut deleted = 0;
        for (idx, c) in self.contents[self.cline].chars().enumerate() {
            if idx >= self.cchar {
                newline.push(c);
                deleted += 1;
            }
        }
        let mut fc = Some(' ');
        for _ in 0..deleted {
            fc = self.contents[self.cline].pop();
        }
        if matches!(fc, Some(')' | ']' | '}')) {
            let mut empty_line = String::new();
            for _ in 0..((self.indent_level + 1) * 4) {
                empty_line.push(' ');
            }
            self.contents.insert(self.cline + 1, empty_line);
            self.contents.insert(self.cline + 2, newline);
            self.cline += 1;
            self.cchar = self.indent_level * 4 + 4;
            self.adjust_top();
            self.make_selection_cursor();
            self.has_unsaved_changes = true;
            return;
        } else if matches!(
            self.contents[self.cline].chars().last(),
            Some('(' | '[' | '{')
        ) {
            let indent_unit = " ".repeat(4);
            newline.insert_str(0, &indent_unit);
            self.indent_level += 1;
        }
        self.contents.insert(self.cline + 1, newline);
        self.cline += 1;
        self.cchar = self.indent_level * 4;
        self.adjust_top();
        self.make_selection_cursor();
        self.has_unsaved_changes = true;
    }

    // True indicates work done.
    pub fn move_up(&mut self) -> bool {
        if self.cline == 0 {
            return false;
        }
        self.cline -= 1;
        self.ensure_cursor_inbound();
        self.adjust_top();
        true
    }

    pub fn extend_up(&mut self) -> bool {
        if self.ecline == 0 {
            false
        } else {
            self.ecline -= 1;
            self.ensure_selection_end_inbound();
            true
        }
    }

    pub fn ensure_cursor_inbound(&mut self) {
        if self.cline >= self.contents.len() {
            self.cline = self.contents.len() - 1;
        }
        let curr_line_len = self.contents[self.cline].chars().count();
        self.cchar = std::cmp::min(self.cchar, curr_line_len);
        self.make_selection_cursor();
    }

    pub fn ensure_selection_end_inbound(&mut self) {
        if self.ecline >= self.contents.len() {
            self.ecline = self.contents.len() - 1;
        }
        let curr_line_len = self.contents[self.ecline].chars().count();
        self.ecchar = std::cmp::min(self.ecchar, curr_line_len);
    }

    pub fn move_down(&mut self) -> bool {
        if self.cline + 1 == self.contents.len() {
            return false;
        }
        self.cline += 1;
        self.adjust_top();
        self.make_selection_cursor();
        self.ensure_cursor_inbound();
        true
    }

    pub fn extend_down(&mut self) -> bool {
        if self.ecline + 1 == self.contents.len() {
            return false;
        }
        self.ecline += 1;
        self.ensure_selection_end_inbound();
        true
    }

    pub fn move_left(&mut self) -> bool {
        if self.cchar == 0 {
            if self.cline != 0 {
                self.cline -= 1;
                self.cchar = self.contents[self.cline].chars().count();
                self.adjust_top();
                self.make_selection_cursor();
                self.ensure_selection_end_inbound();
                true
            } else {
                false
            }
        } else {
            self.cchar -= 1;
            self.adjust_top();
            self.make_selection_cursor();
            self.ensure_selection_end_inbound();
            true
        }
    }

    pub fn extend_left(&mut self) -> bool {
        if self.ecchar == 0 {
            if self.ecline != 0 {
                self.ecline -= 1;
                self.ecchar = self.contents[self.ecline].chars().count();
                self.ensure_selection_end_inbound();
                true
            } else {
                false
            }
        } else {
            self.ecchar -= 1;
            self.ensure_selection_end_inbound();
            true
        }
    }

    pub fn move_right(&mut self) -> bool {
        if self.cchar == self.contents[self.cline].chars().count() {
            if self.cline + 1 != self.contents.len() {
                self.cchar = 0;
                self.cline += 1;
                self.adjust_top();
                self.make_selection_cursor();
                true
            } else {
                false
            }
        } else {
            self.cchar += 1;
            self.adjust_top();
            self.make_selection_cursor();
            true
        }
    }

    pub fn extend_right(&mut self) -> bool {
        if self.ecchar == self.contents[self.ecline].chars().count() {
            if self.ecline + 1 != self.contents.len() {
                self.ecchar = 0;
                self.ecline += 1;
                self.ensure_selection_end_inbound();
                true
            } else {
                false
            }
        } else {
            self.ecchar += 1;
            self.ensure_selection_end_inbound();
            true
        }
    }

    // Ok indicates no errors.
    // Ok(true) indicates successful saving.
    // Ok(false) indicates no errors, but unsuccessful saving.
    // Err indicates fs::write has failed.
    pub fn save(&mut self) -> Option<bool> {
        if !self.has_unsaved_changes {
            return Some(false);
        }
        if self.filepath.starts_with('*') {
            return Some(false);
        }
        let mut write_contents = self
            .contents
            .iter()
            .map(|s| s.trim_end())
            .fold(String::new(), |a, b| a + b + "\n");
        _ = write_contents.pop();
        self.content_history.push(ContentHistory {
            data: write_contents.clone(),
            cchar: self.cchar,
            cline: self.cline,
            ecchar: self.ecchar,
            ecline: self.ecline,
            top: self.top,
        });
        // PHASE3: this `64` is subject to change
        if self.content_history.len() > 64 {
            _ = self.content_history.remove(0);
        }
        fs::write(&self.filepath, write_contents).ok()?;
        self.has_unsaved_changes = false;
        Some(true)
    }

    pub fn undo(&mut self) -> bool {
        if let Some(prev) = self.content_history.pop() {
            self.contents = prev
                .data
                .split('\n')
                .map(|line| line.trim_end())
                .map(|line| line.to_string())
                .collect();
            self.cline = prev.cline;
            self.cchar = prev.cchar;
            self.ecline = prev.ecline;
            self.ecchar = prev.ecchar;
            self.top = prev.top;
            true
        } else {
            let mut write_contents = self
                .contents
                .iter()
                .map(|s| s.trim_end())
                .fold(String::new(), |a, b| a + b + "\n");
            _ = write_contents.pop();
            self.content_history.push(ContentHistory {
                data: write_contents.clone(),
                cchar: self.cchar,
                cline: self.cline,
                ecline: self.ecline,
                ecchar: self.ecchar,
                top: self.top,
            });
            false
        }
    }

    pub fn home_nonwhitespace(&mut self) -> bool {
        if let Some(p) = self.contents[self.cline]
            .chars()
            .position(|c| !c.is_whitespace())
        {
            self.cchar = p;
            true
        } else {
            false
        }
    }

    pub fn end(&mut self) {
        self.ecchar = self.cchar;
        self.cchar = self.contents[self.cline].chars().count();
    }

    // backspaces until pred(self.backspace()) no longer returns true.
    pub fn backspace_while<P>(&mut self, mut pred: P)
    where
        P: FnMut(&char) -> bool,
    {
        while let Some(lc) = self.backspace() {
            if !pred(&lc) {
                if lc == '\n' {
                    self.enter();
                } else {
                    _ = self.type_char(lc);
                }
                break;
            }
        }
    }

    pub fn seek_forward_until<P>(&mut self, mut pred: P) -> bool
    where
        P: FnMut(char) -> bool,
    {
        self.ecchar = self.cchar;
        self.ecline = self.cline;
        let mut curr_line = self.contents[self.cline].chars();
        for _ in 0..(self.cchar + 1) {
            _ = curr_line.next();
        }
        if let Some(p) = curr_line.position(&mut pred) {
            self.cchar += p + 1;
            return true;
        }
        if self.cline + 1 == self.contents.len() {
            return false;
        }
        let mut ctr = self.cline + 1;
        for line in &mut self.contents[self.cline + 1..] {
            if let Some(p) = line.chars().position(&mut pred) {
                self.cline = ctr;
                self.cchar = p;
                self.adjust_top();
                return true;
            }
            ctr += 1;
        }
        false
    }

    pub fn seek_backward_until<P>(&mut self, mut pred: P) -> bool
    where
        P: FnMut(char) -> bool,
    {
        self.ecchar = self.cchar;
        self.ecline = self.cline;
        let mut curr_line = self.contents[self.cline].chars().rev();
        let clen = curr_line.clone().count();
        for _ in self.cchar..clen {
            _ = curr_line.next();
        }
        if let Some(p) = curr_line.position(&mut pred) {
            self.cchar -= p + 1;
            return true;
        }
        if self.cline == 0 {
            return false;
        }
        let mut ctr = self.cline - 1;
        while ctr != 0 {
            let line = &self.contents[ctr];
            let mut chars = line.chars();
            if let Some(p) = chars.position(&mut pred) {
                self.cline = ctr;
                self.cchar = p;
                self.adjust_top();
                return true;
            }
            ctr -= 1;
        }
        false
    }

    pub fn adjust_top(&mut self) {
        let (_, terminal_height_u16) = terminal::size().expect("Terminal should have a size");
        let terminal_height = terminal_height_u16 as usize;
        let padding_size = 3;
        let bottom_rows = 2;
        if self.cline < self.top + padding_size {
            self.top = self.cline.saturating_sub(padding_size);
        }
        if self.cline > (self.top + terminal_height).saturating_sub(padding_size + bottom_rows) {
            self.top = self
                .cline
                .saturating_sub(terminal_height.saturating_sub(padding_size + bottom_rows))
        }
    }

    pub fn determine_indent(&mut self) {
        // PHASE2: get size of indent with language.
        self.indent_level = self.contents[self.cline]
            .chars()
            .take_while(|c| c.is_whitespace())
            .count()
            / 4;
    }

    pub fn find_empty_line_forward(&mut self) {
        let mut lines = self.contents.iter();
        for _ in 0..self.cline + 1 {
            _ = lines.next();
        }
        if let Some(l) = lines.position(|l| l.trim().is_empty()) {
            self.cline += l + 1;
            self.ensure_cursor_inbound();
            self.adjust_top();
        }
    }

    pub fn find_empty_line_backward(&mut self) {
        if let Some(l) = self
            .contents
            .iter()
            .take(self.cline.saturating_sub(1))
            .rposition(|l| l.trim().is_empty())
        {
            self.cline = l;
            self.ensure_cursor_inbound();
            self.adjust_top();
        }
    }

    pub fn delete_selection(&mut self) {
        let (cline, ecline) = if self.cline > self.ecline {
            (self.ecline, self.cline)
        } else {
            (self.cline, self.ecline)
        };
        let (cchar, ecchar) = if self.cline == self.ecline
            && self.cchar > self.ecchar {
            (self.ecchar, self.cchar)
        } else {
            (self.cchar, self.ecchar)
        };
        for (lnumberr, line) in self.contents[cline..(ecline + 1)]
            .iter_mut()
            .enumerate()
        {
            let lnumber = lnumberr + cline;
            if lnumber == cline && cline == ecline {
                let old_line = line.clone();
                line.clear();
                for (i, c) in old_line.chars().enumerate() {
                    if !(i < ecchar && i >= cchar) {
                        line.push(c);
                    }
                }
                if line.is_empty() {
                    line.push_str("DELETE ME NOW!! nshtm;cgaei")
                }
            } else if lnumber == cline {
                let old_line = line.clone();
                line.clear();
                for (i, c) in old_line.chars().enumerate() {
                    if i < cchar {
                        line.push(c);
                    } else {
                        break;
                    }
                }
                if line.is_empty() {
                    line.push_str("DELETE ME NOW!! nshtm;cgaei");
                }
            } else if lnumber == ecline {
                let old_line = line.clone();
                line.clear();
                for (i, c) in old_line.chars().enumerate() {
                    if i >= ecchar {
                        line.push(c)
                    }
                }
                if line.is_empty() {
                    line.push_str("DELETE ME NOW!! nshtm;cgaei");
                }
            } else {
                *line = String::from("DELETE ME NOW!! nshtm;cgaei")
            }
        }
        let mut new_contents: Vec<String> = self.contents
            .iter()
            .filter(|s| **s != "DELETE ME NOW!! nshtm;cgaei")
            .cloned()
            .collect();
        self.contents.clear();
        self.cline = cline;
        self.cchar = cchar;
        self.contents.append(&mut new_contents);
        self.ensure_cursor_inbound();
        self.adjust_top();
        self.has_unsaved_changes = true;
    }

    pub fn is_within_selection(&self, line: usize, idx: usize) -> bool {
        let (cline, ecline, cchar, ecchar) = if self.cline > self.ecline {
            (self.ecline, self.cline, self.ecchar, self.cchar)
        } else if self.cline == self.ecline {
            if self.cchar > self.ecchar {
                (self.ecline, self.cline, self.ecchar, self.cchar)
            } else {
                (self.cline, self.ecline, self.cchar, self.ecchar)
            }
        } else {
            (self.cline, self.ecline, self.cchar, self.ecchar)
        };
        if line > cline {
            if line < ecline {
                true
            } else if line == ecline {
                idx < ecchar
            } else {
                false
            }
        } else if line == cline && cline == ecline {
            idx >= cchar && idx < ecchar
        } else if line == cline {
            idx >= cchar
        } else {
            false
        }
    }

    pub fn make_selection_cursor(&mut self) {
        self.ecline = self.cline;
        self.ecchar = self.cchar + 1;
    }

    pub fn type_string(&mut self, source: &str) {
        let mut ln = 0;
        for ch in source.chars() {
            if ch == '\n' {
                ln += 1;
                self.contents.insert(self.cchar + ln, String::new());
            } else {
                _ = self.type_char(ch);
            }
        }
        self.has_unsaved_changes = true;
    }

    pub fn yank_selection(&mut self, dest: &mut String) {
        let (cline, ecline, cchar, ecchar) = if self.cline > self.ecline {
            (self.ecline, self.cline, self.ecchar, self.cchar)
        } else if self.cline == self.ecline {
            if self.cchar > self.ecchar {
                (self.ecline, self.cline, self.ecchar, self.cchar)
            } else {
                (self.cline, self.ecline, self.cchar, self.ecchar)
            }
        } else {
            (self.cline, self.ecline, self.cchar, self.ecchar)
        };
        for (lnumberr, line) in self.contents[cline..(ecline + 1)]
            .iter().enumerate() {
            let lnumber = lnumberr + cline;
            if lnumber == cline && cline == ecline {
                for (i, c) in line.chars().enumerate() {
                    if i >= cchar {
                        if i < ecchar {
                            dest.push(c);
                        } else {
                            break;
                        }
                    }
                }
            } else if lnumber == cline {
                for (i, c) in line.chars().enumerate() {
                    if i >= cchar {
                        dest.push(c)
                    } else {
                        break;
                    }
                }
            } else if lnumber == ecline {
                for c in line.chars().take(ecchar.saturating_sub(1)) {
                    dest.push(c);
                }
            } else {
                dest.push_str(line.clone().as_str());
            }
            dest.push('\n')
        }
    }

    pub fn flip_selection_ends(&mut self) {
        (self.cline, self.ecline, self.cchar, self.ecchar) = (
            self.ecline,
            self.cline,
            self.ecchar.saturating_sub(1),
            self.cchar,
        );
    }
}

#[derive(PartialEq, Copy, Clone)]
pub enum DefaultState {
    Normal,
    Till,
    TillBack,
    Command,
}

pub enum Mode {
    Default(DefaultState),
    Insert,
}

impl Mode {
    pub fn has_dialog(&self) -> bool {
        match self {
            Mode::Default(_) => true,
            Mode::Insert => false,
        }
    }
}

impl fmt::Display for Mode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Mode::Default(_) => write!(f, "default"),
            Mode::Insert => write!(f, "insert"),
        }
    }
}

pub enum Modifiers {
    None,
    Shift,
    Ctrl,
    Alt,
    CtrlShift,
    AltShift,
    CtrlAlt,
    CtrlAltShift,
}

pub enum EDAction {
    None,
    OpenFile(String),
    SetBufferHead(usize),
}

/*

Development cycle for commands in cim:

1. Declaration of function in main()
2. Development of function in an impl block

Always isolate functionality code out, never hard code it into a part of the
main function to ensure reusability from other modes.

*/

pub fn new_alert(
    alert: &mut Vec<String>,
    alert_spawn_instant: &mut time::Instant,
    alert_timeout: &mut time::Duration,
    message: &[String],
    timeout: time::Duration,
) {
    alert.clear();
    alert.extend_from_slice(message);
    *alert_spawn_instant = time::Instant::now();
    *alert_timeout = timeout;
}

#[macro_export]
macro_rules! repeat_action {
    ($temp_str: ident, $action: block) => {
        let num_str: String = $temp_str.chars().take_while(|c| c.is_numeric()).collect();
        for _ in 0..num_str.parse::<usize>().unwrap_or(1) {
            $action
        }
        $temp_str.clear();
    };
}

fn main() {
    let mut stdout = io::stdout();

    let mut mode = Mode::Default(DefaultState::Normal);
    let mut buffers: Vec<Buffer> = vec![];
    let mut buffer_head = 0;
    let mut alert: Vec<String> = vec![];
    let mut alert_spawn_instant: time::Instant = time::Instant::now();
    let mut alert_timeout: time::Duration = time::Duration::from_secs(10);
    let mut temp_str = String::new();
    let mut clipboard = String::new();

    let mut args = args().peekable();
    _ = args.next();
    if args.peek().is_some() {
        let path = args.collect::<Vec<String>>().join(" ");
        if path.ends_with(".exe") {
            println!("[INFO]: you shouldn't do this");
            return;
        }
        match Buffer::new(&path) {
            Ok(b) => {
                buffers.push(b);
            }
            Err(e) => {
                for l in e {
                    println!("{l}");
                }
                return;
            }
        }
    } else {
        buffers.push(Buffer::new_scratch())
    };

    print!("\x1bc\x1b[?25l");
    if stdout.flush().is_err() {
        return;
    };
    _ = terminal::enable_raw_mode();
    let mut action: EDAction;
    'ed: loop {
        action = EDAction::None;
        if buffer_head >= buffers.len() {
            buffer_head = buffers.len().saturating_sub(1);
        }
        let buf = buffers.get_mut(buffer_head).unwrap();

        let (terminal_width_u16, terminal_height_u16) =
            terminal::size().expect("Terminal should have a size");
        let terminal_width = terminal_width_u16 as usize;
        let terminal_height = terminal_height_u16 as usize;

        /*
        if buf.cline > buf.ecline {
            (buf.cline, buf.ecline) = (buf.ecline, buf.cline);
        }
        if buf.cchar > buf.ecchar && buf.cline == buf.ecline {
            (buf.cchar, buf.ecchar) = (buf.ecchar, buf.cchar);
        }
        */

        print!("\x1b[J\x1b[H");
        if alert_spawn_instant.elapsed() >= alert_timeout {
            alert.clear();
        }
        let alert_show_len = std::cmp::min(alert.len(), 16);
        let mut outbuf = String::new();
        let linenumwidth = format!("{}", buf.contents.len()).width_cjk();
        let mut clnumber = 0;
        'ctnt: for (lnumber, line) in buf.contents[buf.top..].iter().enumerate() {
            let mut width_printed = linenumwidth + 2;
            clnumber += 1;
            if clnumber + alert_show_len + 1 + 1 > terminal_height {
                break 'ctnt;
            }
            // PHASE2: change this to StyledChar
            // I will be leaving the ruler up to the syntax highlighter to
            // enforce.
            if lnumber + buf.top == buf.cline {
                _ = write!(
                    &mut outbuf,
                    "\x1b[36m{:>linenumwidth$}  \x1b[0m",
                    buf.top + lnumber + 1
                );
            } else {
                _ = write!(
                    &mut outbuf,
                    "\x1b[2m\x1b[36m{:>linenumwidth$}  \x1b[0m",
                    (lnumber + buf.top).abs_diff(buf.cline),
                );
            }
            let mut idx = 0;
            'ln: for ch in line.chars() {
                width_printed += ch.width_cjk().unwrap_or(0);
                if width_printed > terminal_width {
                    break 'ln;
                }
                match idx {
                    a if a == buf.cchar && lnumber + buf.top == buf.cline => {
                        _ = write!(&mut outbuf, "\x1b[43m\x1b[30m{ch}\x1b[0m");
                    }
                    b if b == buf.ecchar && lnumber + buf.top == buf.ecline => {
                        _ = write!(&mut outbuf, "\x1b[4m{ch}\x1b[0m");
                    }
                    _ if buf.is_within_selection(lnumber + buf.top, idx) => {
                        _ = write!(&mut outbuf, "\x1b[47m\x1b[30m{ch}\x1b[0m");
                    }
                    _ => {
                        outbuf.push(ch);
                    }
                }
                idx += 1;
            }
            while width_printed < terminal_width {
                width_printed += 1;
                match idx {
                    a if a == buf.cchar && lnumber + buf.top == buf.cline => {
                        _ = write!(&mut outbuf, "\x1b[43m\x1b[30m \x1b[0m");
                    }
                    b if b == buf.ecchar && lnumber + buf.top == buf.ecline => {
                        _ = write!(&mut outbuf, "\x1b[4m \x1b[0m");
                    }
                    _ if buf.is_within_selection(lnumber + buf.top, idx) => {
                        _ = write!(&mut outbuf, "\x1b[47m\x1b[30m \x1b[0m");
                    }
                    _ => {
                        outbuf.push(' ');
                    }
                }
                idx += 1;
            }
            outbuf.push('\n');
        }
        while clnumber + alert_show_len + 1 + 1 < terminal_height {
            clnumber += 1;
            for _ in 0..terminal_width {
                outbuf.push(' ');
            }
            outbuf.push('\n');
        }
        // PHASE3: Change this `16` if needed
        for line in alert.iter().take(16) {
            let mut width_printed = 0;
            outbuf.push_str("\x1b[47m\x1b[30m");
            'ln: for ch in line.chars() {
                width_printed += ch.width_cjk().unwrap_or(0);
                if width_printed > terminal_width {
                    break 'ln;
                }
                outbuf.push(ch);
            }
            while width_printed < terminal_width {
                width_printed += 1;
                outbuf.push(' ');
            }
            outbuf.push_str("\x1b[0m\n");
        }
        let bottom_bar = if buf.has_unsaved_changes {
            format!("[{}*]", buf.filepath)
        } else {
            format!("[{}]", buf.filepath)
        };
        {
            let mut width_printed = 0;
            'ln: for ch in bottom_bar.chars() {
                width_printed += ch.width_cjk().unwrap_or(0);
                if width_printed > terminal_width {
                    break 'ln;
                }
                outbuf.push(ch);
            }
            while width_printed < terminal_width {
                width_printed += 1;
                outbuf.push(' ');
            }
            outbuf.push('\n');
        };
        let mode_bar = if mode.has_dialog() {
            format!("{mode} :: {temp_str}")
        } else {
            format!("{mode}")
        };
        {
            let mut width_printed = 0;
            'ln: for ch in mode_bar.chars() {
                width_printed += ch.width_cjk().unwrap_or(0);
                if width_printed > terminal_width {
                    break 'ln;
                }
                outbuf.push(ch);
            }
            while width_printed < terminal_width {
                width_printed += 1;
                outbuf.push(' ');
            }
        };
        print!("{outbuf}");
        if stdout.flush().is_err() {
            break 'ed;
        }

        let event = event::read().expect("There should be an event");
        let key;
        if let Event::Key(k) = event {
            key = k;
        } else {
            continue 'ed;
        }
        if key.kind == event::KeyEventKind::Release {
            continue 'ed;
        }

        let modifier = {
            let mut has_alt = 0;
            let mut has_shift = 0;
            let mut has_ctrl = 0;
            for m in key.modifiers.iter() {
                fn btoi(b: bool) -> usize {
                    if b { 1 } else { 0 }
                }
                has_alt |= btoi(m == KeyModifiers::ALT);
                has_shift |= btoi(m == KeyModifiers::SHIFT);
                has_ctrl |= btoi(m == KeyModifiers::CONTROL);
            }
            if has_alt != 0 {
                if has_shift != 0 {
                    if has_ctrl != 0 {
                        Modifiers::CtrlAltShift
                    } else {
                        Modifiers::AltShift
                    }
                } else if has_ctrl != 0 {
                    Modifiers::CtrlAlt
                } else {
                    Modifiers::Alt
                }
            } else if has_shift != 0 {
                if has_ctrl != 0 {
                    Modifiers::CtrlShift
                } else {
                    Modifiers::Shift
                }
            } else if has_ctrl != 0 {
                Modifiers::Ctrl
            } else {
                Modifiers::None
            }
        };

        match mode {
            Mode::Default(DefaultState::Command) => match key.code {
                KeyCode::Char(n) => {
                    temp_str.push(n);
                }
                KeyCode::Enter => {
                    let ts = temp_str.clone();
                    temp_str.clear();
                    mode = Mode::Default(DefaultState::Normal);
                    let mut tsi = ts.chars().peekable();
                    if tsi.next() != Some(':') {
                        new_alert(
                            &mut alert,
                            &mut alert_spawn_instant,
                            &mut alert_timeout,
                            &[String::from("bad state: expected `:`")],
                            time::Duration::from_secs(1),
                        );
                        continue;
                    };
                    let mut cmd = String::new();
                    while let Some(x) = tsi.peek() {
                        if !x.is_whitespace() {
                            cmd.push(*x);
                            _ = tsi.next();
                        } else {
                            break;
                        }
                    }
                    let args = tsi.collect::<String>();

                    match cmd.trim() {
                        "o" | "open" => {
                            action = EDAction::OpenFile(args.trim().to_string());
                        }
                        "q" | "quit" => {
                            break 'ed;
                        }
                        "s" | "save" => {
                            if buf.save() == Some(true) {
                                new_alert(
                                    &mut alert,
                                    &mut alert_spawn_instant,
                                    &mut alert_timeout,
                                    &[String::from("save")],
                                    time::Duration::from_secs(1),
                                );
                            }
                        }
                        "n" | "next" => {
                            action = EDAction::SetBufferHead(buffer_head + 1);
                        }
                        "p" | "previous" => {
                            if buffer_head != 0 {
                                action = EDAction::SetBufferHead(buffer_head - 1);
                            }
                        }
                        "rl" | "reload_file" => {
                            if buf.reload_file() {
                                print!("\x1bc\x1b[?25l");
                            }
                        }
                        _ => {
                            new_alert(
                                &mut alert,
                                &mut alert_spawn_instant,
                                &mut alert_timeout,
                                &[format!("command `{cmd}` not recognised")],
                                time::Duration::from_secs(1),
                            );
                        }
                    }
                }
                KeyCode::Backspace => {
                    _ = temp_str.pop();
                    if temp_str.is_empty() {
                        mode = Mode::Default(DefaultState::Normal);
                    }
                }
                _ => {}
            },

            Mode::Default(DefaultState::Till) => match key.code {
                KeyCode::Char(n) => {
                    repeat_action!(temp_str, {
                        _ = buf.seek_forward_until(|c| c == n);
                    });
                    mode = Mode::Default(DefaultState::Normal);
                }
                _ => {
                    temp_str.clear();
                }
            },

            Mode::Default(DefaultState::TillBack) => match key.code {
                KeyCode::Char(n) => {
                    repeat_action!(temp_str, {
                        _ = buf.seek_backward_until(|c| c == n);
                    });
                    mode = Mode::Default(DefaultState::Normal);
                }
                _ => {
                    temp_str.clear();
                }
            },

            Mode::Default(_) => match modifier {
                Modifiers::None | Modifiers::Shift => match key.code {
                    KeyCode::Char('c') => {
                        repeat_action!(temp_str, {
                            _ = buf.move_left();
                        });
                    }
                    KeyCode::Char('a') => {
                        repeat_action!(temp_str, {
                            _ = buf.move_down();
                        });
                    }
                    KeyCode::Char('e') => {
                        repeat_action!(temp_str, {
                            _ = buf.move_up();
                        });
                    }
                    KeyCode::Char('i') => {
                        repeat_action!(temp_str, {
                            _ = buf.move_right();
                        });
                    }
                    KeyCode::Char('C') => {
                        repeat_action!(temp_str, {
                            _ = buf.extend_left();
                        });
                    }
                    KeyCode::Char('A') => {
                        repeat_action!(temp_str, {
                            _ = buf.extend_down();
                        });
                    }
                    KeyCode::Char('E') => {
                        repeat_action!(temp_str, {
                            _ = buf.extend_up();
                        });
                    }
                    KeyCode::Char('I') => {
                        repeat_action!(temp_str, {
                            _ = buf.extend_right();
                        });
                    }
                    KeyCode::Char('q') => {
                        mode = Mode::Insert;
                    }
                    KeyCode::Home => {
                        _ = buf.home_nonwhitespace();
                    }
                    KeyCode::Char(n) if n.is_numeric() => {
                        if n == '0' && temp_str.is_empty() {
                            _ = buf.home_nonwhitespace();
                        } else {
                            temp_str.push(n);
                        }
                    }
                    KeyCode::Char('$') | KeyCode::End => {
                        buf.end();
                    }
                    KeyCode::Char('t') => {
                        mode = Mode::Default(DefaultState::Till);
                        temp_str.push('t');
                    }
                    KeyCode::Char('T') => {
                        mode = Mode::Default(DefaultState::TillBack);
                        temp_str.push('T');
                    }
                    KeyCode::Char(':') => {
                        mode = Mode::Default(DefaultState::Command);
                        temp_str.push(':');
                    }
                    KeyCode::Char(']') => {
                        repeat_action!(temp_str, {
                            buf.find_empty_line_forward();
                        });
                    }
                    KeyCode::Char('[') => {
                        repeat_action!(temp_str, {
                            buf.find_empty_line_backward();
                        });
                    }
                    KeyCode::Char('g') => {
                        buf.cline = temp_str.parse::<usize>().unwrap_or(0);
                        buf.ensure_cursor_inbound();
                        buf.adjust_top();
                        temp_str.clear();
                    }
                    KeyCode::Char('w') => {
                        repeat_action!(temp_str, {
                            _ = buf.seek_forward_until(|c| c.is_whitespace());
                            _ = buf.seek_forward_until(|c| !c.is_whitespace());
                        });
                    }
                    KeyCode::Char('b') => {
                        repeat_action!(temp_str, {
                            _ = buf.seek_backward_until(|c| !c.is_whitespace());
                            _ = buf.seek_backward_until(|c| c.is_whitespace());
                        });
                    }
                    KeyCode::Char('u') => {
                        repeat_action!(temp_str, {
                            if !buf.undo() {
                                new_alert(
                                    &mut alert,
                                    &mut alert_spawn_instant,
                                    &mut alert_timeout,
                                    &[String::from("already at oldest change")],
                                    time::Duration::from_secs(1),
                                );
                            }
                        });
                    }
                    KeyCode::Char('o') => {
                        repeat_action!(temp_str, {
                            buf.end();
                            buf.enter();
                        });
                        mode = Mode::Insert;
                    }
                    KeyCode::Char('O') => {
                        repeat_action!(temp_str, {
                            _ = buf.home_nonwhitespace();
                            buf.enter();
                            _ = buf.move_up();
                        });
                        mode = Mode::Insert;
                    }
                    KeyCode::Char('D') => {
                        temp_str.clear();
                        buf.delete_selection();
                        mode = Mode::Insert;
                    }
                    KeyCode::Char('d') => {
                        temp_str.clear();
                        buf.delete_selection();
                    }
                    KeyCode::Char(';') => {
                        temp_str.clear();
                        buf.make_selection_cursor();
                    }
                    KeyCode::Char('y') => {
                        buf.yank_selection(&mut clipboard);
                    }
                    KeyCode::Char('p') => {
                        repeat_action!(temp_str, {
                            buf.type_string(&clipboard);
                        });
                        clipboard.clear();
                    }
                    KeyCode::Char('x') => {
                        repeat_action!(temp_str, {
                            buf.cchar = 0;
                            buf.ecchar = 0;
                            buf.ecline += 1;
                            buf.ensure_selection_end_inbound();
                        });
                    }
                    KeyCode::Char(',') => {
                        buf.flip_selection_ends();
                        temp_str.clear();
                    }
                    _ => {}
                },
                _ => {}
            },

            Mode::Insert => match modifier {
                Modifiers::None | Modifiers::Shift => match key.code {
                    KeyCode::Char(n) => {
                        _ = buf.type_char(n);
                    }
                    KeyCode::Backspace => {
                        // PHASE2: handle error to update highlighting.
                        // Could also handle error within method itself to
                        // update highlighting.
                        _ = buf.backspace();
                    }
                    KeyCode::Enter => {
                        buf.enter();
                    }
                    KeyCode::Up => {
                        _ = buf.move_up();
                    }
                    KeyCode::Down => {
                        _ = buf.move_down();
                    }
                    KeyCode::Left => {
                        _ = buf.move_left();
                    }
                    KeyCode::Right => {
                        _ = buf.move_right();
                    }
                    KeyCode::Home => {
                        _ = buf.home_nonwhitespace();
                    }
                    KeyCode::End => {
                        buf.end();
                    }
                    _ => {}
                },
                Modifiers::Alt | Modifiers::AltShift => match key.code {
                    KeyCode::Char('q') => {
                        break 'ed;
                    }
                    KeyCode::Char('c') => {
                        _ = buf.move_left();
                    }
                    KeyCode::Char('a') => {
                        _ = buf.move_down();
                    }
                    KeyCode::Char('e') => {
                        _ = buf.move_up();
                    }
                    KeyCode::Char('i') => {
                        _ = buf.move_right();
                    }
                    KeyCode::Char('s') => {
                        if buf.save() == Some(true) {
                            new_alert(
                                &mut alert,
                                &mut alert_spawn_instant,
                                &mut alert_timeout,
                                &[String::from("save")],
                                time::Duration::from_secs(1),
                            );
                        }
                    }
                    KeyCode::Char('u') => {
                        if !buf.undo() {
                            new_alert(
                                &mut alert,
                                &mut alert_spawn_instant,
                                &mut alert_timeout,
                                &[String::from("already at oldest change")],
                                time::Duration::from_secs(1),
                            );
                        }
                    }
                    KeyCode::Char('n') => {
                        mode = Mode::Default(DefaultState::Normal);
                    }
                    _ => {}
                },
                Modifiers::Ctrl | Modifiers::CtrlShift => match key.code {
                    KeyCode::Backspace => {
                        buf.backspace_while(|c| c.is_whitespace());
                        buf.backspace_while(|c| c.is_alphanumeric());
                    }
                    KeyCode::Char('n') => {
                        action = EDAction::SetBufferHead(buffer_head + 1);
                    }
                    KeyCode::Char('p') => {
                        if buffer_head > 0 {
                            action = EDAction::SetBufferHead(buffer_head - 1);
                        }
                    }
                    _ => {}
                },
                _ => {}
            },
        }

        match action {
            EDAction::None => {}
            EDAction::OpenFile(path) => match Buffer::new(&path) {
                Ok(b) => {
                    if let Some(pos) = buffers.iter().position(|s| s.filepath == path) {
                        buffer_head = pos;
                    } else {
                        buffers.insert(buffer_head + 1, b);
                        buffer_head += 1;
                    }
                }
                Err(v) => {
                    alert = v;
                    alert_spawn_instant = time::Instant::now();
                    alert_timeout = time::Duration::from_secs(1);
                }
            },
            EDAction::SetBufferHead(nbh) => {
                if nbh < buffers.len() {
                    buffer_head = nbh;
                }
            }
        }
    }
    print!("\x1bc\x1b[?25h");
    _ = terminal::disable_raw_mode();
}
