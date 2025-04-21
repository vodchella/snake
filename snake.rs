/*
 *  Для сборки предварительно необходимо:
 *    - Инициализировать фейковый проект: cargo init --bin .
 *    - Откатить изменения в .gitignore
 *    - В Cargo.toml в раздел dependencies прописать: libc = "0.2"
 *    - Собрать проект: cargo build --release
 *  Для сборки и запуска с помощью cargo:
 *    - Выполнить: cargo run
 *  Для сборки и запуска с помощью rustc:
 *    - В переменную окружения поместить путь к libc rlib: libc_rlib=$(ls target/release/deps/liblibc-*.rlib)
 *    - Запустить сборку: rustc snake.rs -L dependency=target/release/deps --extern libc=$libc_rlib
 */

extern crate libc;

use std::io::{self, Read, Write};
use std::os::unix::io::AsRawFd;
use std::time::Duration;
use std::{thread};
use libc::{termios, tcgetattr, tcsetattr, TCSANOW, ECHO, ICANON};


const ESC:        u8 = 0x1B;
const ASCII_ESC:  &str = "\x1B";
const WND_WIDTH:  i8 = 46;
const WND_HEIGHT: i8 = 15;
const WND_WIDTH_MIDDLE:  i8 = WND_WIDTH/ 2;
const WND_HEIGHT_MIDDLE: i8 = WND_HEIGHT/ 2;


fn clear_screen() {
    print!("{ASCII_ESC}[2J{ASCII_ESC}[H");
    io::stdout().flush().unwrap();
}

fn move_cursor(x: i8, y: i8) {
    let pos_x: i8 = x + 1;
    let pos_y: i8 = y + 1;
    print!("{ASCII_ESC}[{pos_y};{pos_x}H");
    io::stdout().flush().unwrap();
}

fn show_cursor() {
    print!("{ASCII_ESC}[?25h");
    io::stdout().flush().unwrap();
}

fn hide_cursor() {
    print!("{ASCII_ESC}[?25l");
    io::stdout().flush().unwrap();
}

fn write_char(c: char, x: i8, y: i8) {
    move_cursor(x, y);
    print!("{}", c);
    hide_cursor();
    io::stdout().flush().unwrap();
}

fn draw_borders() {
    let horizontal = "-".repeat((WND_WIDTH - 2) as usize);
    let empty_space = " ".repeat((WND_WIDTH - 2) as usize);

    println!("+{}+", horizontal);
    for _ in 0..(WND_HEIGHT - 2) {
        println!("|{}|", empty_space);
    }
    println!("+{}+", horizontal);
}

fn main() {
    let stdin = io::stdin();
    let fd = stdin.as_raw_fd();
    let mut term: termios = unsafe { std::mem::zeroed() };

    clear_screen();
    draw_borders();
    write_char('@', WND_WIDTH_MIDDLE, WND_HEIGHT_MIDDLE);

    unsafe {
        tcgetattr(fd, &mut term);
        let original = term;
        term.c_lflag &= !(ICANON | ECHO);
        tcsetattr(fd, TCSANOW, &term);

        let mut buffer = [0u8; 1];
        loop {
            if let Ok(n) = stdin.lock().read(&mut buffer) {
                if n > 0 {
                    if buffer[0] == ESC {
                        break;
                    }
                    println!("Код: {}", buffer[0]);
                }
            }
            thread::sleep(Duration::from_millis(10));
        }
        tcsetattr(fd, TCSANOW, &original);
    }

    show_cursor();
}
