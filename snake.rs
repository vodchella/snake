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
use std::process;
use libc::{termios, tcgetattr, tcsetattr, TCSANOW, ECHO, ICANON, sigaction, sighandler_t, SIGINT, SIGTERM, SIGQUIT, SIGTSTP};


const ESC:        u8 = 0x1B;
const ASCII_ESC:  &str = "\x1B";
const WND_WIDTH:  i8 = 46;
const WND_HEIGHT: i8 = 15;
const WND_WIDTH_MIDDLE:  i8 = WND_WIDTH / 2;
const WND_HEIGHT_MIDDLE: i8 = WND_HEIGHT / 2;


extern "C" fn handle_signal(_: i32) {
    // We just need empty handler
}

fn setup_signal_handlers() {
    unsafe {
        let mut action: sigaction = std::mem::zeroed();
        action.sa_sigaction = handle_signal as sighandler_t;
        action.sa_flags = 0; // Without SA_RESTART!
        libc::sigemptyset(&mut action.sa_mask);

        for &sig in &[SIGINT, SIGTERM, SIGQUIT, SIGTSTP] {
            libc::sigaction(sig, &action, std::ptr::null_mut());
        }
    }
}

fn set_raw_mode(fd: i32, orig: &mut termios) {
    unsafe {
        tcgetattr(fd, orig);
        let mut raw = *orig;
        raw.c_lflag &= !(ICANON | ECHO);
        tcsetattr(fd, TCSANOW, &raw);
    }
}

fn restore_mode(fd: i32, orig: &termios) {
    unsafe {
        tcsetattr(fd, TCSANOW, orig);
    }
}

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

fn cleanup_and_exit(fd: i32, orig: &termios, exit_code: i32) -> ! {
    restore_mode(fd, orig);
    show_cursor();
    process::exit(exit_code);
}

fn main() {
    let stdin = io::stdin();
    let fd = stdin.as_raw_fd();
    let mut orig_termios: termios = unsafe { std::mem::zeroed() };

    setup_signal_handlers();
    set_raw_mode(fd, &mut orig_termios);
    clear_screen();
    draw_borders();
    write_char('@', WND_WIDTH_MIDDLE, WND_HEIGHT_MIDDLE);

    let mut buffer = [0; 3];
    loop {
        match stdin.lock().read(&mut buffer) {
            Ok(0) => {
                    println!("[!] EOF получен. Завершаем.");
                    cleanup_and_exit(fd, &orig_termios, 1);
            }
            Ok(n) => match &buffer[..n] {
                [ESC, 91, 65] => println!("Стрелка вверх"),
                [ESC, 91, 66] => println!("Стрелка вниз"),
                [ESC, 91, 67] => println!("Стрелка вправо"),
                [ESC, 91, 68] => println!("Стрелка влево"),
                [ESC, ..]     => {
                    println!("Нажат ESC — выход.");
                    break;
                }
                [0x04] => {
                        println!("Ctrl+D — выход");
                        cleanup_and_exit(fd, &orig_termios, 1);
                    }
                _ => {}
            }
            Err(e) => {
                eprintln!("\n[!] Ошибка чтения: {e}");
                cleanup_and_exit(fd, &orig_termios, 1);
            }
        }

        buffer = [0; 3];
    }

    cleanup_and_exit(fd, &orig_termios, 0);
}
