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

use std::io::{self, Read, Write, stdout};
use std::os::unix::io::AsRawFd;
use std::sync::{Arc, Mutex};
use std::process::exit;
use std::thread::{sleep, spawn};
use std::time::Duration;
use libc::{termios, tcgetattr, tcsetattr, TCSANOW, ECHO, ICANON, sigaction, sigemptyset, sighandler_t, SIGINT, SIGTERM, SIGQUIT, SIGTSTP};


const ASCII_ESC:         &str = "\x1B";
const ESC:               u8 = 0x1B;
const CSI:               u8 = 91;
const ARROW_UP:          u8 = 65;
const ARROW_DOWN:        u8 = 66;
const ARROW_RIGHT:       u8 = 67;
const ARROW_LEFT:        u8 = 68;
const WND_WIDTH:         i8 = 46;
const WND_HEIGHT:        i8 = 15;
const WND_WIDTH_MIDDLE:  i8 = WND_WIDTH / 2;
const WND_HEIGHT_MIDDLE: i8 = WND_HEIGHT / 2;
const SNAKE_DELAY:       Duration = Duration::from_millis(500);
const SNAKE_LENGTH:      usize = 8;
const SNAKE_BODY:        char = '*';
const SNAKE_HEAD_CHARS:  [char; 4] = [
    '^', '>', 'v', '<'
];
const HWALL:             &str = "-";
const VWALL:             &str = "|";
const CORNER:            &str = "+";
const EMPTY_SPACE:       char = ' ';
const DISSALLOWED_DIRS:  [SnakeDirection; 4] = [
    SnakeDirection::Down,
    SnakeDirection::Left,
    SnakeDirection::Up,
    SnakeDirection::Right,
];
const MOVING_RULES:      [Point; 4] = [
    Point{ x:  0, y: -1 },
    Point{ x:  1, y:  0 },
    Point{ x:  0, y:  1 },
    Point{ x: -1, y:  0 },
];


#[derive(PartialEq, Copy, Clone)]
enum SnakeDirection {
    Up,
    Right,
    Down,
    Left,
}

struct GameState {
    should_exit: bool,
    exit_code: i32,
    snake_dir: SnakeDirection,
}

#[derive(Copy, Clone)]
struct Point {
    x: i8,
    y: i8,
}


extern "C" fn handle_signal(_: i32) {
    // We just need empty handler
}

fn setup_signal_handlers() {
    unsafe {
        let mut action: sigaction = std::mem::zeroed();
        action.sa_sigaction = handle_signal as sighandler_t;
        action.sa_flags = 0; // Without SA_RESTART!
        sigemptyset(&mut action.sa_mask);

        for &sig in &[SIGINT, SIGTERM, SIGQUIT, SIGTSTP] {
            sigaction(sig, &action, std::ptr::null_mut());
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
    stdout().flush().unwrap();
}

fn move_cursor(x: i8, y: i8) {
    let pos_x: i8 = x + 1;
    let pos_y: i8 = y + 1;
    print!("{ASCII_ESC}[{pos_y};{pos_x}H");
    stdout().flush().unwrap();
}

fn show_cursor() {
    print!("{ASCII_ESC}[?25h");
    stdout().flush().unwrap();
}

fn hide_cursor() {
    print!("{ASCII_ESC}[?25l");
    stdout().flush().unwrap();
}

fn write_char(c: char, x: i8, y: i8) {
    move_cursor(x, y);
    print!("{}", c);
    hide_cursor();
    stdout().flush().unwrap();
}

fn draw_borders() {
    let horizontal = HWALL.repeat((WND_WIDTH - 2) as usize);
    let empty = EMPTY_SPACE.to_string().repeat((WND_WIDTH - 2) as usize);
    let h_wall = format!("{}{}{}", CORNER, horizontal, CORNER);
    let v_wall = format!("{}{}{}", VWALL, empty, VWALL);

    println!("{}", h_wall);
    for _ in 0..(WND_HEIGHT - 2) {
        println!("{}", v_wall);
    }
    println!("{}", h_wall);
}

fn cleanup_and_exit(fd: i32, orig: &termios, exit_code: i32) -> ! {
    restore_mode(fd, orig);
    show_cursor();
    clear_screen();
    exit(exit_code);
}


fn snake_worker(game_state_ref: Arc<Mutex<GameState>>) {
    let mut current_dir = SnakeDirection::Up;
    let mut snake_body: [Point; SNAKE_LENGTH] = [Point{ x: 0, y: 0 }; SNAKE_LENGTH];

    fn snake_init(body: &mut [Point; SNAKE_LENGTH]) {
        body[0].x = WND_WIDTH_MIDDLE;
        body[0].y = WND_HEIGHT_MIDDLE - 1;
        for i in 1..body.len() {
            body[i].x = body[i - 1].x;
            body[i].y = body[i - 1].y + 1;
        }
    }

    fn snake_move_and_draw(body: &mut [Point; SNAKE_LENGTH], dir: SnakeDirection) {
        let tail = body[SNAKE_LENGTH - 1];
        write_char(EMPTY_SPACE, tail.x, tail.y);

        for i in (1..body.len()).rev() {
            body[i] = body[i - 1];
            write_char(SNAKE_BODY, body[i].x, body[i].y);
        }

        let rule = MOVING_RULES[dir as usize];
        body[0].x += rule.x;
        body[0].y += rule.y;
        write_char(SNAKE_HEAD_CHARS[dir as usize], body[0].x, body[0].y);
    }

    fn snake_has_collisions(body: &[Point; SNAKE_LENGTH]) -> bool {
        let head_x = body[0].x + 1;
        let head_y = body[0].y + 1;
        if [1, WND_HEIGHT].contains(&head_y) || [1, WND_WIDTH].contains(&head_x) {
            return true;
        }
        for i in 4..body.len() {
            if body[i].x == body[0].x && body[i].y == body[0].y {
                return true;
            }
        }
        false
    }

    snake_init(&mut snake_body);

    loop {
        let mut game_state = game_state_ref.lock().unwrap();
        if game_state.should_exit || game_state.exit_code != 0 {
            break;
        }

        let try_dir = &game_state.snake_dir;
        if *try_dir != current_dir && *try_dir != DISSALLOWED_DIRS[current_dir as usize] {
            current_dir = *try_dir;
        }
        snake_move_and_draw(&mut snake_body, current_dir);
        if snake_has_collisions(&snake_body) {
            move_cursor(WND_WIDTH_MIDDLE - 5, WND_HEIGHT_MIDDLE);
            println!("Game over!");
            game_state.should_exit = true;
        }

        drop(game_state);
        sleep(SNAKE_DELAY);
    }
}

fn main() {
    let exit_code: i32;
    let stdin = io::stdin();
    let fd = stdin.as_raw_fd();
    let mut orig_termios: termios = unsafe { std::mem::zeroed() };

    setup_signal_handlers();
    set_raw_mode(fd, &mut orig_termios);
    clear_screen();
    draw_borders();

    let game_state_ref = Arc::new(Mutex::new(GameState {
        should_exit: false,
        exit_code: 0,
        snake_dir: SnakeDirection::Up,
    }));
    let cloned = Arc::clone(&game_state_ref);
    spawn(move || {
        snake_worker(cloned);
    });


    let mut buffer = [0; 3];
    loop {
        let bytes_read = stdin.lock().read(&mut buffer);
        let mut game_state = game_state_ref.lock().unwrap();

        match bytes_read {
            Ok(0) => { // EOF
                game_state.exit_code = 1;
            }
            Ok(bytes_read) => match &buffer[..bytes_read] {
                [ESC, CSI, ARROW_UP]    => game_state.snake_dir = SnakeDirection::Up,
                [ESC, CSI, ARROW_DOWN]  => game_state.snake_dir = SnakeDirection::Down,
                [ESC, CSI, ARROW_RIGHT] => game_state.snake_dir = SnakeDirection::Right,
                [ESC, CSI, ARROW_LEFT]  => game_state.snake_dir = SnakeDirection::Left,
                [ESC, ..] => {
                    game_state.should_exit = true;
                }
                [0x04] => { // Ctrl+D
                        game_state.exit_code = 1;
                    }
                _ => {}
            }
            Err(_e) => {
                game_state.exit_code = 1;
            }
        }

        buffer = [0; 3];
        if game_state.should_exit || game_state.exit_code != 0 {
            exit_code = game_state.exit_code;
            break;
        }
    }

    cleanup_and_exit(fd, &orig_termios, exit_code);
}
