// gleam.toml:
// 
//     [dependencies]
//     gleam_stdlib = ">= 0.44.0 and < 2.0.0"
//     gleam_erlang = ">= 0.34.0 and < 1.0.0"
//
// Run it with: gleam run

import gleam/erlang/process
import gleam/io
import gleam/list
import gleam/option.{type Option, Some, None}


const snake_head_chars = ["^", ">", "v", "<"]

type SnakeDirection {
    Up
    Right
    Down
    Left
}


fn list_at(lst: List(t), i: Int) -> Option(t) {
    case list.drop(lst, i) {
        [val, .._] -> Some(val)
        []         -> None
    }
}

fn dir_to_int(dir: SnakeDirection) -> Int {
    case dir {
        Up -> 0
        Right -> 1
        Down  -> 2
        Left  -> 3
    }
}

fn get_snake_head_char(dir: SnakeDirection) -> String {
    case list_at(snake_head_chars, dir_to_int(dir)) {
        Some(char) -> char
        None       -> panic as "ERROR: Invalid snake direction"
    }
}

fn loop() {
    let cur_dir = Up
    io.println("Head: " <> get_snake_head_char(cur_dir))
    process.sleep(500)
    loop()
}

pub fn main() {
    loop()
}
