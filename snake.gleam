// gleam.toml:
// 
//     [dependencies]
//     gleam_stdlib = ">= 0.44.0 and < 2.0.0"
//     gleam_erlang = ">= 0.34.0 and < 1.0.0"
//
// Run it with: gleam run

import gleam/erlang/process.{sleep}
import gleam/io.{print, println}
import gleam/int.{is_even, to_string, random}
import gleam/list.{drop}
import gleam/option.{unwrap, type Option, Some, None}


type SnakeDirection      { Up Right Down Left }
const snake_head_chars   = ["^", ">", "v", "<"]
const disallowed_dirs    = [Down, Left, Up, Right]
const ascii_esc: String  = "\u{001b}"
const wnd_width          = 46
const wnd_height         = 15


fn clear_screen() {
    print(ascii_esc <> "[2J" <> ascii_esc <> "[H")
}

fn move_cursor(x, y : Int) {
    print(ascii_esc <> "[" <> to_string(y + 1) <> ";" <> to_string(x + 1) <> "H")
}

fn list_item_at(lst: List(t), i: Int) -> Option(t) {
    case drop(lst, i) {
        [val, .._] -> Some(val)
        []         -> None
    }
}

fn int_to_dir(index: Int) -> SnakeDirection {
    case index {
        0 -> Up
        1 -> Right
        2 -> Down
        3 -> Left
        _ -> panic as "ERROR: Invalid snake direction index"
    }
}

fn dir_to_int(dir: SnakeDirection) -> Int {
    case dir {
        Up    -> 0
        Right -> 1
        Down  -> 2
        Left  -> 3
    }
}

fn get_snake_head_char(dir: SnakeDirection) -> String {
    case list_item_at(snake_head_chars, dir_to_int(dir)) {
        Some(char) -> char
        None       -> panic as "ERROR: Invalid snake direction"
    }
}

fn get_next_random_dir(dir: SnakeDirection) -> SnakeDirection {
    let rand_index = random(4)
    let possible_dir = int_to_dir(rand_index)
    let dir_index = dir_to_int(dir)
    let disallowed_dir = list_item_at(disallowed_dirs, dir_index)
                         |> unwrap(dir)
    case possible_dir {
        d if d == disallowed_dir -> get_next_random_dir(dir)
        _ -> possible_dir
    }
}

fn loop(cur_dir: SnakeDirection, tick: Int) {
    move_cursor({wnd_width / 2} - 5, wnd_height / 2)
    println("Head: " <> get_snake_head_char(cur_dir))
    sleep(500)
    let new_dir = case is_even(tick) {
        True  -> get_next_random_dir(cur_dir)
        False -> cur_dir
    }
    loop(new_dir, tick + 1)
}

pub fn main() {
    clear_screen()
    loop(Up, 1)
}
