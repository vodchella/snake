// gleam.toml:
// 
//     [dependencies]
//     gleam_stdlib = ">= 0.44.0 and < 2.0.0"
//     gleam_erlang = ">= 0.34.0 and < 1.0.0"
//
// Run it with: gleam run

import gleam/bool.{lazy_guard}
import gleam/erlang/process.{sleep}
import gleam/io.{print, println}
import gleam/int.{is_even, to_string, random}
import gleam/list.{drop, each, range}
import gleam/option.{lazy_unwrap, type Option, Some, None}
import gleam/string.{repeat}


type  SnakeDirection     { Up Right Down Left }
const snake_head_chars   = ["^", ">", "v", "<"]
const disallowed_dirs    = [Down, Left, Up, Right]
const ascii_esc          = "\u{001b}"
const wnd_width          = 46
const wnd_height         = 15
const hwall              = "-"
const vwall              = "|"
const corner             = "+"
const space              = " "


fn clear_screen() {
    print(ascii_esc <> "[2J" <> ascii_esc <> "[H")
}

fn move_cursor(x, y : Int) {
    print(ascii_esc <> "[" <> to_string(y + 1) <> ";" <> to_string(x + 1) <> "H")
}

fn draw_borders() {
    let cnt = wnd_width - 2
    let edge_row = corner <> repeat(hwall, cnt) <> corner
    let middle_row = vwall <> repeat(space, cnt) <> vwall

    println(edge_row)

    range(1, wnd_height - 2)
    |> each(fn(_) { println(middle_row) })

    println(edge_row)
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


fn snake_get_next_random_dir(dir: SnakeDirection) -> SnakeDirection {
    let new_dir = random(4) |> int_to_dir()
    let disallowed_dir = dir_to_int(dir)
                         |> list_item_at(disallowed_dirs, _)
                         |> lazy_unwrap(fn() { panic as "ERROR: can't find disallowed dir" })
    use <- lazy_guard(when: new_dir == disallowed_dir,
                      return: fn() { snake_get_next_random_dir(dir) })
    new_dir
}

fn snake_get_head_char(dir: SnakeDirection) -> String {
    let head_char = dir_to_int(dir)
                    |> list_item_at(snake_head_chars, _)
    case head_char {
        Some(char) -> char
        None       -> panic as "ERROR: Invalid snake direction"
    }
}

fn snake_move_and_draw(dir: SnakeDirection) {
    snake_get_head_char(dir) |> print()
    move_cursor(wnd_width - 1, wnd_height - 1)
}

fn loop(cur_dir: SnakeDirection, tick: Int) {
    move_cursor(wnd_width / 2, wnd_height / 2)
    snake_move_and_draw(cur_dir)
    sleep(500)
    let new_dir = case is_even(tick) {
        True  -> snake_get_next_random_dir(cur_dir)
        False -> cur_dir
    }
    loop(new_dir, tick + 1)
}

pub fn main() {
    clear_screen()
    draw_borders()
    loop(Up, 1)
}
