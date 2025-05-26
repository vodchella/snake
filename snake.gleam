// gleam.toml:
// 
//     [dependencies]
//     gleam_stdlib = ">= 0.44.0 and < 2.0.0"
//     gleam_erlang = ">= 0.34.0 and < 1.0.0"
//     prng = ">= 4.0.1 and < 5.0.0"
//
// Run it with: gleam run

import gleam/erlang/process
import gleam/io
import gleam/list
import gleam/option.{type Option, Some, None, unwrap}
import prng/random.{type Generator}
import prng/seed


type SnakeDirection { Up Right Down Left }
const snake_head_chars = ["^", ">", "v", "<"]
const disallowed_dirs  = [Down, Left, Up, Right]


fn is_even(n: Int) -> Bool {
    n % 2 == 0
}

fn list_item_at(lst: List(t), i: Int) -> Option(t) {
    case list.drop(lst, i) {
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
    let gen: Generator(Int) = random.int(0, 3)
    let #(rand_index, _) = gen |> random.step(seed.random())
    let possible_dir = int_to_dir(rand_index)
    let dir_index = dir_to_int(dir)
    let disallowed_dir = unwrap(list_item_at(disallowed_dirs, dir_index), dir)
    case possible_dir {
        d if d == disallowed_dir -> get_next_random_dir(dir)
        _ -> possible_dir
    }
}

fn loop(cur_dir: SnakeDirection, tick: Int) {
    io.println("Head: " <> get_snake_head_char(cur_dir))
    process.sleep(500)
    let new_dir = case is_even(tick) {
        True  -> get_next_random_dir(cur_dir)
        False -> cur_dir
    }
    loop(new_dir, tick + 1)
}

pub fn main() {
    loop(Up, 1)
}
