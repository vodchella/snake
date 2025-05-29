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
import gleam/int.{is_odd, to_string, random}
import gleam/list.{contains, drop, each, length, range, reverse, take}
import gleam/option.{lazy_unwrap, type Option, Some, None}
import gleam/string.{repeat}


const ascii_esc          = "\u{001b}"
const wnd_width          = 46
const wnd_height         = 15
const food               = "O"
const hwall              = "-"
const vwall              = "|"
const corner             = "+"
const body               = "*"
const space              = " "
const initial_length     = 8
const head_chars         = ["^", ">", "v", "<"]
const disallowed_dirs    = [Down, Left, Up, Right]
const moving_rules       = [
    Point(0, -1),
    Point(1, 0),
    Point(0, 1),
    Point(-1, 0)
]

pub type SnakeDirection { Up Right Down Left }
pub type Point {
    Point(x: Int, y: Int)
}
pub type Snake {
    Snake(
        dir: SnakeDirection,
        body: List(Point),
        len: Int,
    )
}
pub type Game {
    Game(
        snake: Snake,
        tick: Int,
        food: Point,
    )
}


fn clear_screen() {
    print(ascii_esc <> "[2J" <> ascii_esc <> "[H")
}

fn move_cursor(x, y : Int) {
    print(ascii_esc <> "[" <> to_string(y + 1) <> ";" <> to_string(x + 1) <> "H")
}

fn print_at(s: String, x, y: Int) {
    move_cursor(x, y)
    print(s)
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
    let new_dir = length(head_chars) |> random() |> int_to_dir()
    let disallowed_dir = dir_to_int(dir)
                         |> list_item_at(disallowed_dirs, _)
                         |> lazy_unwrap(fn() { panic as "ERROR: can't find disallowed dir" })
    use <- lazy_guard(when: new_dir == disallowed_dir,
                      return: fn() { snake_get_next_random_dir(dir) })
    new_dir
}

fn snake_get_head_char(dir: SnakeDirection) -> String {
    let head_char = dir_to_int(dir)
                    |> list_item_at(head_chars, _)
    case head_char {
        Some(char) -> char
        None       -> panic as "ERROR: Invalid snake direction"
    }
}

fn snake_init_body(body: List(Point), expected_cnt: Int) -> List(Point) {
    use <- lazy_guard(expected_cnt == 0, fn() { reverse(body) })
    case body {
        []          -> [Point(wnd_width / 2, wnd_height / 2 - 1)]
        [p, ..rest] -> [Point(p.x, p.y + 1), p, ..rest]
    }
    |> snake_init_body(expected_cnt - 1)
}

fn snake_get_head(snake: Snake) -> Point {
    list_item_at(snake.body, 0)
    |> lazy_unwrap(fn() { panic as "ERROR: can't find snake head" })
}

fn snake_get_tail(snake: Snake) -> Point {
    list_item_at(snake.body, snake.len - 1)
    |> lazy_unwrap(fn() { panic as "ERROR: can't find snake tail" })
}

fn snake_draw(snake: Snake) {
    case snake.body {
        [head, ..rest] -> {
            rest
            |> each(fn(p) {
                print_at(body, p.x, p.y)
            })
            snake_get_head_char(snake.dir) |> print_at(head.x, head.y)
            move_cursor(wnd_width - 1, wnd_height - 1)
        }
        _ -> panic as "ERROR: snake isn't initialized"
    }
}

fn snake_pre_draw(snake: Snake) {
    let head = snake_get_head(snake)
    let tail = snake_get_tail(snake)
    print_at(body, head.x, head.y)
    print_at(space, tail.x, tail.y)
}

fn snake_move(snake: Snake) {
    let rule = dir_to_int(snake.dir)
               |> list_item_at(moving_rules, _)
    case rule {
        Some(rule) -> {
            let head = snake_get_head(snake)
            let body = [
                Point(head.x + rule.x, head.y + rule.y),
                ..take(snake.body, snake.len - 1)
            ]
            Snake(..snake, body:)
        }
        _ -> panic as "ERROR: can't find moving rule for dir"
    }
}

fn snake_has_collisions(snake: Snake) -> Bool {
    let head = snake_get_head(snake)
    case head {
        Point(_, y) if y <= 0              -> True
        Point(_, y) if y >= wnd_height - 1 -> True
        Point(x, _) if x <= 0              -> True
        Point(x, _) if x >= wnd_width - 1  -> True
        head -> snake.body
                |> drop(1)
                |> contains(head)
    }
}


fn food_gen_random(forbidden_points: List(Point)) -> Point {
    let x = random(wnd_width - 2) + 1
    let y = random(wnd_height - 2) + 1
    let food = Point(x, y)
    case contains(forbidden_points, food) {
        False -> food
        True  -> food_gen_random(forbidden_points)
    }
}

fn food_draw(game: Game) {
    print_at(food, game.food.x, game.food.y)
}


fn loop(game: Game) {
    food_draw(game)
    snake_pre_draw(game.snake)
    let snake = snake_move(game.snake)
    snake_draw(snake)
    case snake_has_collisions(snake) {
        False -> {
            sleep(500)
            let dir = case is_odd(game.tick) {
                True  -> snake_get_next_random_dir(snake.dir)
                False -> snake.dir
            }
            let snake = Snake(..snake, dir:, len: length(snake.body))
            let game = Game(snake, game.tick + 1, game.food)
            loop(game)
        }
        True  -> {
            print_at("Game over!", {wnd_width / 2} - 5, wnd_height  / 2)
            move_cursor(1, wnd_height)
        }
    }
}

pub fn main() {
    clear_screen()
    draw_borders()
    let snake = Snake(Up, snake_init_body([], initial_length), initial_length)
    let game = Game(snake, 1, food_gen_random(snake.body))
    loop(game)
}
