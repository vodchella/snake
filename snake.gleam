// gleam.toml:
// 
//     [dependencies]
//     gleam_stdlib = ">= 0.44.0 and < 2.0.0"
//     gleam_erlang = ">= 0.34.0 and < 1.0.0"
//
// Run it with: gleam run

import gleam/bool.{lazy_guard, negate}
import gleam/erlang/process.{sleep}
import gleam/io.{print, println}
import gleam/int.{is_odd, to_string, random}
import gleam/list.{append, contains, drop, each, filter, length, map, range, reverse, take}
import gleam/option.{lazy_unwrap, type Option, Some, None}
import gleam/string.{repeat}


const max_cost           = 999_999_999_999
const ascii_esc          = "\u{001b}"
const wnd_width          = 46
const wnd_height         = 15
const food               = "◯"
const wall               = "╳"
const hwall              = "═"
const vwall              = "║"
const tlcorner           = "╔"
const trcorner           = "╗"
const blcorner           = "╚"
const brcorner           = "╝"
const body               = "*"
const space              = " "
const initial_length     = 8
const head_chars         = ["^", ">", "v", "<"]
const disallowed_dirs    = [Down, Left, Up, Right]
const moving_rules       = [
    Point(0, -1),
    Point(1,  0),
    Point(0,  1),
    Point(-1, 0),
]

pub type SnakeDirection { Up Right Down Left }
pub type Point {
    Point(x: Int, y: Int)
}
pub type Node {
    Node(
        point: Point,
        cost:  Int,
        prev:  Option(Node),
    )
}
pub type Snake {
    Snake(
        dir:  SnakeDirection,
        body: List(Point),
        len:  Int,
    )
}
pub type Game {
    Game(
        snake: Snake,
        tick:  Int,
        food:  Point,
        walls: List(Point)
    )
}


//
//  Utils
//

fn screen_clear() {
    print(ascii_esc <> "[2J" <> ascii_esc <> "[H")
}

fn cursor_move(x, y : Int) {
    print(ascii_esc <> "[" <> to_string(y + 1) <> ";" <> to_string(x + 1) <> "H")
}

fn print_at(s: String, x, y: Int) {
    cursor_move(x, y)
    print(s)
}

fn borders_draw() {
    let cnt = wnd_width - 2
    let top_row = tlcorner <> repeat(hwall, cnt) <> trcorner
    let bottom_row = blcorner <> repeat(hwall, cnt) <> brcorner
    let middle_row = vwall <> repeat(space, cnt) <> vwall

    println(top_row)

    range(1, wnd_height - 2)
    |> each(fn(_) { println(middle_row) })

    println(bottom_row)
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


//
//  Snake
//

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
            cursor_move(wnd_width - 1, wnd_height - 1)
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

fn snake_has_collisions(game: Game) -> Bool {
    let head = snake_get_head(game.snake)
    case head {
        Point(_, y) if y <= 0              -> True
        Point(_, y) if y >= wnd_height - 1 -> True
        Point(x, _) if x <= 0              -> True
        Point(x, _) if x >= wnd_width - 1  -> True
        head -> game.snake.body
                |> drop(1)
                |> append(game.walls)
                |> contains(head)
    }
}


//
//  Food
//

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


//
//  Walls
//

fn walls_init() -> List(Point) {
    [
        // Static for now
        Point(12, 3), Point(35, 7),
        Point(12, 4), Point(35, 8),
        Point(12, 5), Point(35, 9),
        Point(12, 6), Point(35, 10),
        Point(12, 7), Point(35, 11),
        Point(12, 8), Point(35, 12),
        Point(12, 9), Point(35, 13),

    ]
}

fn walls_draw(game: Game) {
    game.walls
    |> each(fn(w) {
        print_at(wall, w.x - 1, w.y - 1)
    })
}


//
//  Pathfinding utils
//

fn node_get_neighbors(node: Node, game: Game) {
    let forbidden = append(game.snake.body, game.walls)
    moving_rules
    |> map(fn(r) { Point(node.point.x + r.x, node.point.y + r.y) })
    |> filter(fn(p) {
        case p.x, p.y {
            x, _ if x < 1              -> False
            x, _ if x > wnd_width - 2  -> False
            _, y if y < 1              -> False
            _, y if y > wnd_height - 2 -> False
            _, _ -> True
        }
    })
    |> filter(fn(p) { forbidden |> contains(p) |> negate })
    |> map(fn(p) { Node(p, max_cost, None) })
}

fn node_debug_draw(nodes: List(Node)) {
    nodes |> each(fn(n) {
        print_at(".", n.point.x, n.point.y)
    })
}

fn node_find_path(src: Node, dst: Node, game: Game) -> List(Node) {
    let reachable = [src]
    let explored = []
    node_find_path_worker(src, dst, game, reachable, explored)
}

fn node_find_path_worker(
    src: Node,
    dst: Node,
    game: Game,
    reachable: List(Node),
    explored: List(Node),
) -> List(Node) {
    let node = node_choose(reachable, dst)
    case node {
        Some(node) if node.point == dst.point -> node_build_path(node, [])
        Some(node) -> {
            let explored = [node, ..explored]
            let forbidden = explored |> map(fn(f) { f.point })
            reachable
            |> append(node_get_neighbors(node, game))
            |> filter(fn(rn) {
                forbidden
                |> contains(rn.point)
                |> negate
            })
            |> map(fn(rn) {
                let new_cost = node.cost + 1
                case rn.cost {
                    cost if cost > new_cost -> Node(rn.point, new_cost, Some(node))
                    _ -> rn
                }
            })
            |> node_find_path_worker(src, dst, game, _, explored)
        }
        _ -> []  // Can't find the path
    }
}

fn node_choose(nodes: List(Node), _dst: Node) -> Option(Node) {
    list_item_at(nodes, 0)   // TODO: A* logic here
}

fn node_build_path(node: Node, acc: List(Node)) -> List(Node) {
    case node.prev {
        Some(prev) -> node_build_path(prev, [node, ..acc])
        None       -> acc
    }
}



//
//  Main loop
//

fn loop(game: Game) {
    food_draw(game)
    snake_pre_draw(game.snake)
    let snake = snake_move(game.snake)
    snake_draw(snake)
    case snake_has_collisions(game) {
        False -> {
            sleep(500)
            let dir = case is_odd(game.tick) {
                True  -> snake_get_next_random_dir(snake.dir)
                False -> snake.dir
            }
            let snake = Snake(..snake, dir:, len: length(snake.body))
            let game = Game(snake, game.tick + 1, game.food, game.walls)
            loop(game)
        }
        True  -> {
            print_at("Game over!", {wnd_width / 2} - 5, wnd_height  / 2)
            cursor_move(1, wnd_height)
        }
    }
}

pub fn main() {
    let walls = walls_init()
    let snake_body = snake_init_body([], initial_length)
    let food = append(snake_body, walls) |> food_gen_random()
    let snake = Snake(Up, snake_body, length(snake_body))
    let game = Game(snake, 1, food, walls)
    screen_clear()
    borders_draw()
    walls_draw(game)
    // loop(game)

    //let head = Point(1, 13)
    //let head = Point(9, 7)  // Not optimal
    //let head = Point(29, 10)  // Not optimal
    let head = Point(39, 10)
    let target = Point(14, 7)
    let nodes = node_find_path(Node(head, 0, None), Node(target, 0, None), game)
    node_debug_draw(nodes)
    print_at("^", head.x, head.y)
    print_at("X", target.x, target.y)
    cursor_move(wnd_width, wnd_height)
}
