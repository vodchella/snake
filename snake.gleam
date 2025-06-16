// gleam.toml:
// 
//     [dependencies]
//     gleam_stdlib = ">= 0.44.0 and < 2.0.0"
//     gleam_erlang = ">= 1.0.0 and < 2.0.0"
//
// Run it with: gleam run

import gleam/bool.{lazy_guard, negate}
import gleam/erlang/process.{sleep}
import gleam/io.{print, println}
import gleam/int.{to_string, random}
import gleam/list.{append, contains, drop, each, filter, first, flatten, last, length, map, prepend, range, reverse, take}
import gleam/option.{from_result, type Option, Some, None}
import gleam/string.{repeat}


const infinity           = 999_999_999_999
const ascii_esc          = "\u{001b}"
const wnd_width          = 46
const wnd_height         = 15
const food               = "O"
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
const moving_rules       = [
    Point(0, -1),  // Up
    Point(1,  0),  // Right
    Point(0,  1),  // Down
    Point(-1, 0),  // Left
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
        food:  Point,
        walls: List(Point)
    )
}


//
//  Utils
//

// Stolen from https://github.com/ollien/gleave
@external(erlang, "erlang", "halt")
@external(javascript, "node:process", "exit")
pub fn exit(status: Int) -> Nil

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

fn point_get_neighbors(point: Point) -> List(Point) {
    moving_rules
    |> map(fn(r) {
        Point(r.x + point.x, r.y + point.y)
    })
}


//
//  Snake
//

fn snake_rule_to_dir(rule: Point) -> SnakeDirection {
    case rule {
        Point(0, -1) -> Up
        Point(1,  0) -> Right
        Point(0,  1) -> Down
        Point(-1, 0) -> Left
        _ -> panic as "ERROR: invalid rule"
    }
}

fn snake_dir_to_rule(dir: SnakeDirection) -> Point {
    case dir {
        Up    -> Point(0, -1)
        Right -> Point(1,  0)
        Down  -> Point(0,  1)
        Left  -> Point(-1, 0)
    }
}

fn snake_get_head_char(dir: SnakeDirection) -> String {
    case dir {
        Up    -> "^"
        Right -> ">"
        Down  -> "v"
        Left  -> "<"
    }
}

fn snake_choose_and_set_next_dir(game: Game, snake: Snake) -> Snake {
    let dir = snake_get_next_dir(game, snake)
    Snake(..snake, dir:)
}

fn snake_get_next_dir(game: Game, new_snake: Snake) -> SnakeDirection {
    let head = snake_get_head(new_snake)
    let path_to_food = node_find_path(node(head), node(game.food), game)
    case path_to_food {
        [] -> new_snake.dir
        path  -> {
            let assert Ok(next_node) = path
            |> first()

            let rule = case next_node.point {
                Point(x, y) -> Point(x - head.x, y - head.y)
            }
            snake_rule_to_dir(rule)
        }
    }
}

fn snake_init_body() -> List(Point) {
    snake_init_body_worker([], initial_length)
}

fn snake_init_body_worker(body: List(Point), expected_cnt: Int) -> List(Point) {
    use <- lazy_guard(expected_cnt == 0, fn() { reverse(body) })
    case body {
        []          -> [Point(wnd_width / 2 - 6, wnd_height / 2 - 1)]
        [p, ..rest] -> [Point(p.x, p.y + 1), p, ..rest]
    }
    |> snake_init_body_worker(expected_cnt - 1)
}

fn snake_get_head(snake: Snake) -> Point {
    let assert Ok(head) = snake.body
    |> first()
    head
}

fn snake_get_tail(snake: Snake) -> Point {
    let assert Ok(tail) = snake.body
    |> last()
    tail
}

fn snake_draw(snake: Snake) {
    let assert [head, ..rest] = snake.body

    rest
    |> each(fn(p) {
        print_at(body, p.x, p.y)
    })

    snake_get_head_char(snake.dir)
    |> print_at(head.x, head.y)

    cursor_move(wnd_width - 1, wnd_height - 1)
}

fn snake_pre_draw(snake: Snake) {
    let head = snake_get_head(snake)
    let tail = snake_get_tail(snake)
    print_at(body, head.x, head.y)
    print_at(space, tail.x, tail.y)
}

fn snake_get_next_head_pos(snake: Snake) -> Point {
    let rule = snake_dir_to_rule(snake.dir)
    let head = snake_get_head(snake)
    Point(head.x + rule.x, head.y + rule.y)
}

fn snake_grow_up(game: Game, snake: Snake) -> Snake {
    let snake = snake_choose_and_set_next_dir(game, snake)
    let body = [
        snake_get_next_head_pos(snake),
        ..snake.body
    ]
    let snake = Snake(..snake, body:, len: length(body))
    snake_choose_and_set_next_dir(game, snake) // Important !!!
}

fn snake_move(snake: Snake) -> Snake {
    let body = [
        snake_get_next_head_pos(snake),
        ..take(snake.body, snake.len - 1)
    ]
    Snake(..snake, body:)
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

fn food_renew(game: Game) -> Game {
    let head = snake_get_head(game.snake)
    let forbidden_points = point_get_neighbors(head)
    |> map(fn(p) {
        point_get_neighbors(p)
        |> prepend(p)
    })
    |> flatten()
    |> prepend(head)
    |> append(game_get_forbidden_points(game))

    let food = forbidden_points
    |> food_gen_random()

    Game(..game, food:)
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


//
//  Walls
//

fn walls_init() -> List(Point) {
    [
        Point(12, 3), Point(14, 3), Point(32, 7),  Point(35, 6),
        Point(12, 4), Point(14, 4), Point(32, 8),  Point(35, 7),
        Point(12, 5), Point(14, 5), Point(32, 9),  Point(35, 8),
        Point(12, 6), Point(14, 6), Point(32, 10), Point(35, 9),
        Point(12, 7), Point(14, 7), Point(32, 11), Point(35, 10),
        Point(12, 8), Point(14, 8), Point(32, 12), Point(35, 11),
        Point(12, 9), Point(14, 9), Point(32, 13), Point(35, 12),
    ]
}

fn walls_draw(game: Game) {
    game.walls
    |> each(fn(w) {
        print_at(wall, w.x, w.y)
    })
}


//
//  Pathfinding utils
//

fn node(point: Point) -> Node {
    Node(point, 0, None)
}

fn node_get_neighbors(node: Node, game: Game) -> List(Node) {
    let forbidden = game_get_forbidden_points(game)
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
    |> filter(fn(p) {
        forbidden
        |> contains(p)
        |> negate
    })
    |> map(fn(p) { Node(p, infinity, None) })
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
            let forbidden = explored
            |> map(fn(f) { f.point })

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
        _ -> {
            game_over(game)
        }
    }
}

fn node_choose(nodes: List(Node), _dst: Node) -> Option(Node) {
    nodes
    |> first()
    |> from_result()
}

fn node_build_path(node: Node, acc: List(Node)) -> List(Node) {
    case node.prev {
        Some(prev) -> node_build_path(prev, [node, ..acc])
        None       -> acc
    }
}


//
//  Game
//

fn game_over(game: Game) {
    game_over_print()
    food_draw(game)
    cursor_move(1, wnd_height)
    exit(0)  // Never returns!
    []
}

fn game_over_print() {
    print_at("Game over!", {wnd_width / 2} - 5, wnd_height  / 2)
    cursor_move(1, wnd_height)
}

fn game_get_forbidden_points(game: Game) -> List(Point) {
    append(game.snake.body, game.walls)
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
        True  -> game_over_print()
        False -> {
            sleep(20)
            case snake_get_head(snake) {
                head if head == game.food -> {
                    let game = food_renew(game)
                    let snake = snake_grow_up(game, snake)
                    Game(..game, snake:)
                    |> loop()
                }
                _ -> {
                    let snake = snake_choose_and_set_next_dir(game, snake)
                    Game(..game, snake:)
                    |> loop()
                }
            }
        }
    }
}

pub fn main() {
    let walls = walls_init()
    let snake_body = snake_init_body()
    let food = Point(13, 7)
    let snake = Snake(Up, snake_body, length(snake_body))
    let game = Game(snake, food, walls)
    screen_clear()
    borders_draw()
    walls_draw(game)
    loop(game)
}
