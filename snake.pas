{$mode objfpc}
{$COPERATORS ON}


uses {$ifdef unix}cthreads,{$endif}crt, sysutils;


const
  SNAKE_LENGTH = 8;
  SNAKE_DIR_UP = 0;
  SNAKE_DIR_RIGHT = 1;
  SNAKE_DIR_DOWN = 2;
  SNAKE_DIR_LEFT = 3;
  DIRS_CHANGING: packed array[SNAKE_DIR_UP..SNAKE_DIR_LEFT] of array[0..1] of byte = (
  	                (SNAKE_DIR_RIGHT, SNAKE_DIR_LEFT),
  	                (SNAKE_DIR_UP, SNAKE_DIR_DOWN),
  	                (SNAKE_DIR_RIGHT, SNAKE_DIR_LEFT),
  	                (SNAKE_DIR_UP, SNAKE_DIR_DOWN)
                 );


type 
  TWindow = packed record
	  width,
	  height:  byte;
  end;
  TPoint = packed record
    x, y:  byte;
  end;
  TSnake = packed record
    body: packed array [0..SNAKE_LENGTH-1] of TPoint;
    dir: byte;
  end;
  PSnake = ^TSnake;
  TThreadData = packed record
    snake: PSnake;
    terminated: boolean;
  end;
  PThreadData = ^TThreadData;


procedure windowDraw(var w: TWindow);
var
  x, y: byte;
begin
  for x := 2 to w.width - 1 do
    begin
      GotoXY(x, 1);
      Write('_');
      GotoXY(x, w.height - 1);
      Write('_');
    end;
  for y := 2 to w.height - 1 do
    begin
      GotoXY(1, y);
      Write('|');
      GotoXY(w.width, y);
      Write('|');
    end;
end;


procedure curSetPos(var p: TPoint);
begin
  GotoXY(p.x, p.y);
end;


procedure snakeInit(var wnd: TWindow; var snake: TSnake);
var
  snake_x, snake_y: byte;
  i: byte;
begin
  snake_x := trunc(wnd.width / 2);
  snake_y := trunc(wnd.height / 2);
  for i := 0 to SNAKE_LENGTH - 1 do
    begin
      snake.body[i].x := snake_x;
      snake.body[i].y := snake_y;
      inc(snake_y);
    end;
  snake.dir := SNAKE_DIR_UP;
end;


procedure snakeDraw(var snake: TSnake);
var
  i: byte;
  ch: char;
begin
  for i := 0 to SNAKE_LENGTH - 1 do
    begin
      curSetPos(snake.body[i]);
      if i = 0 then
        ch := '@'
      else
        ch := '*';
      Write(ch);
    end;
  GotoXY(1, 1);
end;


procedure snakeErase(var wnd: TWindow; var snake: TSnake);
var
  i: byte;
begin
  for i := 0 to SNAKE_LENGTH - 1 do
    begin
      curSetPos(snake.body[i]);
      if snake.body[i].y = wnd.height - 1 then
        Write('_')
      else
        Write(' ');
    end;
  GotoXY(1, 1);
end;


function snakeChangeDir(var snake: TSnake; new_dir: byte): boolean;
begin
  result := false;
  if snake.dir <> new_dir then
    if (new_dir = DIRS_CHANGING[snake.dir][0]) or (new_dir = DIRS_CHANGING[snake.dir][1]) then
      begin
        result := true;
        snake.dir := new_dir;
      end;
end;


procedure snakeMove(var snake: TSnake);
var
  i: byte;
begin
  for i := SNAKE_LENGTH - 1 downto 1 do
    snake.body[i] := snake.body[i - 1];
  case snake.dir of
    SNAKE_DIR_UP:    snake.body[0].y -= 1;
    SNAKE_DIR_RIGHT: snake.body[0].x += 1;
    SNAKE_DIR_DOWN:  snake.body[0].y += 1;
    SNAKE_DIR_LEFT:  snake.body[0].x -= 1;
  end;
end;


function snakeHasCollisions(var wnd: TWindow; var snake: TSnake): boolean;
var
  i: byte;
begin
  result := false;
  if (snake.body[0].x = 1) or (snake.body[0].x = wnd.width) or
     (snake.body[0].y = 1) or (snake.body[0].y = wnd.height) then
    exit(true);
  for i := 2 to SNAKE_LENGTH - 1 do
    if (snake.body[i].x = snake.body[0].x) and (snake.body[i].y = snake.body[0].y) then
      exit(true);
end;


function keyboardHandler(p: pointer): ptrint;
var
  ch:  char;
  dir: smallint;
  td:  PThreadData;
begin
  td := PThreadData(p);
  repeat
    ch := ReadKey();
    case ch of
     #0 : begin
			dir := -1;
            ch := ReadKey();
            case ch of
              #72 : dir := SNAKE_DIR_UP;
              #75 : dir := SNAKE_DIR_LEFT;
              #77 : dir := SNAKE_DIR_RIGHT;
              #80 : dir := SNAKE_DIR_DOWN;
            end;
            if dir <> -1 then
              snakeChangeDir(td^.snake^, dir);
          end;
    end;
  until ch = #27;
  td^.terminated := true;
  result := 0;
end;


procedure gameOver(var wnd: TWindow; var snake: TSnake);
begin
  TextColor(Red);
  curSetPos(snake.body[0]);
  Write('@');
  GotoXY(trunc(wnd.width / 2) - 5, trunc(wnd.height / 2));
  TextBackground(Red);
  TextColor(White);
  Write('Game over!');
  GotoXY(1, 1);
end;


var
  wnd: TWindow;
  snake: TSnake;
  td: TThreadData;
  tattr: byte;
begin
  tattr := TextAttr;
  ClrScr();
  CursorOff();

  wnd.width  := 50;
  wnd.height := 22;
  windowDraw(wnd);
  
  snakeInit(wnd, snake);
  snakeDraw(snake);
  
  td.snake := @snake;
  td.terminated := false;
  BeginThread(@keyboardHandler, @td);

  while not td.terminated do
    begin
      Delay(500);
      snakeErase(wnd, snake);
      snakeMove(snake);
      snakeDraw(snake);
      if snakeHasCollisions(wnd, snake) then
        begin
          gameOver(wnd, snake);
          repeat until KeyPressed();
          break;
        end;
    end;

  TextAttr := tattr;
  ClrScr();
end.
