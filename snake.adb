-- gnatmake -gnatwa snake.adb

with Ada.Text_IO;       use Ada.Text_IO;
with Interfaces.C;      use Interfaces.C;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Snake is

   WND_WIDTH  : constant Positive := 50;
   WND_HEIGHT : constant Positive := 15;

   type Move_Offset is range -1 .. 1;
   type Move_Rule is record
      X, Y : Move_Offset;
   end record;
   type Snake_Direction is (Up, Right, Down, Left);
   Disallowed_Dirs : constant array (Snake_Direction) of Snake_Direction := (Down, Left, Up, Right);
   Moves : constant array (Snake_Direction) of Move_Rule := ((0, -1), (1, 0), (0, 1), (-1, 0));
   Head_Chars : constant array (Snake_Direction) of Character := ('^', '>', 'v', '<');
   type Window_Width_Dim  is mod WND_WIDTH;
   type Window_Height_Dim is mod WND_HEIGHT;

   WND_WIDTH_MIDDLE  : constant Window_Width_Dim  := Window_Width_Dim(WND_WIDTH / 2);
   WND_HEIGHT_MIDDLE : constant Window_Height_Dim := Window_Height_Dim(WND_HEIGHT / 2);


   function "+" (Left : Window_Width_Dim; Right : Move_Offset) return Window_Width_Dim is
      (Window_Width_Dim(Integer(Left) + Integer(Right)));
   function "+" (Left : Window_Height_Dim; Right : Move_Offset) return Window_Height_Dim is
      (Window_Height_Dim(Integer(Left) + Integer(Right)));



   type Point is record
      X : Window_Width_Dim;
      Y : Window_Height_Dim;
   end record;
   type Snake_Body_Index is range 1 .. 8;
   type Snake_Body is array (Snake_Body_Index) of Point;
   type Snake_Type is record
      Figure : Snake_Body;
      Dir    : Snake_Direction := Up;
   end record;

   type Bitwise_Int is mod 2**32;
   subtype CInt is Interfaces.C.Int;
   type Termios is record
      IFlag, OFlag, CFlag, LFlag : CInt;
      CC : String (1 .. 32);
      ISpeed, OSpeed : CInt;
   end record;
   pragma Convention (C, Termios);

   ICANON     : constant Bitwise_Int := Bitwise_Int(16#0002#);
   ECHO       : constant Bitwise_Int := Bitwise_Int(16#0008#);



   protected Exit_Flag is
      procedure Set (Value : Boolean);
      function  Get return Boolean;
   private
      Flag : Boolean := False;
   end Exit_Flag;

   protected body Exit_Flag is
      procedure Set (Value : Boolean) is
      begin
         Flag := Value;
      end;

      function Get return Boolean is (Flag);
   end Exit_Flag;
   


   function Tcgetattr (Fd : Integer; T : access Termios) return Integer
      with Import => True, Convention => C, External_Name => "tcgetattr";
   function Tcsetattr (Fd, OptionalActions : Integer; T : access Termios) return Integer
      with Import => True, Convention => C, External_Name => "tcsetattr";
   function Get_Char return Character
      with Import => True, Convention => C, External_Name => "getchar";



   procedure Clear_Screen is
   begin
      Put (ASCII.ESC & "[2J" & ASCII.ESC & "[H");
   end Clear_Screen;

   procedure Draw_Border is
   begin
      Put_Line ("+" & (WND_WIDTH - 2) * "-" & "+");
      for I in 1 .. WND_HEIGHT - 2 loop
         Put_Line ("|" & (WND_WIDTH - 2) * " " & "|");
      end loop;
      Put_Line ("+" & (WND_WIDTH - 2) * "-" & "+");
   end Draw_Border;

   function Int_To_Str (N : Integer) return String is (Trim (Integer'Image(N), Ada.Strings.Left));

   procedure Move_Cursor (
      X : Window_Width_Dim;
      Y : Window_Height_Dim
   ) is
   begin
      Put (ASCII.ESC & "[" & Int_To_Str(Integer(Y) + 1) & ";" & Int_To_Str(Integer(X) + 1) & "H");
   end Move_Cursor;

   procedure Hide_Cursor is
   begin
      Put (ASCII.ESC & "[?25l");
   end Hide_Cursor;

   procedure Show_Cursor is
   begin
      Put (ASCII.ESC & "[?25h");
   end Show_Cursor;

   procedure Write_Char (
      C : Character;
      X : Window_Width_Dim;
      Y : Window_Height_Dim
   ) is
   begin
      Move_Cursor (X, Y);
      Put (C);
      Hide_Cursor;
   end Write_Char;



   Old_Termios, New_Termios : aliased Termios;
   procedure Setup_Term is
   begin
      if Tcgetattr (0, Old_Termios'Access) /= 0 then
         return;
      end if;

      New_Termios := Old_Termios;
      New_Termios.LFlag := CInt (
         Bitwise_Int(New_Termios.LFlag) and not ICANON
      );
      New_Termios.LFlag := CInt (
        Bitwise_Int(New_Termios.LFlag) and not ECHO
      );

      if Tcsetattr (0, 0, New_Termios'Access) /= 0 then
         return;
      end if;
   end Setup_Term;

   procedure Restore_Term is
   begin
      if Tcsetattr (0, 0, Old_Termios'Access) /= 0 then
         return;
      end if;
   end Restore_Term;



   task Snake_Worker is
      entry Stop;
      entry Set_Dir (Dir : in Snake_Direction);
   end Snake_Worker;

   task body Snake_Worker is
      Running : Boolean := True;
      Failed  : Boolean := False;
      Snake   : Snake_Type;
      Head    : Point renames Snake.Figure(Snake.Figure'First);
      Tail    : Point renames Snake.Figure(Snake.Figure'Last);

      procedure Move_And_Draw_Snake is
         Move : Move_Rule;
      begin
         Write_Char (' ', Tail.X, Tail.Y);
         for I in reverse Snake.Figure'First + 1 .. Snake.Figure'Last loop
            Snake.Figure(I).X := Snake.Figure(I - 1).X;
            Snake.Figure(I).Y := Snake.Figure(I - 1).Y;
            Write_Char ('*', Snake.Figure(I).X, Snake.Figure(I).Y);
         end loop;

         Move := Moves(Snake.Dir);
         Head.X := Head.X + Move.X;
         Head.Y := Head.Y + Move.Y;

         Write_Char (Head_Chars(Snake.Dir), Head.X, Head.Y);
      end;

      procedure Init_Snake is
      begin
         Head.X := WND_WIDTH_MIDDLE;
         Head.Y := WND_HEIGHT_MIDDLE + Move_Offset(-1);
         for I in Snake.Figure'First + 1 .. Snake.Figure'Last loop
            Snake.Figure(I).X := Head.X;
            Snake.Figure(I).Y := Snake.Figure(I - 1).Y + Move_Offset(1);
         end loop;
      end;

      function Snake_Has_Collisions return Boolean is
      begin
         if (Head.X in Window_Width_Dim'First | Window_Width_Dim'Last) or (Head.Y in Window_Height_Dim'First | Window_Height_Dim'Last) then
            return True;
         end if;
         for I in Snake.Figure'First + 4 .. Snake.Figure'Last loop
            if (Snake.Figure(I).X = Head.X) and (Snake.Figure(I).Y = Head.Y) then
               return True;
            end if;
         end loop;
         return False;
      end;

      procedure Game_Over is
      begin
         Move_Cursor (WND_WIDTH_MIDDLE - Window_Width_Dim(5), WND_HEIGHT_MIDDLE);
         Put_Line ("Game over!");
         Running := False;
         Exit_Flag.Set (True);
      end;


   begin
      Clear_Screen;
      Draw_Border;
      Setup_Term;
      Init_Snake;

      while Running loop
         select
            accept Stop do
               Running := False;
            end Stop;
         or
            accept Set_Dir (Dir : in Snake_Direction) do
               if (Dir /= Disallowed_Dirs(Snake.Dir)) then
                  Snake.Dir := Dir;
               end if;
            end Set_Dir;
         or
            delay 0.01;
         end select;

         Move_And_Draw_Snake;
         Failed := Snake_Has_Collisions;
         if Failed then
            Game_Over;
         end if;
         delay 0.5;

      end loop;
      
      Restore_Term;
      Clear_Screen;
      Show_Cursor;
      if Failed then
         Put_Line ("Press ENTER to exit");
      end if;
   end Snake_Worker;


   Key, Key2 : Character;

begin
   loop
      Key := Get_Char;

      if Key = ASCII.ESC then
         if Get_Char = '[' then
            Key2 := Get_Char;
            case Key2 is
               when 'A' => Snake_Worker.Set_Dir(Up);
               when 'B' => Snake_Worker.Set_Dir(Down);
               when 'C' => Snake_Worker.Set_Dir(Right);
               when 'D' => Snake_Worker.Set_Dir(Left);
               when others => null;
            end case;
         else
            Exit_Flag.Set (True);
         end if;
      end if;

      exit when Exit_Flag.Get;
   end loop;

   begin
      Snake_Worker.Stop;
   exception
      when others => null;
   end;

end Snake;
