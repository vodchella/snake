with Ada.Text_IO;
with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Strings.Fixed;

procedure Snake is
   use Ada.Text_IO;
   use Interfaces.C;
   use Ada.Strings.Fixed;

   type Point is record
      X, Y : Integer;
   end record;

   type Snake_Direction is (Up, Right, Down, Left);
   Disallowed_Dirs : constant array (Snake_Direction) of Snake_Direction := (Down, Left, Up, Right);
   Moves : constant array (Snake_Direction) of Point := ((0, -1), (1, 0), (0, 1), (-1, 0));
   Head_Chars : constant array (Snake_Direction) of Character := ('^', '>', 'v', '<');

   type Snake_Body is array (1 .. 8) of Point;
   type Snake_Type is record
      Figure : Snake_Body;
      Dir : Snake_Direction := Up;
   end record;

   type Bitwise_Int is mod 2**32;
   subtype CInt is Interfaces.C.Int;

   type Termios is record
      IFlag, OFlag, CFlag, LFlag : CInt;
      CC : String (1 .. 32);
      ISpeed, OSpeed : CInt;
   end record;
   pragma Convention (C, Termios);

   protected Exit_Flag is
      procedure Set (Value : Boolean);
      function Get return Boolean;
   private
      Flag : Boolean := False;
   end Exit_Flag;

   protected body Exit_Flag is
      procedure Set (Value : Boolean) is
      begin
         Flag := Value;
      end;

      function Get return Boolean is
      begin
	 return Flag;
      end;
   end Exit_Flag;
   
   SCR_WIDTH  : constant Natural := 50;
   SCR_HEIGHT : constant Natural := 15;
   ICANON     : constant Bitwise_Int := Bitwise_Int(16#0002#);
   ECHO       : constant Bitwise_Int := Bitwise_Int(16#0008#);




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
      Put_Line ("+" & (SCR_WIDTH - 2) * "-" & "+");
      for I in 1 .. SCR_HEIGHT - 2 loop
         Put_Line ("|" & (SCR_WIDTH - 2) * " " & "|");
      end loop;
      Put_Line ("+" & (SCR_WIDTH - 2) * "-" & "+");
   end Draw_Border;

   function Trim_Integer_Image (N : Integer) return String is
      Str : constant String := Integer'Image(N);
   begin
      return Trim (Str, Ada.Strings.Left);
   end Trim_Integer_Image;

   procedure Move_Cursor (X, Y : Integer) is
   begin
      Put (ASCII.ESC & "[" & Trim_Integer_Image(Y + 1) & ";" & Trim_Integer_Image(X + 1) & "H");
   end Move_Cursor;

   procedure Hide_Cursor is
   begin
      Put (ASCII.ESC & "[?25l");
   end Hide_Cursor;

   procedure Show_Cursor is
   begin
      Put (ASCII.ESC & "[?25h");
   end Show_Cursor;

   procedure Write_Char (C : Character; X, Y : Natural) is
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
      Failed : Boolean := False;
      Snake : Snake_Type;
      Head : Point renames Snake.Figure(Snake.Figure'First);
      Tail : Point renames Snake.Figure(Snake.Figure'Last);

      procedure Move_And_Draw_Snake is
         Move : Point;
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
         Head.X := Integer(SCR_WIDTH / 2);
         Head.Y := Integer(SCR_HEIGHT / 2) - 1;
         Write_Char (Head_Chars(Snake.Dir), Head.X, Head.Y);
         for I in Snake.Figure'First + 1 .. Snake.Figure'Last loop
	    Snake.Figure(I).X := Head.X;
	    Snake.Figure(I).Y := Snake.Figure(I - 1).Y + 1;
         end loop;
      end;

      function Snake_Has_Collisions return Boolean is
      begin
	 if (Head.X in 0 | (SCR_WIDTH - 1)) or (Head.Y in 0 | (SCR_HEIGHT - 1)) then
	    return True;
	 end if;
         for I in Snake.Figure'First + 1 .. Snake.Figure'Last loop
	    if (Snake.Figure(I).X = Head.X) and (Snake.Figure(I).Y = Head.Y) then
               return True;
	    end if;
         end loop;
	 return False;
      end;

      procedure Game_Over is
      begin
	 Move_Cursor (Integer(SCR_WIDTH / 2) - 5, Integer(SCR_HEIGHT / 2));
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
         -- else
            -- Exit_Flag.Set (True);
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
