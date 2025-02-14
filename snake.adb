with Ada.Text_IO;
with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Strings.Fixed;

procedure Snake is
   use Ada.Text_IO;
   use Interfaces.C;
   use Ada.Strings.Fixed;

   type Snake_Direction is (Up, Right, Down, Left);
   type Snake_Head is array (Snake_Direction) of Character;
   type Point is record
      X, Y : Natural;
   end record;
   type Snake_Body is array (1 .. 8) of Point;
   type Snake_Type is record
      Figure : Snake_Body;
      Dir : Snake_Direction := Up;
      To_Erase : Point;
   end record;

   type Bitwise_Int is mod 2**32;
   subtype CInt is Interfaces.C.Int;

   type Termios is record
      IFlag, OFlag, CFlag, LFlag : CInt;
      CC : String (1 .. 32);
      ISpeed, OSpeed : CInt;
   end record;
   pragma Convention (C, Termios);
   
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
      return Trim(Str, Ada.Strings.Left);
   end Trim_Integer_Image;


   procedure Move_Cursor (X, Y : Integer) is
   begin
      Put (ASCII.ESC & "[" & Trim_Integer_Image(Y + 1) & ";" & Trim_Integer_Image(X + 1) & "H");
   end Move_Cursor;


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

   procedure Hide_Cursor is
   begin
      Put (ASCII.ESC & "[?25l");
   end Hide_Cursor;

   procedure Show_Cursor is
   begin
      Put (ASCII.ESC & "[?25h");
   end Show_Cursor;


   task Snake_Worker is
      entry Stop;
      entry Set_Dir (Dir : in Snake_Direction);
   end Snake_Worker;

   task body Snake_Worker is
      Running : Boolean := True;
      Snake : Snake_Type;
      Head_Chars : Snake_Head := ('^', '>', 'v', '<');
      Head : Point renames Snake.Figure(1);
   begin
      Clear_Screen;
      Draw_Border;
      Setup_Term;

      Head.X := Integer(SCR_WIDTH / 2);
      Head.Y := Integer(SCR_HEIGHT / 2);

      while Running loop
	 select
	    accept Stop do
	       Running := False;
	    end Stop;
	 or
            accept Set_Dir (Dir : in Snake_Direction) do
               Snake.Dir := Dir;
	    end Set_Dir;
	 or
            delay 0.1;
         end select;

         Move_Cursor (Head.X, Head.Y);
         Put (Head_Chars(Snake.Dir));
	 Hide_Cursor;

      end loop;
      
      Restore_Term;
      Clear_Screen;
      Show_Cursor;
   end Snake_Worker;


   Key, Key2 : Character;
   Exit_Flag : Boolean := False;

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
            Exit_Flag := True;
         end if;
      end if;

      exit when Exit_Flag;
   end loop;

   Snake_Worker.Stop;

end Snake;
