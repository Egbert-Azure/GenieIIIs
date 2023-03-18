(*
 * block
 * print out block letters
 *)

program block(input,output);

const
   length       =  6;     (* length of a letter *)
   length1      =  7;

var
   ch   : char;           (* character to enlarge *)

(*
 * line
 * draw a line
 *)

procedure line;

var
   index : 1..length;

begin
   for index := 1 to length do
      write(ch);
   writeln
end;  (* line *)

(*
 * midline
 * print out a line half way across
 * the screen
 *)

procedure midline;

var
   i : 1..length;

begin
   for i := 1 to length div 2 do
      write(' ');
   writeln(ch)
end;  (* midline *)

(*
 * backdiag
 * draw a backward diagonal
 *)

procedure backdiag;

var
   j,i : 0..length;

begin
   for j := 1 to length do
   begin
      for i := 1 to length -j do
         write(' ');
      writeln(ch)
   end
end;  (* backdiag *)

(*
 * side
 * draw a single side
 *)

procedure side;

begin
   writeln(ch)
end; (* side *)

(*
 * farside
 * draw a line on the far side
 *)

procedure farside;

var
   i : 1..length;

begin
   for i := 1 to length -1 do
      write(' ');
   writeln(ch)
end;  (* farside *)

(*
 * twoside
 * draw a '*    *'
 *)

procedure twoside;

var
   index : 1..length;

begin
   write(ch);
   for index := 2 to length -1 do
      write(' ');
   side
end;  (* twoside *)

(*
 * topv
 * draw a V on the top of the letter
 * used for x, y and v
 *)

procedure topv;

var
   i,j : 0..length;
   d : 0..length1;

begin
   for i := 1 to round(length/2) do
   begin
      for j := 1 to i-1 do
         write(' ');
      write(ch);
      if (ch='V') then d := i + 1
      else d := i;
      for j := i to length - d do
         write(' ');
      writeln(ch)
   end
end;  (* topv *)

(*
 * botv
 * draw an upside down v, on  the
 * bottom of the screen (used in x)
 *)

procedure botv;

var
   i,j : 0..length;

begin
   for i := round(length/2) downto 1 do
   begin
      for j := i-1 downto 1 do
         write(' ');
      write(ch);
      for j := length - i downto i do
         write(' ');
      writeln(ch)
   end;
end; (* botv *)

(*
 * what follows here are the special case
 * letters. They must be drawn by themselves.
 *)

(*
 * drawk
 * draw a k
 *)

procedure drawk;

var
   i,j : 0..length;

begin
   for i := 1 to trunc(length / 2) do
   begin
      write(ch);
      for j := i-1 to (length div 2) -1 do
         write(' ');
      writeln(ch)
   end;
   for i := round(length/2) downto 1 do
   begin
      write(ch);
      for j := i-1 to (length div 2) -1 do
         write(' ');
      writeln(ch)
   end
end;  (* drawk *)

(*
 * drawmw
 * draw an m or w
 *)

procedure drawmw(ism : boolean);

var
   i,j : 0..length;
   stop : 0..length;
   by : -1..1;

begin
   if ism then
   begin
      by := 1;
      i := 1;
      stop := length div 2
   end
   else
   begin
      by := -1;
      i := length div 2;
      stop := 0
   end;
   twoside;
   while i<>stop do
   begin
      write(ch);
      for j := 2 to i do
         write(' ');
      write(ch);
      if i<>(length div 2) then
      begin
         j := abs(i + i + 2 - length);
         while j>0 do
         begin
            write(' ');
            j := j - 1
         end;
         write(ch);
         for j := 2 to i do
            write(' ');
      end
      else
         for j := 3 to i do
            write(' ');;
      writeln(ch);
      i := i + by
   end;
   twoside;
end; (* drawmw *)

(*
 * drawn
 * draw an n
 *)

procedure drawn;

var
   i,j : 0..length;

begin
   for i := 1 to length do
   begin
      write(ch);
      for j := 2 to length -1 do
      begin
         if j = i then
            write(ch)
         else
            write(' ')
      end;

      writeln(ch)
   end
end;  (* drawn *)

(*
 * draw
 * case statement which calls the
 * needed routines for each character
 *)

procedure draw;

begin
   writeln;
   if ch in ['A'..'Z'] then
   begin
      case ch of
         'A' : begin
                  line;
                  twoside;
                  twoside;
                  line;
                  twoside;
                  twoside;
                  twoside
               end;
         'B' : begin
                  line;
                  twoside;
                  twoside;
                  line;
                  twoside;
                  twoside;
                  line
               end;
         'C' : begin
                  line;
                  side;
                  side;
                  side;
                  side;
                  side;
                  line
               end;
         'D' : begin
                  line;
                  twoside;
                  twoside;
                  twoside;
                  twoside;
                  twoside;
                  line
               end;
         'E' : begin
                  line;
                  side;
                  side;
                  line;
                  side;
                  side;
                  line
               end;
         'F' : begin
                  line;
                  side;
                  side;
                  line;
                  side;
                  side;
                  side
               end;
         'G' : begin
                  line;
                  side;
                  side;
                  twoside;
                  twoside;
                  twoside;
                  line
               end;
         'H' : begin
                  twoside;
                  twoside;
                  twoside;
                  line;
                  twoside;
                  twoside;
                  twoside
               end;
         'I' : begin
                  line;
                  midline;
                  midline;
                  midline;
                  midline;
                  midline;
                  line
               end;
         'J' : begin
                  farside;
                  farside;
                  farside;
                  farside;
                  farside;
                  farside;
                  line
               end;
         'K' : drawk;
         'L' : begin
                  side;
                  side;
                  side;
                  side;
                  side;
                  side;
                  line;
               end;
         'M' : drawmw(true);
         'N' : drawn;
         'O' : begin
                  line;
                  twoside;
                  twoside;
                  twoside;
                  twoside;
                  twoside;
                  line
               end;
         'P' : begin
                  line;
                  twoside;
                  twoside;
                  line;
                  side;
                  side;
                  side
               end;
         'Q' : begin
                  line;
                  twoside;
                  twoside;
                  twoside;
                  twoside;
                  line;
                  farside
               end;
         'R' : begin
                  line;
                  twoside;
                  twoside;
                  line;
                  twoside;
                  twoside;
                  twoside
               end;
         'S' : begin
                  line;
                  side;
                  side;
                  line;
                  farside;
                  farside;
                  line
               end;
         'T' : begin
                  line;
                  midline;
                  midline;
                  midline;
                  midline;
                  midline;
                  midline
               end;
         'U' : begin
                  twoside;
                  twoside;
                  twoside;
                  twoside;
                  twoside;
                  twoside;
                  line
               end;
         'V' : begin
                  twoside;
                  twoside;
                  twoside;
                  twoside;
                  topv
               end;
         'W' : drawmw(false);
         'X' : begin
                  topv;
                  botv
               end;
         'Y' : begin
                  topv;
                  midline;
                  midline;
                  midline;
                  midline
               end;
         'Z' : begin
                  line;
                  backdiag;
                  line
               end;
      end;
   end;
end; (* draw *)

begin
   writeln;
   while not eoln do
   begin
      readln(ch);
      writeln;
      draw;
      writeln
   end
end.  (* block *)

