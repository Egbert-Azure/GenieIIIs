program life;

const
   height      = 24;   (* number of lines on screen *)
   width       = 80;   (* number of columns on screen *)
   minbound    = -1;   (* minimum dimension of screen bounds *)
   clearscreen = 26;   (* screen clear character *)

type
   state = (alive, dead);
   cell = record
             lookslikeitis : state;
             nearby : integer;
          end;
   edges = record
              left,
              right,
              top,
              bottom : integer;
           end;

var
   board : array [minbound .. height] of array [minbound .. width] of cell;
   population : integer;
   births     : integer;
   deaths     : integer;
   ch         : char;
   edge       : edges;

(*
 * initializes the edges of the pattern. this starts representing a pattern
 * which has no insides; the top is lower than the bottom, the left side is
 * to the right of the right side. this ensures that the coordinates of the
 * corner of the pattern after it is entered will be correct without needing
 * to scan the entire array after the pattern is entered (a time consuming
 * process).
 *)

procedure resetedges;

begin
   edge.top    := height - 1;
   edge.right  := minbound + 1;
   edge.left   := width - 1;
   edge.bottom := minbound + 1;
end;

procedure instructions;

var
   answer : char;

begin
   writeln('Would you like instructions for LIFE? ');
   readln(answer);
   while not (answer in ['y', 'n', 'Y', 'N']) do
      begin
      writeln('please answer yes or no');
      readln(answer);
      end;
   if (answer = 'y') or (answer = 'Y') then
      begin
      writeln('LIFE simulates the growth of a colony of animals on a 0..',
               height-1:1,' by 0..',width-1:1,' ''world''.');
      writeln('Whether a cell is born, lives, or dies depends on the number');
      writeln('of living animals immediately adjacent to it. If a cell is');
      writeln('empty and has exactly 3 neighbors it will be born in the next');
      writeln('generation. If it is alive and has either two or three');
      writeln('neighbors, it will continue to live. Otherwise it dies of');
      writeln('loneliness or overcrowding.');
      writeln('   The initial pattern is entered by typing the row and then');
      writeln('the column of the desired position. A cell is removed by entering');
      writeln('its position again. To finish entering give a position outside');
      writeln('of the dimensions of the screen. To stop a pattern, just hit');
      writeln('any key. Type return to start.');
      writeln;
      readln(answer);
      end;
      write(chr(clearscreen));
end;

(*
 * initialize
 * resets the board to empty (all dead and with no neighbors)
 *)

procedure initialize;

var
   down, across : integer;

begin
   instructions;
   for down := minbound to height do
      for across := minbound to width do
         begin
         board[down, across].lookslikeitis := dead;
         board[down, across].nearby := 0;
         end;
   resetedges;
end;

(*
 * max ( & min)
 * returns the larger (smaller) of the two integer arguments
 *)

function max(a, b: integer): integer;

begin
   if a >= b then
      max := a
   else
      max := b
end;

function min(a, b: integer): integer;

begin
   if a <= b then
      min := a
   else
      min := b
end;

(*
 * determine if and how the co-ordinates passed as argument change the bounds
 * of the pattern (the position of a box that could contain living cells),
 * checking that it does not go off one of the sides of the board.
 *)

procedure limits(x, y: integer);

begin
   with edge do
      begin
      left   := min(left,x);
      right  := max(right,x);
      top    := min(top,y);
      bottom := max(bottom,y);
      end;
end;

(*
 * this erases the record of the neighbors of all the cells, in preparation
 * for the new calculation of the nearby field
 *)

procedure clearnearby;

var
   down, across : integer;

begin
   for down := edge.top - 1 to edge.bottom + 1 do
      for across := edge.left - 1 to edge.right + 1 do
         board[down,across].nearby := 0;
end;

(*
 * computes the number of adjacent cells, and thus which cells will survive
 * through the next generation. To speed this up, the middle cell of the 3 by 3
 * matrix which is being examioned is included in the count, even though it is
 * not really a neighbor of itself. this off-by-one discrepancy is taken into
 * account in the board update.
 *)

procedure countneighbors;

var
   down, across : integer;
   deltadown, deltaacross : integer;

begin
   clearnearby;
   for down := edge.top - 1 to edge.bottom + 1 do
      for across := edge.left - 1 to edge.right + 1 do
         if board[down,across].lookslikeitis = alive then
            for deltadown := -1 to 1 do
               for deltaacross := -1 to 1 do
                  board[down+deltadown,across+deltaacross].nearby :=
                  board[down+deltadown,across+deltaacross].nearby + 1;
end;

(*
 * update
 * if a birth or death occurs, the screen is updated.
 *)

procedure update;

var
   down, across : integer;
   localedge : edges;

begin
   births := 0;
   deaths := 0;
   localedge := edge;
   resetedges;
   for down := max(minbound+1, localedge.top-1) to
               min(height-1, localedge.bottom+1) do
      for across := max(minbound+1, localedge.left-1) to
                    min(width-1,localedge.right+1) do
         with board[down][across] do
            case lookslikeitis of
               dead:
                   if nearby = 3 then
                      begin
                      lookslikeitis := alive;
                      gotoxy(across,down);
                      write('*');
                      limits(across,down);
                      births := births + 1;
                      end;
                alive:
                   if (nearby = 3) or (nearby = 4) then
                      limits(across,down)
                   else
                      begin
                      lookslikeitis := dead;
                      gotoxy(across,down);
                      write(' ');
                      deaths := deaths + 1;
                      end;
                end;
   population := population + births - deaths;
end;

(*
 * get the starting positions of the cells
 *)

procedure getpositions;

var
   down, across : integer;
   finished : boolean;

(*
 * this is needed to reprint the top line of the pattern, which is destroyed
 * by the prompt line which asks for the cell positions.
 *)

procedure reprinttopline;

var
   across : integer;

begin
   gotoxy(1,1);
   for across := minbound + 1 to width - 1 do
      if board[minbound + 1][across].lookslikeitis = dead then
         write(' ')
      else
         write('*');
end;

begin
   finished := false;
   population := 0;
   gotoxy(1,1);
   write('Position of cell #',population + 1:1,' is : ');
   while not finished do
      begin
      readln(down, across);
      if (down <= minbound) or
         (down >= height) or
         (across <= minbound) or
         (across >= width) then
         finished := true
      else
         with board[down][across] do
            begin
            limits(across, down);
            gotoxy(across, down);
            if lookslikeitis = alive then
               begin
                  write(' ');
                  lookslikeitis := dead;
                  population := population - 1;
                  end
            else
               begin
               write('*');
               lookslikeitis := alive;
               population := population + 1;
               end;
            gotoxy(1,1);
            write('Position of cell #',population + 1:1,' is : ');
            end;
      end;
      reprinttopline;
end;

begin
   initialize;
   getpositions;
   repeat
      countneighbors;
      update;
   until (population = 0) or ((births = 0) and (deaths = 0)) or keypressed;
   gotoxy(1,1);
   if keypressed then
      readln(ch)
   else
      if population = 0 then
         writeln('This colony has died.')
      else
         writeln('The pattern is stable.');
end.


