(*
 * blackbox
 * try to determine the location of n balls in a black box
 *)

program blackbox(input, output);

const
   side     =  8;     (* there are 8 squares to a side *)
   siden1   =  9;     (* the length of one side plus one *)
   maxstart = 32;     (* maximum possible location for ray *)
   absorbed =  0;     (* what track returns when ray absorbed *)
   boxsize  = 64;     (* size of the box *)

type
   course   = (up,down,left,right);   (* possible directions for rays *)
   lines    = array[0..siden1] of boolean;  (* possible locations in the box *)
   location = 0..boxsize;                   (* 0 is for absorption *)

var
   box       : array[0..siden1] of lines;  (* the black box *)
   direction : course;                     (* the ray's current direction *)
   startray  : 1..maxstart;                (* the ray's starting location *)
   numballs  : integer;                    (* the number of balls *)

(*
 * track
 * recursive function that follows a ray's course through the box
 *)

function track(place:location):location;

var
   rownum : 1..side;
   colnum : 1..side;

(*
 * onside
 * tests to see if there is a ball on either side of a ray
 *)

function onside:boolean;

begin
   if (direction = right) or (direction = left) then
      onside := box[rownum-1][colnum] or box[rownum+1][colnum]
   else
      onside := box[rownum][colnum-1] or box[rownum][colnum+1]
end;

(*
 * edge
 * tests if a ray has reached the end of the box
 *)

function edge:boolean;

begin
   case direction of
      left : edge := ( (place - 1) mod side ) = 0;
      right: edge := ( (place - 1) mod side ) = side - 1;
      up   : edge := ( (place - 1) div side ) = 0;
      down : edge := ( (place - 1) div side ) = side - 1
   end
end;

(*
 * diagup
 * tells me if there is a ball on the upward diagonal
 *)

function diagup:boolean;

begin
   case direction of
      left : diagup := box[rownum-1][colnum-1];
      right: diagup := box[rownum-1][colnum+1];
      up   : diagup := box[rownum-1][colnum-1];
      down : diagup := box[rownum+1][colnum-1]
   end
end;

(*
 * diagdown
 * tells me if there is a ball on the diagonal downward
 *)

function diagdown:boolean;

begin
   case direction of
      left : diagdown := box[rownum+1][colnum-1];
      right: diagdown := box[rownum+1][colnum+1];
      up   : diagdown := box[rownum-1][colnum+1];
      down : diagdown := box[rownum+1][colnum+1]
   end
end;

(*
 * change
 * moves the ray one square in the current direction
 *)

procedure change(var num:location);

begin
   (* if we are on an edge, then don't move *)
   if not edge then
      case direction of
         left : num := num - 1;
         right: num := num + 1;
         up   : num := num - side;
         down : num := num + side
      end
end;

(*
 * track
 *)

begin
   rownum := (place - 1) div side + 1;
   colnum := (place - 1) mod side + 1;

   if box[rownum][colnum] then
      track := absorbed
   else
   begin
      if onside then
      begin
         case direction of
            left : direction := right;
            right: direction := left;
            up   : direction := down;
            down : direction := up
         end;
         track := place
      end
      else
      begin
         if diagup and diagdown then
         begin
            case direction of
               left : direction := right;
               right: direction := left;
               up   : direction := down;
               down : direction := up
            end;
            change(place);
            track := track(place)
         end
         else
         begin
            if diagup then
            begin
               case direction of
                  left, right : direction := down;
                  up,down     : direction := right
               end;
               change(place);
               track := track(place)
            end
            else
            if diagdown then
            begin
               case direction of
                  left,right : direction := up;
                  up,down    : direction := left
               end;
               change(place);
               track := track(place)
            end
            else
            if not edge then
            begin
               change(place);
               track := track(place)
            end
            else
               track := place
         end
      end
   end
end;

(*
 * clearballs
 * removes all the balls from the box
 *)

procedure clearballs;

var
   i,j : 0..siden1;

begin
   for i := 0 to siden1 do
      for j := 0 to siden1 do
         box[i][j] := false
end;

(*
 * placeballs
 * asks the user for the number of balls to place in the box, then
 * randomly places the balls in the box
 *)

procedure placeballs;

var
   i,j    : 0..siden1;
   placed : integer;

begin
   randomize;
   clearballs;

   writeln;
   write('How many balls? ');
   readln(numballs);
   randomize;
   placed := 0;
   while placed < numballs do
   begin
      i := 1 + random(side);
      j := 1 + random(side);
      if not box[i][j] then
      begin
         box[i][j] := true;
         placed := placed + 1
      end
   end
end;

(*
 * getray
 * sits in a loop getting numbers from the user, and using them to determine
 * where to fire a ray. if the number is 0, then the user is readu to guess
 * where the balls are located.
 *)

procedure getray;

var
   raynum : integer;
   outat  : location;

(*
 * raytobox
 * converts a user number into the equivalent location in the box
 *)

procedure raytobox(var num : integer);

begin
   if num <> 0 then
   begin
      if num <= side then
      begin
         direction := right;
         num := (num-1)*side + 1
      end
      else
      if num <= (side + side) then
      begin
         direction := up;
         num := sqr(side) - ((2*side)-num)
      end
      else
      if num <= (3 * side) then
      begin
         direction := left;
         num := (3 * side - num + 1) * side
      end
      else
      begin
         direction := down;
         num := (4 * side - num + 1)
      end
   end
end;

(*
 * boxtoray
 * returns the ray location of an edge
 *)

function boxtoray(place:location):location;

begin
   case direction of
      right : boxtoray := place div side + 17;
      left  : boxtoray := (place + (side-1)) div side;
      up    : boxtoray := 33 - place;
      down  : boxtoray := place - 48
   end
end;

begin
   repeat
      write('Where should I shoot now? ');
      readln(raynum);
      while not (raynum in [0..maxstart]) do
      begin
         writeln('Sorry, but only numbers between 0 and ',maxstart);
         write('Start where? ');
         readln(raynum)
      end;
      if raynum <> 0 then
      begin
         write('in at ',raynum:2,' ');
         outat := 0;
         raytobox(raynum);
         outat := track(raynum);
         if outat = 0 then
            writeln('Absorbed')
         else
            writeln('It came out at ',boxtoray(outat))
      end
   until raynum = 0
end;

(*
 * printballs
 * prints out the balls
 *)

procedure printballs;

var

   i,j : 1..side;

begin
   writeln('  32 31 30 29 28 27 26 25');
   writeln('  -------------------------');
   for i := 1 to side do
   begin
      write(i:2,'(');
      for j := 1 to side do
         if box[i][j] then
            write('*  ')
         else
            write('.  ');
      writeln(')',25-i)
   end;
   writeln('  -------------------------');
   writeln('  9  10 11 12 13 14 15 16')
end;

begin
   clearballs;
   printballs;
   placeballs;
   getray;
   printballs;

end.




