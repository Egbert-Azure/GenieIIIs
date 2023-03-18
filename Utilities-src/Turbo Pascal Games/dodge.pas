{TUG PDS CERT 1.0

==========================================================================

                  TUG PUBLIC DOMAIN SOFTWARE CERTIFICATION

The Turbo User Group (TUG) is recognized by Borland International as the
official support organization for Turbo Pascal.  This file has been compiled
and verified by the TUG library staff.  We are reasonably certain that the
information contained in this file is public domain material, but it is also
subject to any restrictions applied by its author.

This diskette contains PROGRAMS and or DATA determined to be in the PUBLIC
DOMAIN, provided as a service of TUG for the use of its members.  The
Turbo User Group will not be liable for any damages, including any lost
profits, lost savings or other incidental or consequential damages arising
out of the use of or inability to use the contents, even if TUG has been
advised of the possibility of such damages, or for any claim by any
other party.

To the best of our knowledge, the routines in this file compile and function
properly in accordance with the information described below.

If you discover an error in this file, we would appreciate it if you would
report it to us.  To report bugs, or to request information on membership
in TUG, please contact us at:

             Turbo User Group
             PO Box 1510
             Poulsbo, Washington USA  98370

--------------------------------------------------------------------------
                       F i l e    I n f o r m a t i o n

* DESCRIPTION
This is a dodge'em type game where you are being pursued by various
meanies and your object is to avoid them or preferrably destroy them.
Version 1.00.
Turbo Pascal 2.0, 3.0.

* ASSOCIATED FILES
None

* CHECKED BY
DM 06/08/86

* KEYWORDS
CP/M-80

==========================================================================
}
(*
 * dodge
 * You are in a 30 x 15 square. There are 6 attackers, one tank,
 * and you. If you hit one of the attackers (or the tank), you
 * are destroyed. If two attackers hit each other, they are randomly
 * put somewhere else. If an attacker hits the tank, then the
 * attacker is destroyed. As an added bonus, there are 15 mines
 * in the square (and the border of the square). If you hit a mine
 * you are destroyed. If an attacker hits a mine, then the attacker
 * is destroyed. Your object is to destroy the 6 attackers.
 *
 * You can move by:
 *     812
 *     703
 *     654
 *
 * Note that 0 leaves you in the same place. A move of 9 will put you
 * in some random place on the square (even on a mine)
 *)

program dodge(input, output);

const
   numattackers = 6;   (* number of attackers *)
   length       = 30;  (* length of playing field *)
   width        = 15;  (* width of playing field *)
   numines      = 15;  (* number of mines *)
   ninemax      = 2;   (* number of times the 9 escape can be used *)

type
   (* what can be at a location *)
   what = (me,attacker,tank,mine,nothing);

   xindex = 0..length;
   yindex = 0..width;

   (* directions something can move *)
   direction = (stay,n,ne,e,se,s,sw,w,nw,nowhere);

   (* the location of an object *)
   loc = record
      xcord : xindex;
      ycord : yindex
   end;

var
   (* the playing field *)
   square : array[1..width,1..length] of what;

   tankloc: loc;
   myloc  : loc;
   atack  : array[1..numattackers] of loc;

   ch     : char;
   left   : 0..numattackers;
   nines  : 0..ninemax;
   saydir : direction;

procedure saywhat;
begin
   write('x = ',myloc.xcord,', y = ',myloc.ycord,', dir = ');
   case saydir of
      n    : writeln('n');
      s    : writeln('s');
      e    : writeln('e');
      w    : writeln('w');
      ne   : writeln('ne');
      nw   : writeln('nw');
      se   : writeln('se');
      sw   : writeln('sw');
      stay : writeln('stay');
      nowhere: writeln('nothing');
   end;
end;


(*
 * makeboard
 * creates the board and place every thing in at random places
 *)

procedure makeboard;

var
   placed : integer;
   xloc   : xindex;
   yloc   : yindex;
   i,j    : integer;

begin
   nines := ninemax;
   for i := 1 to width do
      for j := 1 to length do
         square[i][j] := nothing;
   left := numattackers;

   for i := 1 to length do
   begin
      square[1][i] := mine;
      square[width][i] := mine
   end;

   for i := 1 to width do
   begin
      square[i][1] := mine;
      square[i][length] := mine
   end;

   placed := 0;
   while placed < numines do
   begin
      xloc := 1 + random(length);
      yloc := 1 + random(width);
      if square[yloc][xloc] = nothing then
         begin
            square[yloc][xloc] := mine;
            placed := placed + 1
         end
   end;

   placed := 1;

   while placed <= numattackers do
   begin
      xloc := 1 + random(length);
      yloc := 1 + random(width);
      if square[yloc][xloc] = nothing then
      begin
         square[yloc][xloc] := attacker;
         atack[placed].xcord := xloc;
         atack[placed].ycord := yloc;
         placed := placed + 1
      end
   end;

   while square[yloc][xloc] <> nothing do
   begin
      xloc := 1 + random(length);
      yloc := 1 + random(width)
   end;
   square[yloc][xloc] := tank;
   tankloc.xcord := xloc;
   tankloc.ycord := yloc;

   while square[yloc][xloc] <> nothing do
   begin
      xloc := 1 + random(length);
      yloc := 1 + random(width)
   end;
   square[yloc][xloc] := me;
   myloc.xcord := xloc;
   myloc.ycord := yloc;
end;

(*
 * print
 * print out the field
 *)

procedure print;

var
   i : xindex;
   j : yindex;

begin
   for j := 1 to width do
   begin
      for i := 1 to length do
         case square[j][i] of
            me : write('*');
            tank : write('T');
            attacker : write('$');
            mine : write('X');
            nothing : write(' ')
         end;
      writeln
   end
end;

(*
 * move
 * attempt to move something from curx,cury in the direction
 * indicated. returns what was on the location.
 *)

function move(var curx:xindex;var cury:yindex;where:direction): what;

begin
   case where of
     n   : cury := cury - 1;
     stay: ;
     s   : cury := cury + 1;
     w   : curx := curx - 1;
     e   : curx := curx + 1;
     nw  : begin
              curx := curx - 1;
              cury := cury - 1
           end;
     sw  : begin
              curx := curx - 1;
              cury := cury + 1
           end;
     ne  : begin
              curx := curx + 1;
              cury := cury - 1
           end;
     se  : begin
              curx := curx + 1;
              cury := cury + 1
           end
  end;

  move := square[cury][curx]
end;

(*
 * ask
 * ask the user where he wants to go, then attempt to go there.
 * returns what the player lands on.
 *)

function ask:what;

var
   command:array['0'..'9'] of direction;
   dir    : direction;
   ch     : char;

begin
   (* init the commands *)
   dir := stay;
   for ch := '0' to '9' do
   begin
      command[ch] := dir;
      dir := succ(dir)
   end;

   write('Direction? ');
   readln(ch);
   if not (ch in ['0'..'9']) or ((ch = '9') and (nines = 0)) then
   begin
      writeln('How is that? ');
      ask := ask
   end

   else
   begin
      square[myloc.ycord][myloc.xcord] := nothing;
      (* on a command of '9', relocate yourself randomly *)
      if ch = '9' then
      begin
         nines := nines - 1;
         myloc.ycord := 1 + random(width);
         myloc.xcord := 1 + random(length);
         ch := '0'
      end;
      saydir := command[ch];
      ask := move(myloc.xcord,myloc.ycord,command[ch]);
      square[myloc.ycord][myloc.xcord] := me
   end
end;

(*
 * moveall
 * moves all the attackers around, and the tank *)

procedure moveall;


var
   i   : 1..numattackers;   (* index used to run through all attackers *)

(*
 * which
 * given a loc, it decides which direction to go so as to come closer
 * to you. returns the direction.
 *)

function which(curr : loc):direction;

var
   xdir,
   ydir : direction;

begin
   xdir := stay;
   ydir := stay;

   if myloc.xcord > curr.xcord then
      xdir := e
   else
      if myloc.xcord < curr.xcord then
         xdir := w;

   if myloc.ycord > curr.ycord then
      ydir := s
   else
      if myloc.ycord < curr.ycord then
         ydir := n;

   if (xdir = stay) or (ydir = stay) then
   begin
      if xdir = stay then
         which := ydir
      else
         which := xdir
      end
   else
   begin
      case xdir of
         w: if ydir = n then
               which := nw
            else
               which := sw;
         e: if ydir = n then
               which := ne
            else
               which := se;
      end
   end
end;

(*
 * checkmove
 * checks to see what happens to an attacker when it is moved
 *)

procedure checkmove(i : integer; dir : what);

var
   remember,
   index     : 1..numattackers;

begin
   case dir of
      nothing : square[atack[i].ycord][atack[i].xcord] := attacker;

      tank    : begin
                   writeln('The tank just destroyed an attacker');
                   atack[i].xcord := 0;
                   left := left - 1
                end;
      me      : begin
                   writeln('You just died!!!');
                   myloc.xcord := 0
                end;
      attacker: begin
                   (* find out which attacker we hit *)

                   for index := 1 to numattackers do
                      if (atack[i].xcord = atack[index].xcord) and
                         (atack[i].ycord = atack[index].ycord) and
                         (i <> index) then
                            remember := index;
                   writeln('Two attackers just collided!');

                   (* remove it from the field *)
                   square[atack[remember].ycord][atack[remember].xcord] := nothing;

                   (* choose a random spot for the first attacker *)
                   atack[i].xcord := 1 + random(length);
                   atack[i].ycord := 1 + random(width);

                   (* move the attacker *)
                   checkmove(i,move(atack[i].xcord,atack[i].ycord,which(atack[i])));

                   (* repeat the same with the other attacker *)
                   atack[remember].xcord := 1 + random(length);
                   atack[remember].ycord := 1 + random(width);
                   checkmove(remember,move(atack[remember].xcord,
                             atack[remember].ycord,which(atack[remember])));
                end;
      mine    : begin
                   writeln('An attacker just hit a mine.');
                   atack[i].xcord := 0;
                   left := left - 1
                end
   end;
end;

begin
   (* for each attacker, if he is still alive, move him toward you *)

   for i := 1 to numattackers do
   begin
      if atack[i].xcord <> 0 then
      begin
         square[atack[i].ycord][atack[i].xcord] := nothing;
         checkmove(i,move(atack[i].xcord,atack[i].ycord,which(atack[i])))
      end
   end;

   (* move the tank, note that the tank will destroy anything in its way
    * (including mines) *)

   square[tankloc.ycord][tankloc.xcord] := nothing;

   case move(tankloc.xcord,tankloc.ycord,which(tankloc)) of
      attacker : begin
                    writeln('The tank just destroyed an attacker');
                    left := left - 1;
                    for i := 1 to numattackers do
                       if (atack[i].xcord = tankloc.xcord) and
                          (atack[i].ycord = tankloc.ycord) then
                             atack[i].xcord := 0
                 end;

      mine     : writeln('The tank just destroyed a mine');

      me       : begin
                    writeln('The tank just destroyed you');
                    myloc.xcord := 0
                 end;

      nothing  :

   end;
   square[tankloc.ycord][tankloc.xcord] := tank
end;

(*
 * inst
 * prints out the instructions
 *)

procedure inst;

begin
   writeln('     Dodge');
   writeln(' You are in a 30 x 15 square. There are ',numattackers:1,' attackers,');
   writeln(' one tank, and you. If you hit one of the attackers (or the tank)');
   writeln(' you are destroyed. If two attackers hit each other, they are');
   writeln(' randomly put somewhere else. If an attacker hits the tank, then');
   writeln(' the attacker is destroyed. As an added bonus, there are ',numines);
   writeln(' mines in the square (and the border of the square). If you hit');
   writeln(' a mine, you are destroyed. If an attacker hits a mine, then the');
   writeln(' attacker is destroyed. Your object is to destroy the attackers.');
   writeln;
   writeln(' You can move by:');
   writeln('     812');
   writeln('     703');
   writeln('     654');
   writeln(' Note that a 0 leaves you in the same place. A move of 9 will');
   writeln(' put you in some random place on the square (even on a mine).');
   writeln;
   readln(ch);
end;

(* dodge *)

begin
   write('Want instructions? ');
   readln(ch);
   if (ch = 'y') or (ch = 'Y') then
      inst;


   repeat
      randomize;

      makeboard;
      print;
      while (myloc.xcord <> 0) and (left > 0) do
      begin
         (* test to see if you killed yourself *)
         if ask <> nothing then
            myloc.xcord := 0
         else
         begin
            moveall;
            print
         end;
         writeln;
      end;

      if myloc.xcord = 0 then
         writeln('Well, you got yourself killed!!!')
      else
         writeln('Congratulations, you did it!!!');

      writeln;
      write('Want to try again? ');
      readln(ch)
   until (ch <> 'y') and (ch <> 'Y');

end.




                                                                      