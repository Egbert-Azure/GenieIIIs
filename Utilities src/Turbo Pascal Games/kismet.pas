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
This game is very similar to Yahtzee. Varations are added by the
introduction of color on the dice and an extra roll. The game ends after
six turns by each player.
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
 *  kismet game
 *)

program kismet (input, output);

const
   maxplayers   = 4;           (* maximum number of players *)
   scores       = 15;          (* 15 ways to make points *)
   numdice      = 5;           (* the number of dice in the game *)
   maxplays     = 6;           (* number of plays in a game *)
   diemax       = 6;           (* die values go from 1-6 *)

type
   diecolor     = (red,green,black,white);  (*possible colors *)
   dievalue     = 1..diemax;   (* possible values a die can have *)
   dieindex     = 1..numdice;  (* index into dice array *)

   die          = record       (* structure for each die *)
      color  : diecolor;       (* its color *)
      value  : dievalue        (* its value *)
   end;

   tabletype    = 1..scores;   (* possible ways to score *)
   scoretype    = array[tabletype] of integer; (* used for scoring *)

var
   dice : array[dieindex] of die;    (* the dice *)
   score : array[1..maxplayers] of scoretype; (* keep track of scores *)
   numplayers : integer;       (* number of players in the game *)
   curgame : 1..maxplays;      (* current play number *)
   ch : char;                  (* input character *)
   colors : array[diecolor] of string[7]; (* will contain red,...*)
   tables : array[tabletype] of string[41]; (* how scoring is done *)
   j : 1..maxplayers;

(*
 * print
 * prints out a given players points
 *)

procedure print(player:integer);

var
   sum : integer;
   bonus : integer;
   i : tabletype;

begin
   sum := 0;
   for i :=1 to 6 do
   begin
      sum := sum + score[player][i];
      writeln(tables[i],' = ',score[player][i])
   end;
   bonus := 0;
   if sum >= 63 then
      if sum <= 70 then
         bonus := 35
      else
         if sum <= 77 then
            bonus := 55
         else
            bonus := 75;
   writeln;
   writeln('Basic section totals to ',sum);
   if bonus <> 0 then
   begin
      writeln('with bonus added of ',bonus);
      writeln('Grand total for basic section ',sum+bonus)
   end;
   writeln;
   sum := sum + bonus;
   for i := 7 to scores do
   begin
      sum := sum + score[player][i];
      writeln(tables[i],' = ',score[player][i])
   end;
   writeln;
   writeln('Total game value = ',sum)

end;

(*
 * play
 * given a player, it will roll the dice,
 * print out the board, keep score, and do
 * anything else that needs to be done
 *)

procedure play(player:integer);

var
   num    : integer;           (* input number from user *)
   roll   : 1..4;              (* roll we are on *)
   ch     : char;              (* input character *)
   i      : 1..maxplayers;     (* index for printing everyone's board *)
   dienum : dieindex;          (* used in rolling the dice *)

(*
 * rolldie
 * will return a random number, and a random
 * color for a die
 *)

procedure rolldie(var dice : die);

begin
   dice.value := random(diemax) + 1;
   case (1 + random(3)) of
      1: dice.color := red;
      2: dice.color := green;
      3: dice.color := black
   end
end;

(*
 * dietotal
 * returns the total of the sum of all the dice.
 *)

function dietotal:integer;

var
   i: dieindex;
   sum : integer;

begin
   sum := 0;
   for i := 1 to numdice do
      sum := sum + dice[i].value;
   dietotal := sum
end;

(*
 * points
 * gives out points to a player
 * the way he (she) asked. for example, if the variable
 * how = 8 then the player asked for 3-of-a-kind
 *)

procedure points(how:tabletype);

var
   i,j,k   : integer;      (* indices into dice array *)
   return  : boolean;      (* used by subprocedures *)
   sum     : integer;      (* point sum *)

(*
 * ifany
 * scores points if any correct
 * die values are shown
 *)

procedure ifany;

begin
   sum := 0;
   for i := 1 to numdice do
      if dice[i].value = how then
         sum := sum + how
end;

(*
 * pair
 * sees if there are 2 different pairs,
 * with the 2 component of each pair having
 * the same color
 *)

function pair:boolean;

var
   numpairs : 0..3;

begin
   numpairs := 0;
   for i := 1 to numdice -1 do
      for j := i+1 to numdice do
         if (dice[i].value = dice[j].value) and
            (dice[i].color = dice[j].color) then
         begin
            numpairs := numpairs + 1;
            dice[i].color := white;
            dice[j].color := white  (* make sure neither is reused in a test *)
         end;
   pair := numpairs >= 2
end;

(*
 * three
 * returns whether on not there is a three of a kind in the dice
 *)

function three:boolean;

begin
   three := false;

   (*
    * simply roll through all combinations of three
    * and see if any are all equal
    *)

   for i := 1 to (numdice -2) do
      for j := i+1 to (numdice -1) do
         for k := i+2 to numdice do
            if (dice[i].value = dice[j].value) and
               (dice[j].value = dice[k].value) then
               three := true
end;

(*
 * straight
 * returns whether or not there is a straight in the dice
 *)

function straight:boolean;

var
   has : set of 1..6;   (* the dice put in a set *)

begin
   has := [];
   for i := 1 to numdice do
      has := has + [dice[i].value];
   straight := (has = [1,2,3,4,5]) or (has = [2,3,4,5,6])
end;

(*
 * flush
 * whether or not there is a flush
 *)

function flush:boolean;

begin
   flush := true;
   for i := 1 to (numdice - 1) do
      if dice[i].color <> dice[i+1].color then
         flush := false
end;


(*
 * fullhouse
 * if there is a full house in the dice
 *)

function fullhouse:boolean;

   (*
    * ifpair
    * if there is a pair (but not 3 or > of a kind)
    *)

function ifpair:boolean;

var
   tmp : array[1..diemax] of 0..numdice;  (* number of each possibility *)

begin
   (*
    * zero out the array
    *)

   for i := 1 to diemax do
      tmp[i] := 0;

   (*
    * count up the number of each value
    *)

   for i := 1 to numdice do
      tmp[dice[i].value] := tmp[dice[i].value] + 1;

   (*
    * see if any is exactly 2
    *)

   for i := 1 to diemax do
      return := return or (tmp[i] = 2);

   ifpair := return
end;

(*
 * fullhouse
 *)

begin
   fullhouse := three and ifpair
end;

(*
 * 4 of a kind
 *)

function four:boolean;

var
   counter : integer;
   j : 1..diemax;

begin
   return := false;
   for j := 1 to diemax do
   begin
      counter := 0;
      for i := 1 to numdice do
         if (dice[i].value = j) then
            counter := counter + 1;
      if (counter = 4) then
         return := true
   end;
   four := return
end;

(*
 * five
 * if there is five of a kind
 *)

function five:boolean;

begin
   five := true;
   for i := 1 to (numdice - 1) do
      if dice[i].value <> dice[i+1].value then
         five := false
end;

(* points *)

begin
   sum := dietotal;
   return := false;
   case how of
      1,2,3,4,5,6: ifany;

      7: if not pair then sum := 0;

      8: if not three then sum := 0;

      9: if straight then sum := 30 else sum := 0;

      10: if flush then sum := 35 else sum := 0;

      11: if fullhouse then sum := sum + 15 else sum := 0;

      12: if fullhouse and flush then sum := sum + 20 else sum := 0;

      13: if four then sum := sum + 25 else sum := 0;

      14: ;

      15: if five then sum := sum + 50 else sum := 0;

   end;
   score[player][how] := sum;
   writeln(tables[how]);
   writeln('For a total of ',sum);
   writeln
end;

(*
 * printdice
 * prints out the dice in a readable format
 *)

procedure printdice;

var
   i : dieindex;

begin
   writeln('Your dice look like:');
   for i := 1 to numdice do
      writeln('Die #-',i,' ',dice[i].value,' ',colors[dice[i].color]);
   writeln
end;

(*
 * replace
 * will ask for a number (num), then will replace num dice
 *)

procedure replace;

var
   num    : integer;
   used   : set of 1..numdice;
   numrep : 1..numdice;

begin
   used := [];
   repeat
      write('Replace how many dice? ');
      readln(num)
   until (num > 0) and (num <= numdice);

(*
 * cycle through num times replacing one die each time
 *)

   for numrep := num downto 1 do
   begin
      repeat
         repeat
            write('Replace which die? ');
            readln(num)
         until (num > 0) and (num <= numdice)
      until not (num in used);
      used := used + [num];
      rolldie(dice[num])
   end;
   roll := roll + 1
end;

(* play *)

begin
   writeln('Player number ',player);
   for dienum := 1 to numdice do
      rolldie(dice[dienum]);
   roll := 1;
   while roll < 4 do
   begin
      repeat
         writeln;
         printdice;
         writeln('P(rint), E(veryone), S(core), R(eplace) ');
         readln(ch)
      until (ch in ['P','p','E','e','S','s','R','r']);
      case ch of
         'P','p' : print(player);
         'E','e' : for i := 1 to numplayers do
                      print(i);
         'S','s' : roll := 4;
         'R','r' : replace
      end
   end;
   printdice;
   repeat
      repeat
         write('Scoring number? ');
         readln(num)
      until (num > 0) and (num <= scores);
   until score[player][num] = 0;
   points(num)
end;

(*
 * init
 * init strings
 *)

procedure init;

var
   i : diecolor;
   j : tabletype;
   k : 1..maxplayers;

begin
   for k := 1 to numplayers do
      for j := 1 to scores do
         score[k][j] := 0;
   tables[1]  := ' 1 - Aces   1 for each Ace                ';
   tables[2]  := ' 2 - Dueces 2 for each Duece              ';
   tables[3]  := ' 3 - Treys  3 for each Trey               ';
   tables[4]  := ' 4 - Fours  4 for each Four               ';
   tables[5]  := ' 5 - Fives  5 for each Five               ';
   tables[6]  := ' 6 - Sixes  6 for each Six                ';
   tables[7]  := ' 7 - 2 pair same color   Total dice       ';
   tables[8]  := ' 8 - 3 of a kind         Total dice       ';
   tables[9]  := ' 9 - Straight            30 points        ';
   tables[10] := '10 - Flush   same color  35 points        ';
   tables[11] := '11 - Full house          Total dice + 15  ';
   tables[12] := '12 - Full house same color Total dice + 20';
   tables[13] := '13 - 4 of a kind         Total dice + 25  ';
   tables[14] := '14 - Yarborough  free turn total dice     ';
   tables[15] := '15 - Kismet  5 of a kind Total dice + 50  ';
   colors[red] := ' red ';
   colors[green] := ' green ';
   colors[black] := ' black '
end;

(* main program *)

begin
   repeat
      write('How many players? ');
      readln(numplayers)
   until (numplayers > 0) and (numplayers <= maxplayers);
   randomize;
   init;
   for curgame := 1 to maxplays do
      for j := 1 to numplayers do
         play(j);
   (*
    * now that it's all done, print out the results
    *)
   for j := 1 to numplayers do
      print(j)
end.
object is to destroy the attackers.');