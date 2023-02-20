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
This is a version of the Las Vegas gambling game KENO. You are allowed
to choose 8 numbers from 1 to 80. The computer will select 20 numbers
in the same range. You win based on how many of your numbers the
computer also selected.
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
 program keno (input,output);

const
   payoff5          = 10;    (* for 5 spots player gets $10      *)
   payoff6          = 100;   (* for 6 spots player gets $100     *)
   payoff7          = 2200;  (* for 7 spots player gets $2200    *)
   payoff8          = 25000; (* for 8 spots player gets $25000   *)
   totalspots       = 80;    (* total of 80 possible spots       *)
   numcompspots     = 20;    (* computer picks 20 spots          *)
   numplayerspots   = 8;     (* player gets 8 spots              *)
   cost             = 1.20;  (*cost to play a game               *)

type
   spottype = set of 1..totalspots;

var
   compspots   : spottype; (* spots the computer chooses *)
   playerspots : spottype; (* player's spots *)
   money       : real;     (* player has this much money *)
   ch          : char;     (* input char to see if user wishes to conyinue *)

(*
 * getspots
 * get the player's 8 spots from him
 *)

procedure getspots;
var
   i : 0..numplayerspots; (* number of spots player has chosen so far *)
   spot : integer;        (* number just chosen *)
begin
   i := 0;
   playerspots := [];
   while i < numplayerspots do
   begin
      randomize;
      writeln ('Your ',i+1,'. spot (between 1 and 80) ? ');
      readln (spot);
      if (spot < 1) or (spot > totalspots) then
         writeln ('illegal spot number.')
      else
         if spot in playerspots then
            writeln ('You have already chosen spot ',spot)
         else
            begin
               i := i+1;
               playerspots := playerspots + [spot]
            end
   end
end;


(*
 * computer
 * have the computer pick its spots
 *)

procedure computer;
var
   i : 0..numcompspots;   (* number of spots chosen so far *)
   spot : 0..totalspots;

begin
   i := 0;
   compspots := [];
   writeln ('The computer chooses ');
   while i < numcompspots do
   begin
      spot := 1 + (random(totalspots));
      if not (spot in compspots) then
      begin
         compspots := compspots + [spot];
         i := i+1;
      end;
   end;


(*
 * print out the computer's spots in
 * order by going linearly through
 * all numbers
 *)
   i := 0;
   spot := 0;
   while i <numcompspots do
   begin
      spot := spot +1;
      if spot in compspots then
      begin
         write(spot:3);
         i := i+1;
         if i=10 then
            writeln
      end;
   end;
   writeln
end;

(*
 * score
 * find out how much (if anything) the
 * player has won
 *)


procedure score;
var
   i : 0..numplayerspots;      (* number of matches made *)
   spot : 1..totalspots;       (* current spot number *)
begin
   money := money - cost;       (* charge for the card *)
   i := 0;
   for spot := 1 to totalspots do
      if (spot in compspots) and (spot in playerspots) then
         i :=+ i+1;
   if i in [5,6,7,8] then
   begin
      write('You lucky person, you have ',i);
      writeln(' matches');
      write('That means you have made ');
      case i of
         5:
            begin
               writeln(payoff5);
               money := money + payoff5
            end;
         6:
            begin
               writeln(payoff6);
               money := money + payoff6
            end;
         7:
            begin
               writeln(payoff7);
               money := money + payoff7
            end;
         8:
            begin
               writeln(payoff8);
               money := money + payoff8
            end;
      end
   end
   else
      writeln('Sorry, but you only matched ',i);

   writeln;
   if money >= 0 then
      writeln('Your total money is $',money:4:2)
   else
      writeln('So far you have lost $',abs(money):4:2);
   writeln
end;


begin
   money := 0;
   while (ch <> 'n') and (ch <> 'N') do
   begin
      getspots;
      computer;
      score;
      writeln;
      write('Want to play Keno again? ');
      readln(ch)
   end;
   writeln;
   writeln('All right then leave, see if i care!!!');
   if (money >= 0) then
      writeln('You made $',money:5:2)
   else
      writeln('you lost $',abs(money):5:2)
end.
