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
This is a rather complicated horse race game. A series of twelve horses
start the race. Each horse has a different handicap which is a distance
to run. Two dice are rolled and the horse represented by the total of
the dice may be moved one length, or the horses represented by each die
may be moved one length.
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
program horserace (input, output);

const
   maxplayer = 10;
   maxhorses = 12;  (* each player has 12 horses *)

type
   horse = record
      horseloc : 0..10;
      togo     : 0..10;
   end;

var
   track : array[1..maxplayer, 1..maxhorses] of horse;
   numplayers : integer;
   curplayer  : 1..maxplayer;
   win        : boolean;
   i,j        : integer;

procedure getnumplayers;

begin
   write('How many players? ');
   readln(numplayers);
   while (numplayers <= 0) or (numplayers > maxplayer) do
   begin
      writeln('Sorry, only numbers between 1 and ',maxplayer,' are allowed.');
      write('How many players? ');
      readln(numplayers)
   end
end;

(*
 * maketracks
 * initializes track.togo to the correct
 * handicaps, and track.horseloc to 0 to start
 * the game.
 *)

procedure maketracks;

begin
   for i := 1 to numplayers do
   begin
      track[i,1].togo := 4;
      track[i,2].togo := 6;
      track[i,3].togo := 6;
      track[i,4].togo := 8;
      track[i,5].togo := 8;
      track[i,6].togo := 10;
      track[i,7].togo := 6;
      track[i,8].togo := 6;
      track[i,9].togo := 4;
      track[i,10].togo := 4;
      track[i,11].togo := 2;
      track[i,12].togo := 2;
      for j := 1 to maxhorses do
         track[i,j].horseloc := 0
   end
end;

procedure play (player : integer);

const
   diemax = 6;

var
   ch : char;
   horsenum : integer;
   poshorses : set of 1..12;
   die1, die2 : 1..diemax;

(*
 * throwdie
 * uses the random function to throw the dice.
 *)

function throwdie: integer;

begin
   throwdie := 1 + random(diemax)
end;

(*
 * print.
 * displays the tracks.
 *)

procedure print;

var
   pnum : 1..maxplayer;
   curhorse : 1..maxhorses;
   length : 1..10;
   sum : 1..10;

begin
   for pnum := 1 to numplayers do
   begin
      writeln('Player number ',pnum);
      for curhorse := 1 to maxhorses do
      begin
         writeln;
         write(curhorse:2,' - ');
         sum := track[pnum,curhorse].togo + track[pnum,curhorse].horseloc;
         for length := 1 to sum do
            if length = track[pnum,curhorse].horseloc then
               write(' (*)')
            else
               write(' ( )')
      end;
      writeln;
      writeln
   end;
   writeln
end;  (* print *)

(*
 * movehorse.
 * increments horseloc and decrements togo
 * to move the horse.
 *)

procedure movehorse (player,horsenum : integer);

begin
   track[player,horsenum].togo := track[player,horsenum].togo - 1;
   track[player,horsenum].horseloc := track[player,horsenum].horseloc + 1;
   win := (track[player,horsenum].togo = 0);
   if win then
   begin
      writeln;
      writeln('Congratulations player ',player);
      writeln('Your horse ',horsenum,' has won the race!')
   end;
end;

begin (* play *)
   write('Player number ',player,' ');
   readln(ch);
   if (ch='p') or (ch='P') then  (* player types a P to request
                                    a display of the tracks *)
   begin
      print;
      readln
   end;
   die1 := throwdie;
   die2 := throwdie;
   poshorses := [die1, die2, (die1 + die2)];
   write('You rolled a ',die1,' and a ',die2);
   writeln(' with a total of ',(die1 + die2));
   write('Which do you want? ');
   readln(horsenum);
   while not (horsenum in poshorses) do
   begin
      writeln('Sorry, but only ',die1,', ',die2,', or ',(die1 + die2));
      writeln(' is allowed.');
      write('Which do you want? ');
      readln(horsenum)
   end;
   if horsenum = (die1 + die2) then
      movehorse (player, (die1 + die2))
   else
   begin
      movehorse (player,die1);
      if not win then
         movehorse (player, die2)
   end
end;  (* play  *)

begin     (* main program  *)

   win := false;
   getnumplayers;
   maketracks;
   curplayer := 1;
   while not win do  (* win is set to true in procedure movehorse  *)
   begin
      randomize;
      play (curplayer);
      if curplayer = numplayers then
         curplayer := 1  (* go back to the first player *)
      else
         curplayer := curplayer + 1  (* next player *)
   end;
   writeln;
   writeln
end.   (* horserace  *)
        write('\');
             end;
   end;
end;

procedure pri