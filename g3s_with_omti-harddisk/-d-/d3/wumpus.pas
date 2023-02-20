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
This is a typical game of Hunt the Wumpus.
Version 1.00.
Turbo Pascal 2.0, 3.0.

* ASSOCIATED FILES
None

* CHECKED BY
DM 06/09/86

* KEYWORDS
CP/M-80

==========================================================================
}
program wumpus;

const
   maxrooms            = 20;
   maxbats             = 2;
   maxpits             = 2;
   numberofarrows      = 7;
   prompt              = '> ';
   tunnelsperroom      = 2;
   move                = 'M';
   quit                = 'Q';
   shoot               = 'S';
   help                = '?';

type
   room = 1..maxrooms;
   rooms = set of room;

var
   cave         : array[room] of rooms;
   player       : room;
   wumpus       : room;
   arrowsleft   : integer;
   quitting     : boolean;
   killed       : boolean;
   wumpuskilled : boolean;
   bats         : rooms;
   pits         : rooms;
   commandset   : set of char;

(*
 * rand
 * returns a random number between low and high
 *)

function rand(low, high : integer) : integer;

begin
   rand := low + random(high-low+1);
end;

procedure doinstr;

begin
   writeln;
   writeln('Your mission, should you desire to accept it, is it hunt for the');
   writeln('Wumpus in his cave. To succeed, you must shoot it with one of your');
   writeln(numberofarrows:1,' arrows. If you shoot into a room which is not directly connected to');
   writeln('yours, the arrow will bounce to one of the rooms that does connect.');
   writeln('The bats in the cave may pick you up and place you in a different');
   writeln('room. If you enter a room which has a pit, you will fall into it.');
   writeln('If the wumpus finds you or you run out of arrows, you lose.');
   writeln;
end;

procedure askinstruct;

var
   answer : char;

begin
   write('Do you want instructions? ');
   readln(answer);
   while (answer <> 'y') and (answer <> 'Y') and (answer <> 'n') and (answer <> 'N') do
      begin
      writeln('Please answer yes or no');
      write('Would you like instructions? ');
      readln(answer);
      end;
   if (answer = 'y') or (answer = 'Y') then
      doinstr;
end;

(*
 * addtunnel
 * make a tunnel connection between two rooms
 *)

procedure addtunnel(from, dest : room);

begin
   cave[from] := cave[from] + [dest];
   cave[dest] := cave[dest] + [from];
end;

(*
 * makemaze
 * makes a reasonably random maze. for each room tries to make 3 new tunnels.
 * if a tunnel already exists in that direction, another digging that way is
 * not made.
 *)

procedure makemaze;

var
   currentroom, tunnelto, newtunnel : room;

begin
   for currentroom := 2 to maxrooms do
      addtunnel(currentroom, currentroom-1);
   for currentroom := 3 to maxrooms do
      begin
      newtunnel := rand(1, currentroom-1);
      if not newtunnel in cave[currentroom] then
         addtunnel(currentroom, newtunnel);
      end;
end;

(*
 * skipblanks
 * skips spaces, tabs until eoln
 *)

procedure skipblanks;


begin
end;

(*
 * describe
 * give a description of the current room ( player ). tell player if the
 * wumpus is nearby.
 *)

procedure describe;

var
   i : room;

begin
   writeln('You are in room ',player:1);
   write('There are tunnels leading to rooms');
   for i := 1 to maxrooms do
      if i in cave[player] then
         write(' ',i:1);
   writeln;
   if (player in cave[wumpus]) or ((cave[player] * cave[wumpus]) <> []) then
      writeln('I smell a Wumpus.');
   if cave[player] * bats <> [] then
      writeln('I hear bats.');
   if cave[player] * pits <> [] then
      writeln('I feel a draft.');
end;

(*
 * command
 * returns the single character that signifies what is to be done
 *)

function command : char;

var
   ch : char;

begin
   describe;
   repeat
      write(prompt);
      readln(ch);
      if not (ch in commandset) then
         begin
         writeln(' Type ? for instructions.');
         end;
   until ch in commandset;
   command := ch;
end;

(*
 * checkwump
 * move the wumpus and see if it went to the same room as the player,
 * if so he's dead.
 *)

procedure checkwump;

var
   newwumproom : room;

begin
   newwumproom := rand(1, maxrooms);
   if (newwumproom in cave[wumpus]) then
      wumpus := newwumproom;
   if (wumpus = player) then
      begin
      writeln('Look Out!! The Wumpus got you.');
      writeln('Better luck next time.');
      killed := true;
      end;
end;

(*
 * checkbats
 * if the player is in a room with bats they will pick him up and move him to
 * another room (which will not have bats in it).
 *)

procedure checkbats;

var
   flewto : room;

begin
   if player in bats then
      begin
      repeat
         flewto := rand(1,maxrooms)
      until (not (flewto in (bats + pits))) and (flewto <> wumpus);
      writeln('A superbat picked you up and carried you off.');
      player := flewto;
      end;
end;

(*
 * checkpits
 * determines if the player fell into a pit.
 *)

procedure checkpits;

begin
   if not killed and (player in pits) then
      begin
      writeln('Don''t do that!! Too late, you fell into a pit.');
      writeln('You should be more careful.');
      killed := true;
      end;
end;

(*
 * randroom
 * returns a random room number in the range limited by the set argument.
 *)

function randroom ( limitedto : rooms) : integer;

var
   apossibility : room;

begin
   repeat
      apossibility := rand(1, maxrooms);
   until apossibility in limitedto;
   randroom := apossibility;
end;

(*
 * doshoot
 * player tries to shoot the wumpus by listing the rooms that he wants to
 * shoot through. if the rooms do not match the list, the arrow bounces
 * randomly to a connecting tunnel.
 *)

procedure doshoot;

var
   nextroom, lastroom : room;

begin
   lastroom := player;
   while not eoln do
      begin
      write('where ');
      readln(nextroom);
      if wumpus = nextroom then
         wumpuskilled := true
      else if player = nextroom then
         killed := true;
      if not (nextroom in cave[lastroom]) then
         nextroom := randroom(cave[lastroom]);
      lastroom := nextroom;
      skipblanks;
      end;
   arrowsleft := arrowsleft - 1;
   if killed then
      writeln('You klutz! You just shot yourself.')
   else if wumpuskilled then
      writeln('Congratulations! You slew the fearsome Wumpus.')
   else if arrowsleft = 0 then
      writeln('You ran out of arrows.')
end;

(*
 * domove
 * player's move, must be to an adjacent room
 *)

procedure domove;

var
   dest : room;

begin
   write('To ');
   readln(dest);
   if not (dest in [1..maxrooms]) then
      writeln('There is no room # ', dest)
   else if not (dest in cave[player]) then
      writeln('I see no tunnel to room # ',dest)
   else
      player := dest;
   checkbats;
   checkwump;
   checkpits;
end;

(*
 * doquit
 * asks if the player really wants to quit
 *)

procedure doquit;

var
   answer : char;

begin
   writeln;
   write('Do you really want to quit now? ');
   readln(answer);
   quitting := answer in ['y','Y'];
end;

procedure doaturn(action : char);

begin
   case action of
      move  : domove;
      shoot : doshoot;
      quit  : doquit;
      help  : doinstr;
      end;
end;

(*
 * gameover
 * returns true if the game is over
 *)

function gameover : boolean;

begin
   gameover := quitting or killed or wumpuskilled or (arrowsleft = 0);
end;

(*
 * initialize
 * generates a random maze and the positions of the player, wumpus,and bats.
 * make sure that the player doesn't start with the wumpus.
 *)

procedure initialize;

var
   i : room;

begin
   randomize;
   for i := 1 to maxrooms do
      cave[i] := [];
   bats := [];
   pits := [];
   makemaze;
   wumpus := rand(1, maxrooms);
   for i := 1 to maxbats do
      bats := bats + [rand(1, maxrooms)];
   for i := 1 to maxpits do
      pits := pits + [rand(1, maxrooms)];
   repeat
      player := rand(1, maxrooms);
   until (player <> wumpus) and not (player in pits) and not (player in bats);
   quitting := false;
   killed := false;
   wumpuskilled := false;
   arrowsleft := numberofarrows;
   commandset := [move, shoot, quit, help];
end;

begin
   writeln('Welcome to Wumpus!!');
   askinstruct;
   initialize;
   repeat
      doaturn(command);
   until gameover;
end.



gambling has destroyed the ship, you foolish plebe!');
      coursesame := true;
      end;
end;

(*
 * figures if any m