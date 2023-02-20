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
This is a typical cannon shooting game. You pick an angle to fire the
the cannon at and, if you get close enough, you hit the enemy.
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
 * artillery
 * fire a shell at an enemy outpost
 *)

program artillery (input, output);

const
   numshells  =   10;   (* allowed 10 shells per target *)
   mindist    =  100;   (* minimum distance for a target *)
   maxdist    = 1000;   (* maximum distance for a target *)

   velocity   = 200.0;  (* initial velocity of 200 ft/sec^2 *)
   gravity    = 32.2;   (* gravity of 32.2 ft/sec^2 *)
   pi         = 3.14159;

var
   angle  : real;       (* angle to shoot at *)
   enemy  : integer;    (* how far away the enemy is *)
   killed : integer;    (* how many we have hit *)
   shots  : 0..numshells; (* number of shells left *)
   ch     : char;       (* used to answer questions *)
   hit    : boolean;    (* whether the enemy has been hit *)

(*
 * dist
 * returns how far the shell went
 *)

function dist:integer;

(*
 * timeinair
 * figures out how long the shell
 * stays in the air
 *)

function timeinair:real;

begin
   timeinair := (2*velocity * sin(angle))/gravity
end;

begin
   dist := round((velocity * cos(angle))*timeinair)
end;

(*
 * fire
 * the user fires at enemy
 *)

procedure fire;

begin
   randomize;
   enemy := mindist + random(maxdist-mindist);
   writeln('The enemy is ',enemy:3,' feet away!!!');
   shots := numshells;
   repeat
      write('What angle? ');
      readln(angle);
      angle := (angle * pi)/180.0;
      hit := abs(enemy-dist) <= 1;
      if hit then
      begin
         killed := killed + 1;
         writeln('You hit him!!!');
         writeln('It took you ',numshells-shots,' shots.');
         if killed = 1 then
             writeln('You have killed one enemy.')
         else
             writeln('You have now destroyed ',killed,' enemies of democracy.')
      end
      else
      begin
         shots := shots - 1;
         if dist > enemy then
            write('You overshot by ')
         else
            write('You under shot by ');
         writeln(abs(enemy-dist))
      end
   until (shots = 0) or hit;
   if shots = 0 then
      writeln('You have run out of ammo.')
end;

begin
   writeln('Welcome to artillery');
   writeln;
   writeln('You are in the middle of a war (depressing, no?) and are being');
   writeln('charged by thousands of enemies.');
   writeln('Your job is to destroy their ouytposts. You have at your disposal');
   writeln('a cannon, which you can shoot at any angle.  As this is war,');
   writeln('supplies are short, so you only have ',numshells,' per target.');
   writeln;

   killed := 0;
   repeat
      writeln('*******************************************');
      fire;
      write('I see another one, care to shoot again? ');
      readln(ch);
      while not (ch in ['y','n','Y','N']) do
      begin
         writeln('Please answer yes or no');
         write('Want to try again? ');
         readln(ch)
      end
   until (ch<>'y') and (ch<>'Y');
   writeln;
   writeln('You killed ',killed,' of the enemy.')
end.
e computer will select 20 numbers
in the same range. You win base