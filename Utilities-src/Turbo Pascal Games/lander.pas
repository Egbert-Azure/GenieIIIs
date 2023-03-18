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
This is a game where you try to land on a planet. The planet is
unspecified and changes each game. Thus the gravity changes, as well as
fuel supplies, speed, and starting height. In addition there are meteor
swarms to contend with.
Version 1.00.
Turbo Pascal 2.0, 3.0.

* ASSOCIATED FILES
LANDER.PAS
LANDER.DOC

* CHECKED BY
DM 06/08/86

* KEYWORDS
CP/M-80

==========================================================================
}
program lander;

const
   instrfile      = 'LANDINST.DAT';
   maxrand        = 32767;
   timeinc        = 0.1;
   slightly       = 0.1;
   moderately     = 0.2;
   very           = 0.3;
   impossible     = 0.5;
   certainty      = 1.0;
   minmeteors     = 2.0;
   maxmeteors     = 7.0;
   eachmisschance = 0.8;
   minavoid       = 10;
   landingheight  = 0.1;
   crashlandspeed = 6.0;

type
   answers = set of 'A'..'Z';
   str80 = string[80];

var
   speed, height, gravity : real;
   maxlandingspeed        : real;
   chance, misschance     : real;
   burn, fuel             : integer;
   nummeteors             : integer;
   inp                    : string[10];
   ranout                 : boolean;

(*
 * returns a random real number between lowlimit and highlimit
 *)

function rand(lowlimit, highlimit : real) : real;

begin
   rand := lowlimit + (highlimit - lowlimit + 1) * random;
end;

(*
 * returns true if a random number, weighted by the difficulty level, is
 * less than the argument
 *)

function unlucky(percentchance : real) : boolean;

begin
   unlucky := rand(0.0, 1.0 - chance) <= percentchance;
end;

(*
 * converts a numerical ascii character to its corresponding integral value
 *)

function asciitoint(digit : char) : integer;

begin
   asciitoint := ord(digit) - ord('0');
end;

(*
 * returns true if the argument is a numerical digit
 *)

function isdigit(ch : char) : boolean;

begin
   isdigit := (ch >= '0') and (ch <= '9');
end;

(*
 * convert determines the numerical value of the digits ina string. it returns
 * the integer and updates the string to now contain whatever was after the
 * end of the number. if the string is blank, the value is zero; if a number
 * is not found it returns -1.
 *)

function convert(numstring : str80) : integer;

var
   intvalue       : integer;
   position       : integer;
   digitsfound    : integer;
   endofnumber    : boolean;
   notinteresting : boolean;

begin
   intvalue := 0;
   digitsfound := 0;
   position := 1;
   endofnumber := false;
   notinteresting := true;
   if length(numstring) > 0 then
      begin
      while (position < length(numstring)) and notinteresting do
         if numstring[position] = ' ' then
            position := position + 1
         else
            notinteresting := false;
      while (position <= length(numstring)) and (not endofnumber) do
         if isdigit(numstring[position]) then
            begin
            intvalue := intvalue * 10 + asciitoint(numstring[position]);
            position := position + 1;
            digitsfound := digitsfound + 1;
            end
         else
            endofnumber := true;
      if digitsfound = 0 then
         intvalue := -1;
      end;
   convert := intvalue;
end;

(*
 * asks a question with a single character answer. if the response is in the
 * set 'answers', the letter is returned. otherwise the string ifbad is
 * printed and the question is asked again.
 *)

function ask(question, ifbad : str80; responses : answers) : char;

var
   bad : boolean;
   ch  : char;

begin
   bad := true;
   repeat
      write(question);
      readln(ch);
      writeln;
      if ch in responses then
         bad := false
      else
         writeln(ifbad);
   until not bad;
   ask := ch;
end;

(*
 * prints out the instructions from a file instrfile
 *)

procedure instructions;

var
   instruct : text;
   ch       : char;
   str      : string[80];

begin
   assign(instruct,instrfile);
   reset(instruct);
   while not eof(instruct) do
      begin
      readln(instruct,str);
      writeln(str);
      end;
end;

procedure startup;

begin
   if ask('Do you want instructions? ','Please answer Y or N',['Y','N']) = 'Y' then
      instructions;
   randomize;
end;

(*
 * ask the player for the difficulty level of the next landing
 *)

procedure getdifficulty;

var
   level : char;

begin
   level := ask('Level of difficulty? ','B:Beginner, E:Expert, N:Navigator, A:Astronaut',
                 ['B','E','N','A']);
   case level of
      'B' : chance := slightly;
      'E' : chance := moderately;
      'N' : chance := very;
      'A' : chance := impossible;
   end;
end;

(*
 * variables that must be re-set each time a new landing is attempted
 *)

procedure startgame;

begin
   getdifficulty;
   gravity := rand(9.0 + chance, 11.0 + chance);
   height  := rand(1.0 + chance, 2.0 + chance) * 100.0;
   speed   := rand(0.0, 100.0 * chance) + 30.0;
   fuel    := round(50.0 * rand(3.0 - chance, 4.0 - chance));
   maxlandingspeed := crashlandspeed - 10.0 * chance;
   misschance := certainty;
   ranout := false;
end;

(*
 * tell player his height, speed and direction
 *)

procedure writestatus;

begin
   writeln;
   write('You are ');
   if speed > 0.0 then
      write('falling')
   else
      write('rising');
   writeln(' from a height of ',height:1:1);
   writeln('meters at ',abs(speed):1:1,' m/s.');
   if not ranout then
      writeln('There are ',fuel:1,' liters of fuel left.');
   ranout := fuel = 0;
end;

(*
 * determines if there are any meteors, and if so, how many
 *)

procedure lookformeteors;

var
   eachrock : integer;

begin
   misschance := certainty;
   if unlucky(0.1) then
      begin
      nummeteors := round(rand(minmeteors,maxmeteors + 10.0 * chance));
      for eachrock := 1 to nummeteors do
         misschance := misschance * eachmisschance;
      writeln('We are on a collision course with ',nummeteors:1);
      write('meteors.');
      if ranout then
         writeln
      else
         begin
         writeln('If we do not use more than ',minavoid:1,'liters of fuel in the');
         writeln('next second, there is a ',round(100.0*(1.0-misschance)),
                 ' % probability that we will be');
         writeln('hit. If more is used it will be only 10 %.');
         end;
   end;
end;

(*
 * asks the player for the amount of fuel to use in the next time period.
 *)

procedure getburn;

const
   fuelprompt = 'Units of fuel : ';

begin
   repeat
      write(fuelprompt);
      readln(inp);
      burn := convert(inp);
      if burn > fuel then
         writeln('There isn''t that much fuel left.')
      else if burn < 0 then
         writeln('I don''t think that''s possible.');
   until (burn <= fuel) and (burn >= 0);
end;

(*
 * figure out the craft's new speed according to the laws of physics
 *)

procedure updatestatus;

var
   deltat : integer;

begin
   fuel := fuel - burn;
   deltat := 0;
   repeat
      deltat := deltat + 1;
      height := height - speed * timeinc - (gravity - burn) * 0.5 * sqr(timeinc);
      speed := speed + (gravity - burn) * timeinc;
   until (deltat = trunc(1 / timeinc)) or (height <= landingheight);
end;

(*
 * all the procedures that make a turn
 *)

procedure doaturn;

begin
   writestatus;
   lookformeteors;
   if fuel > 0 then
      getburn
   else
      burn := 0;
   updatestatus;
end;

(*
 * the course has changed: meteors have a lower chance of hitting
 *)

function coursechanged : boolean;

begin
   if unlucky(0.1) then
      begin
      writeln('Despite the precautionary measures taken, the ship was destroyed.');
      coursechanged := true;
      end
   else
      begin
      writeln('Your prudent actions saved the ship from the menacing meteors!');
      coursechanged := false;
      end
end;

function coursesame: boolean;

begin
   if not unlucky(1.0 - misschance) then
      begin
      writeln('Your piloting skills have steered you through the center of the swarm!');
      coursesame := false;
      end
   else if ranout then
      begin
      writeln('What a pity .. your craft was demolished by meteors before it could');
      writeln('be vaporized on contact with the surface.');
      coursesame := true;
      end
   else
      begin
      writeln('Your pointless gambling has destroyed the ship, you foolish plebe!');
      coursesame := true;
      end;
end;

(*
 * figures if any meteors (if there were any) managed to hit the ship.
 * different messages are printed depending on the thrust of the last turn.
 *)


function anyhit : boolean;

begin
   if (misschance = certainty) or (height <= landingheight) then
      anyhit := false
   else if burn > minavoid then
      anyhit := coursechanged
   else
      anyhit := coursesame;
end;

(*
 * returns true if the ship has come close enough to the ground that we can
 * say it has landed. could crash or touch down safely.
 *)

function landed : boolean;

begin
   if height < landingheight then
      begin
      speed := abs(speed);
      if speed < maxlandingspeed then
         write('We have landed safely')
      else
         write('We have crashed');
      writeln(' at a speed of ',speed:1:1,' meters/second.');
      landed := true;
      end
   else
      landed := false;
end;


begin
   startup;
   repeat
      startgame;
      repeat
         doaturn;
      until landed or anyhit;
   until ask('Again? ','Y or N',['Y','N']) = 'N';
   writeln('Bye!');
end.





 * init
 * init strings
 *)

procedure init;