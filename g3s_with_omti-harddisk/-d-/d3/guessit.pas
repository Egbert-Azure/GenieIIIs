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
This is the simpliest game, guessing a number between 1 and 100.
You are told if you are high or low.
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
 *  GUESS IT
 *  A computer guessing game
 *)
program guessit (input,output);
const
   maxtrys        =  6;        (* user allowed 6 tries to guess number *)
   maxnum         = 100;       (* maximum number *)
   minnum         =  1;        (* minimum number *)

var
   ch : char;               (* used to answer questions *)
   guess : integer;         (* human's guess *)
   number: minnum..maxnum;  (* the number itself *)
   numtry: 0..maxtrys;      (* number od times human has guessed *)


  (*
   * instructions
   * prints out the instructions
   *)
  procedure instructions;
  begin
     writeln('This is match it');
     writeln;
     writeln('I will choose a number between 1 and 100.');
     writeln('You will try to guess that number.');
     writeln('If you guess wrong, I will tell you if you guessed');
     writeln('too high, or too low.');
     writeln('you have ',maxtrys,' tries to get the number.');
     writeln;
     writeln('Enjoy');
     writeln;
  end;   (* instructions *)


  (*
   * getnum
   * get a guess from the human,
   * with error checking
   *)
  procedure getnum;
  begin
     write('Your guess? ');

     (*
      * wait for a character
      *)

     begin

        readln(guess);
        if guess > maxnum then
        begin
           writeln('Illegal number');
           getnum
        end
        else
        begin
           if guess > number then
              writeln('too high')
           else
              if guess < number then
                 writeln('too low')
              else
                 writeln('correct!!!')
        end
     end
  end;   (* getnum *)

(*
 * main
 *)

begin
   instructions;
   randomize;
   while pos(ch,'nN')=0 do
   begin
      guess := 0;
      numtry := 0;
      number := random(maxnum)+1-minnum;   (* get number *)
      while (numtry < maxtrys) and (guess <> number) do
      begin
         getnum;
         numtry := numtry + 1
      end;
      writeln;
      if guess <> number then
         writeln('The number was ',number);
      writeln;
      write('want to try again? ');
      readln(ch)
   end
end.
 *)

procedure makeboard;

var