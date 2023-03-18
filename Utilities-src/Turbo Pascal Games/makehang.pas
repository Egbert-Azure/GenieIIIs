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
This is the program to create the word for HANGMAN.PAS. It must be run to
create a word file before HANGMAN can be used.
Version 1.00.
Turbo Pascal 2.0, 3.0.

* ASSOCIATED FILES
HANGMAN.PAS
MAKEHANG.PAS

* CHECKED BY
DM 06/08/86

* KEYWORDS
CP/M-80

==========================================================================
}
program makehang;

(*
 * this program makes the datafile that is used by the hangman game.
 *)

const
   stringsize = 10;

type
   word = string[stringsize];

var
   i : integer;
   dict : file of word;
   nextword : word;
   numwords : integer;

(*
 * the dictionary for hangman is a file of strings. it cannot be a text
 * file because seeking is not allowed on text, and hangman uses a random
 * seek to get the word. the first string in the file contains the
 * number of words in the dictionary.
 *)

begin
   numwords := 0;
   nextword := '';
   assign(dict, 'words.dat');
   rewrite(dict);
   str(0,nextword);
   write(dict,nextword);
   writeln('Type the words to be entered into the dictionary; no more than');
   writeln(stringsize:1,' letters long. Type and extra carriage return to exit.');
   nextword := '';
   readln(nextword);
   for i := 1 to length(nextword) do nextword[i] := upcase(nextword[i]);
   while (length(nextword) > 0) do
      begin
      write(dict,nextword);
      numwords := numwords + 1;
      nextword := '';
      readln(nextword);
      for i := 1 to length(nextword) do nextword[i] := upcase(nextword[i]);
   end;
   writeln('That''s all, folks!');
   seek(dict,0);
   nextword := '';
   str(numwords,nextword);
   write(dict,nextword);
   close(dict);
end.

egral value
 *)

function asciitoint(digit : char) : integer;

begin
 