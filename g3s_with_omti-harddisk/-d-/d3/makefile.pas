program makedict;

(*
 * this program makes the datafile that is used by the hanggman game.
 *)

const
   stringsize = 10;

type
   word = string[stringsize];

var
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
   writeln(stringsize:1,' letters long. Type the end-of-file character to exit.');
   nextword := '';
   readln(nextword);
   while not eof(con) and (length(nextword) > 0) do
      begin
      write(dict,nextword);
      numwords := numwords + 1;
      nextword := '';
      readln(nextword);
   end;
   writeln('That''s all, folks!');
   seek(dict,0);
   nextword := '';
   str(numwords,nextword);
   write(dict,nextword);
   close(dict);
end.

