program telephone;

const
   lettersperdigit =  3;    (* each number can have 3 letters *)
   maxnumlength    = 10;    (* allows long distance numbers   *)
   maxconsonants   =  3;    (* maximum number of consonants   *)
                            (* that are allowed in a word, if *)
                            (* there are more than this many  *)
                            (* the rest of the combinations   *)
                            (* with this root are skipped     *)

type
   digitslet = array[1..lettersperdigit] of char;

var
   dial      : array['0'..'9'] of digitslet;
   vowels    : set of 'A'..'Z';
   phonenum  : string[10];
   word      : packed array [1..maxnumlength] of char;
   numlength : integer;

(*
 * initializes the array dial to contain the same (digit, 3 letters)
 * groups that are on telephones. 0 and 1 do not have any, and are set
 * to blank. the rest of the numbers are mapped to succeeding triples
 * of letters ( 2 -> A,B,C). the exception to this is that the letter
 * Q is ignored, making the triple for 7 to be P,R,S.
 *)

procedure setupdial;

var
   i      : integer;
   digit  : char;
   letter : char;

begin
   for digit := '0' to '1' do
      for  i := 1 to lettersperdigit do
         dial[digit][i] := ' ';
   letter := 'A';
   for digit := '2' to '9' do
      for i := 1 to lettersperdigit do
         begin
         if letter = 'Q' then
            letter := succ(letter);
         dial[digit][i] := letter;
         letter := succ(letter);
         end;
end;

(*
 * enoughvowels applies a simple test to the word to find out if it is
 * pronounceable
 *)

function enoughvowels(sofar:integer):boolean;

var
   posinword, lastvowel : integer;

begin
   lastvowel := 0;
   enoughvowels := true;
   if sofar > maxconsonants then
      for posinword := 1 to sofar do
         begin
         if word[posinword] in vowels then
            lastvowel := posinword
         else if posinword - lastvowel > maxconsonants then
            enoughvowels := false;
         end;
end;

(*
 * this is a recursive procedure which prints all possible combinations
 * of letters for the digits in the given phone number. given a position in
 * the number at which to continue looking, it loops through all the
 * possible values of the current position's letter (determined by the
 * corresponding letters for that digit on a telephone dial). for each
 * letter, it calls itself recursively on the next position in the number
 * (to figure out the next letter). the end of the number is reached
 * when the position would extend past the end of the string; then the current
 * permutation is printed and it returns to get the next combination.
 *)

procedure permutate(position:integer);

var
   i : integer;

begin
   if keypressed then
      halt;
   if position > numlength then
      writeln(word:numlength)
   else for i := 1 to lettersperdigit do
      begin
      word[position] := dial[phonenum[position]][i];
      if enoughvowels(position) then
         permutate(position + 1);
      end;
end;

(*
 * returns true if ch is a numerical character
 *)

function isdigit (ch : char) : boolean;

begin
   isdigit := (ch >= '0') and (ch <= '9');
end;

(*
 * determines if the string typed in might be a syntactically correct
 * telephone number. it must contain digits; it may optionally have
 * dashes (which are removed).
 *)

function arealphonenumber : boolean;

var
   digitpos : integer;

begin
   arealphonenumber := true;
   while pos('-',phonenum) <> 0 do
      delete(phonenum,pos('-',phonenum),1);
   numlength := length(phonenum);
   if (numlength <= 0) or (numlength > maxnumlength) then
      arealphonenumber := false
   else for digitpos := 1 to numlength do
      if not isdigit(phonenum[digitpos]) then
         begin
         arealphonenumber := false;
         end;
end;

begin
   setupdial;
   vowels := ['A','E','I','O','U','Y'];
   writeln('This program finds words that match the digits in a telephone');
   writeln('number. Words with more than ',maxconsonants:1,' consonants are automatically');
   writeln('rejected. Numbers may have embedded dashes and may be up to ',maxnumlength:1);
   writeln('digits long. Hit any key to exit before all the combinations');
   writeln('have been listed.');
   writeln;
   repeat
      write('What is your number? ');
      readln(phonenum);
   until arealphonenumber;
   permutate(1);
end.
