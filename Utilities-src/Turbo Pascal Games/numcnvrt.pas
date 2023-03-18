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
This program lets you practice converting numbers from one base
to another.
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
 * numcnvrt
 * tests your ability to convert a number
 * from bases
 * binary -> decimal, decimal -> binary
 * octal -> decimal, decimal -> octal
 * binary -> octal, octal -> binary
 *)

program numcnvrt (input, output);

const
   base2      = 2;     (* binary *)
   base8      = 8;     (* octal *)
   base10     = 10;    (* decimal *)

type
   base = (binary,octal,decimal);

var
   i        : integer;          (* index variable *)
   right    : integer;          (* number of correct gueses *)
   ch       : char;             (* used to answer questions *)
   frombase : base;             (* from this base *)
   tobase   : base;             (* to this base *)
   good     : boolean;
   input    : char;

(*
 * makenumber
 * creates a random number of base
 * first, then prints it out in base
 * first. it then returns a decimal representation
 * of that number
 *)

function makenumber(first:base):integer;

var
   temp  : string[10];
   num   : integer;
   index : integer;
   i     : integer;

begin
   num := 0;
   case first of
      binary : begin
                  write('Binary: ');
                  for index := 1 to 5 do
                  begin
                     i := random(base2);
                     num := num * base2 + i;
                     write(i:1)
                  end
               end;
      octal : begin
                 write('Octal: ');
                 for index := 1 to 2 do
                 begin
                    i := random(base8);
                    num := num * base8 + i;
                    write(i:1)
                 end
              end;
      decimal : begin
                   write('Decimal: ');
                   for index := 1 to 2 do
                   begin
                      i := random(base10);
                      num := num * base10 + i;
                      write(i:1)
                   end
                end
   end;
   makenumber := num
end;

(*
 * getnumber
 * gets a number from the user, in the
 * passed base. it then checks to see
 * if it is equal to the decimal answer
 * it was passed. it returns a boolean indictor
 * whether or not they matched
 *)

function getnumber(readbase:base; answer:integer):boolean;

var
   ch        : string[5];     (* input from user *)
   factor    : integer;       (* multiplying factor *)
   inter     : integer;       (* intermediate results *)
   index     : integer;       (* index into ch *)
   strlength : integer;       (* length of read in string *)

begin
   write(' -> ');
   case readbase of
      decimal : begin
                   write(' decimal: ');
                   factor := base10
                end;
      octal : begin
                   write(' octal: ');
                   factor := base8
              end;
      binary : begin
                   write(' binary: ');
                   factor := base2
               end
   end;
   readln(ch);
   strlength := length(ch);
   index := 1;
   inter := 0;
   while (strlength >= index) do
   begin
      if ch[index] in ['0'..'9'] then
         inter := (inter * factor) + (ord(ch[index]) - ord('0'))
      else
         index := strlength;
      index := index + 1
   end;
   if inter = answer then
      getnumber := true
   else
   begin
      writeln('Wrong!!!');
      getnumber := false
   end
end;

begin
   repeat
      writeln('Which would you like?');
      writeln('1) Binary to decimal');
      writeln('2) Binary to octal');
      writeln('3) Octal to decimal');
      writeln;
      write('Which number? ');
      readln(input);
      repeat
      good := true;
      case input of
         '1' : begin
                  frombase := binary;
                  tobase := decimal
               end;
         '2' : begin
                   frombase := binary;
                  tobase := octal
               end;
         '3' : begin
                   frombase := octal;
                  tobase := decimal
               end
         else
         begin
            writeln('Illegal number');
            write('Which number? ');
            readln(input);
            good := false
         end
      end;
      until good = true;
      randomize;
      right := 0;
      for i := 1 to 5 do
         if getnumber(tobase, makenumber(frombase)) then
            right := right + 1;
      writeln;
      writeln('You got ',right:2,' out of 5');
      writeln;
      writeln('Again? ');
      readln(ch);
   until (ch<>'y') and (ch<>'Y')
end.
