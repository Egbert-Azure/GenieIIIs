(*
 * chuckaluck
 * you choose a number between 1 and 6.
 * I roll three dice, if you match 1, you
 * get even money. If you match 2 dice,
 * you get double, if you match all three,
 * you get triple.
 *)

program chuckaluck(input,output);

const

   startmoney   =  1000;     (* amount of money you start with *)

   maxdie       =     6;     (* maximum value of a die *)
   mindie       =     1;     (* minimum value of a die *)

   numdie       =     3;     (* number of dice *)

   payoff1      =     1;     (* pay off even money on 1 match *)
   payoff2      =     2;     (* pay off double on two *)
   payoff3      =     3;     (* pay off triple on three *)
type
   str80  = string[80];

var
   money  : integer;         (* the amount of money you have *)
   bet    : integer;         (* amount you bet *)
   guess  : mindie..maxdie;  (* the die you guess *)
   ch     : char;            (* used for input *)
   eachdie: 1..numdie;       (* index used to roll die *)
   matched: 0..numdie;       (* number of matched dice *)

(*
 * inst
 * prints out instructions
 *)

procedure inst;
begin

   writeln('Chuck-a-luck');
   writeln;
   writeln('You choose a number between 1 and 6.');
   writeln('I roll three dice, if you match 1, you');
   writeln('get even money. If you match 2 dice,');
   writeln('you get double your bet, if you match all');
   writeln('three, you get triple.');
   writeln
end;  (* inst *)

(*
 * getnum
 * ask a question from the player,
 * and get a number between min and
 * max with error checking
 *)

function getnum(question:str80;min,max:integer):integer;

var
   inter : integer;          (* intermediate result *)
begin
   write(question);
   readln(inter);
   if (inter < min) or (inter > max) then
   begin
      writeln('only numbers between ',min,' and ',max,' are allowed.');
      getnum := getnum(question,min,max)
   end
   else
      getnum := inter
end;  (* getnum *)

(*
 * rolldie
 * roll a die and print out its
 * value.
 *)

function rolldie: integer;

var
   dievalue : mindie..maxdie;
begin
   randomize;
   dievalue := mindie + random(maxdie);
   write(dievalue:4,'  ');
   rolldie := dievalue
end; (* rolldie *)


begin
   money := startmoney;

   write('Want instructions? ');
   readln(ch);
   if (ch = 'y') or (ch = 'Y') then
      inst;


   repeat
      writeln;
      writeln('You have $',money);
      bet := getnum('How large a bet? ',1,money);
      guess := getnum('Pick a number ',mindie,maxdie);

      matched := 0;
      writeln('Die1  Die2  Die3');
      for eachdie := 1 to numdie do
         if rolldie = guess then
            matched := matched + 1;

      writeln;

      if matched = 1 then
      begin
         writeln('You won $',bet*payoff1);
         money := money + bet*payoff1
      end

      else
      if matched = 2 then
      begin
         writeln('you won $',bet*payoff2);
         money := money + bet*payoff2
      end

      else
      if matched = 3 then
      begin
         writeln('You won $',bet*payoff3);
         money := money + bet*payoff3
      end

      else
      begin
         writeln('You lost $',bet);
         money := money - bet
      end;

      if money > 0 then
      begin
         write('Care to try your luck again? ');
         readln(ch)
      end

   until (money <= 0) or ((ch<>'y') and (ch<>'Y'));

   writeln('You leave with $',money)

end.  (* chuckaluck *)

