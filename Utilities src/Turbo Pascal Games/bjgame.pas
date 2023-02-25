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
This is the time honored game of blackjack. This is a pretty rudimentary
version of the game splitting and doubling down are not allowed.
Blackjacks are honored, but pay only even money. Blackjack pushes are won
by the house. The dealer plays on even if you bust and thus can also bust.
This is a push (no bet).
Version 1.00.
Turbo Pascal 2.0, 3.0

* ASSOCIATED FILES
None

* CHECKED BY
DM 06/08/86

* KEYWORDS
CP/M-80

==========================================================================
}
program bjgame;

const
   decksize    = 52;
   maxhandsize = 5;
   mincards    = 5;
   dealerstays = 17;
   busted      = 21;
   startamount = 100;
   minbet      = 2;
   maxbet      = 200;

type
   cardsuit = (spades, hearts, clubs, diamonds);
   cardvalue = (duece, three, four, five, six, seven, eight, nine, ten,
                jack, queen, king, ace);
   cardstate = (picked, indeck);
   card = record
             suit : cardsuit;
             value : cardvalue;
             state : cardstate;
          end;
   hand = array [1..maxhandsize] of card;

var
   deck         : array[1..decksize] of card;
   cardsleft    : integer;
   suitname     : array[cardsuit] of string[8];
   valuename    : array[cardvalue] of string[5];
   countvalue   : array[cardvalue] of integer;
   player       : hand;
   dealer       : hand;
   money        : integer;
   bet          : integer;
   curcard      : integer;

(*
 * write the suit and value of a card
 *)

procedure printcard(acard: card);

begin
   write('the ',valuename[acard.value]);
   writeln(' of ',suitname[acard.suit]);
end;

(*
 * asks for intructions
 *)

procedure instructions;

var
   response : char;

begin
   writeln(' ':15,'Blackjack for one');
   writeln('Do you want instructions? ');
   readln(response);
   if (response = 'y') or (response = 'Y') then
      begin
      writeln('This program plays a simple version of blackjack. Neither');
      writeln('splitting, nor modification of the bet after the hand has');
      writeln('been dealt is allowed.');
      end;
   writeln;
end;

(*
 * returns true if the card c is in the hand
 *)

function inhand(c: card; whose: hand): boolean;

var
   handindex : integer;

begin
   inhand := false;
   for handindex := 1 to maxhandsize do
      if ((c.suit = whose[handindex].suit) and
         (c.value = whose[handindex].value)) then
         begin
         inhand := true;
         end;
end;

(*
 * returns a random index into the deck
 *)

function randcard(l: integer) : integer;

begin
   randcard := 1 + random(l);
end;

(*
 * removes all cards from the argument hand
 *)

procedure clearhand(var ahand: hand);

var
   handindex : integer;

begin
   for handindex := 1 to maxhandsize do
      ahand[handindex].state := indeck;
end;

(*
 * initialize the names of the suits and values
 *)

procedure initialize;

var
   i       : integer;
   cardval : cardvalue;

begin
   instructions;
   clearhand(player);
   clearhand(dealer);
   money := startamount;
   cardsleft := 0;
   i := 2;
   for cardval := duece to ten do
      begin
      countvalue[cardval] := i;
      i := i + 1;
      end;
   for cardval := jack to king do
      countvalue[cardval] := 10;
   countvalue[ace] := 11;

   valuename[duece] := 'two';
   valuename[three] := 'three';
   valuename[four]  := 'four';
   valuename[five]  := 'five';
   valuename[six]   := 'six';
   valuename[seven] := 'seven';
   valuename[eight] := 'eight';
   valuename[nine]  := 'nine';
   valuename[ten]   := 'ten';
   valuename[jack]  := 'jack';
   valuename[queen] := 'queen';
   valuename[king]  := 'king';
   valuename[ace]   := 'ace';
   suitname[diamonds] := 'diamonds';
   suitname[spades]   := 'spades';
   suitname[hearts]   := 'hearts';
   suitname[clubs]    := 'clubs';
   randomize;
end;

(*
 * shuffles the cards that are not in either player's hand. the initial shuffle
 * does all the cards because both hands start empty.
 *)

procedure shuffle;

var
   asuit  : cardsuit;
   avalue : cardvalue;
   i      : integer;

(*
 * exchange the cards at the two positions in the deck
 *)

procedure swapcard(first, second : integer);

var
   tempcard : card;

begin
   tempcard := deck[first];
   deck[first] := deck[second];
   deck[second] := tempcard;
end;

begin
   i := 1;
   for asuit := spades to diamonds do
      for avalue := duece to ace do
         with deck[i] do
         begin
         suit := asuit;
         value := avalue;
         if not (inhand(deck[i], player) or inhand(deck[i], dealer)) then
            begin
            state := indeck;
            i := i + 1;
            end;
         end;
   curcard := 0;
   cardsleft := i - 1;
   writeln('*** ',cardsleft:1,' cards left.');
   for i := 1 to cardsleft do
      swapcard(i, randcard(cardsleft));
end;

(*
 * returns the index into the deck of the next card. calls shuffle if deck
 * is nearly finished.
 *)

function pickcard : integer;

begin
   if cardsleft < mincards then
      begin
      writeln('Reshuffling ...');
      shuffle;
      end;
   curcard := curcard + 1;
   deck[curcard].state := picked;
   cardsleft := cardsleft - 1;
   pickcard := curcard;
end;

(*
 * determines the sum of the values in a hand. a card's state must be
 * 'picked' for it to be included. aces are assumed to be 11. if the
  * count is over 21 and there are aces in it, as many as are needed
  * will be devalued to 1.
  *)

function countcards(someone: hand): integer;

var
   handindex, sum, numaces : integer;

begin
   sum := 0;
   numaces := 0;
   for handindex := 1 to maxhandsize do
      if someone[handindex].state = picked then
         with someone[handindex] do
         begin
         if value = ace then
            numaces := numaces +1;
         sum := sum + countvalue[value];
         end;
   while (numaces > 0) and (sum > busted) do
      begin
      numaces := numaces - 1;
      sum := sum - 10;
      end;
   countcards := sum;
end;

(*
 * returns true if the argument hand is a blackjack
 *)

function blackjack(someone: hand): boolean;

begin
   blackjack := ((countvalue[someone[1].value] = 10) and
                 (countvalue[someone[2].value] = 11)) or
                ((countvalue[someone[1].value] = 11) and
                 (countvalue[someone[2].value] = 10));
end;

procedure getbet;

const
   betprompt = 'Size of bet (0 to end)? ';

begin
   write(betprompt);
   readln(bet);
   while not (bet in [0,minbet..maxbet]) or (bet > money) do
      begin
      write('A bet must be between ');
      writeln(minbet:1,' and ',maxbet:1);
      writeln('and must be no larger than the amount of money you have.');
      writeln('Enter 0 to leave.');
      write(betprompt);
      readln(bet);
      end;
   if bet = 0 then
      begin
      writeln('You have quit with $',money:1,'.');
      halt;
      end;
end;

(*
 * deals the cards tpo both participants for this hand
 *)

procedure dealhands;

begin
   player[1] := deck[pickcard];
   dealer[1] := deck[pickcard];
   player[2] := deck[pickcard];
   dealer[2] := deck[pickcard];
   write('You drew ');
   printcard(player[1]);
   write('and ');
   printcard(player[2]);
   writeln;
   write('The dealer''s up card is ');
   printcard(dealer[2]);
end;

(*
 * asks the player if more cards are wanted.
 *)

procedure playertakes;

var
   atcard : integer;
   answer : char;

begin
   atcard := 3;
   answer := 'h';
   while (atcard <= maxhandsize) and (countcards(player) < busted) and
         ((answer = 'h') or (answer = 'H')) do
      begin
      writeln('Your count is ',countcards(player));
      write('Hit or stay? ');
      readln(answer);
      if (answer = 'h') or (answer = 'H') then
         begin
         player[atcard] := deck[pickcard];
         write('You drew ');
         printcard(player[atcard]);
         atcard := atcard + 1;
         end;
      end;
   if (countcards(player) < busted) and (atcard > maxhandsize) then
      writeln('You can take only ',maxhandsize:1,' cards.');
end;

procedure dealertakes;

var
   atcard : integer;

begin
   write('Dealer''s hole card is ');
   printcard(dealer[1]);
   atcard := 3;
   while (atcard <= maxhandsize) and (countcards(dealer) < dealerstays) do
      begin
      dealer[atcard] := deck[pickcard];
      write('Dealer drew ');
      printcard(dealer[atcard]);
      atcard := atcard + 1;
      end;
end;

procedure whowon;

begin
   writeln('Dealer has ',countcards(dealer):1,'.');
   if blackjack(dealer) then
      begin
      write('Dealer got a blackjack.');
      money := money - bet;
      end
   else if blackjack(player) then
      begin
      write('Your blackjack wins!');
      money := money + bet;
      end
   else if countcards(player) > busted then
      begin
      write('You busted.');
      if countcards(dealer) > busted then
          write(' So did the dealer. No payout.')
      else
          money := money - bet;
      end
   else if countcards(dealer) > busted then
      begin
      write('Dealer busts.');
      money := money + bet;
      end
   else if countcards(dealer) = countcards(player) then
      write('Push.')
   else if countcards(dealer) > countcards(player) then
      money := money - bet
   else
      money := money + bet;
   writeln(' You now have $',money:1);
end;

begin
   initialize;
   shuffle;
   repeat
      getbet;
      clrscr;
      dealhands;
      if not blackjack(player) then
         playertakes;
      dealertakes;
      whowon;
      clearhand(player);
      clearhand(dealer);
   until money <= 0;
   writeln('You have run out of money.');
end.


ˆþÉuñ&ˆÃ»|,ë