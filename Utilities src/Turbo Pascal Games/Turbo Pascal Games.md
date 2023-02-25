# Turbo Pascal Games

These  programs  were put together to help me learn  PASCAL.
They  function  fairly well as games and are  provided  for  your
amusement.  Although  intructions  are provided by each  program,
a brief description of each program is provided below.

        ARTILLRY.PAS
            This is a typical cannon shooting game.  You pick an angle to 
            fire the cannon at and,  if you get close enough, you hit the 
            enemy.

        BACCARAT.PAS
            Baccarat  is  a  card game popular in Europe.  It is  a  high 
            stakes gambling game favored by the rich (us poor types can't 
            afford it).  The player and the bank get two cards each. Tens 
            and  face cards count 0,  number cards count their value  and 
            aces  count  one.  Each  player totals his  cards  modulo  10 
            (anything over 10 is dropped) and the higher total  wins.  An 
            additional card may be added if desired. A total of 8 or 9 on 
            the  initial  two  cards  is  called  a  "NATURAL"  and  wins 
            immediately.  An example of counting; dealt an ace and a six, 
            the  player has a total of seven;  dealt a jack and a  three, 
            the  player has a total of three;  dealt a five and an eight, 
            the player has a total of three (thirteen minus ten).

        BJGAME.PAS
            This is the time honored game of blackjack.  This is a pretty 
            rudimentary  version  of the game as splitting  and  doubling 
            down are not allowed.  Blackjacks are honored,  but pay  only 
            even money. Blackjack pushes are won by the house. The dealer 
            plays  on even if you bust and thus can also bust.  This is a 
            push (no bet).

        BLACKBOX.PAS
            This  a game to guess where a hidden object (or objects)  are 
            in  a black box.  You do this by shooting balls into the  box 
            and  observing  where  they exit  (if  they  exit).  This  is 
            affected by the way the ball enetering approaches the waiting 
            object.  If  it  hits head on,  the ball is absorbed,  if  it 
            passed between two objects separated by one space,  the  ball 
            is  reflected  back to the source,  if it is adjacent  to  an 
            object on the edge, the ball is reflected back to the source, 
            if  it is diagonal to an object,  the ball is reflected at  a 
            right angle to the original path.

        BLOCK.PAS
            This is a block letter producer.

        BOGGLE.PAS
            This  is a version of the ever popular number  guessing  game 
            mastermind.

        CHUCKLCK.PAS
            This  is a dice rolling game.  You pick a number and bet that 
            at least one of three dice rolled will be that number.

        DODGE.PAS
            This  is a dodge'em type game where you are being pursued  by 
            various  meanies  and  your  object  is  to  avoid  them   or 
            preferrably destroy them.

        GUESSIT.PAS
            This  is the simpliest game,  guessing a number between 1 and 
            100. You are told if you are high or low.

        HANGMAN.PAS
            This a typical hangman program.  It has a list of words  (see 
            MAKEFILE.PAS),  selects one and then you guess the letters in 
            the word,  with seven incorrect guesses causing the man to be 
            hanged.

        HORSERAC.PAS
            This  is  a rather complicated horse race game.  A series  of 
            twelve  horses  start the race.  Each horse has  a  different 
            handicap which is a distance to run.  Two dice are rolled and 
            the  horse represented by the total of the dice may be  moved 
            one  length,  or  the horses represented by each die  may  be 
            moved one length.

        KENO.PAS
            This  is a version of the Las Vegas gambling game  KENO.  You 
            are  allowed to choose 8 numbers from 1 to 80.  The  computer 
            will  select 20 numbers in the same range.  You win based  on 
            how many of your numbers the computer also selected.

        KISMET.PAS
            This game is very similar to Yahtzee. Variations are added by 
            the introduction of color on the dice and an extra roll.  The 
            game ends after six turns by each player.

        LANDER.PAS
            This a game where you try to land on a planet.  The planet is 
            unspecified and changes each game.  Thus the gravity changes, 
            as  well as fuel supplies,  speed,  and starting  height.  In 
            addition  there are meteor swarms to contend with.  The  file 
            LANDINST.DAT contains instructions for this game.

        LANDINST.DAT
            The instructions for LANDER.PAS.

        MAKEFILE.PAS
            This  is the program to create the word file for HANGMAN.PAS. 
            It  must be run to create a word file before HANGMAN  can  be 
            used.

        NUMCNVRT.PAS
            This  program  lets you practice converting numbers from  one 
            base to another.

        OVERUNDR.PAS
            This is a dice betting game.  You pick a number from 2 to  12 
            and roll two dice.  You win (four to one) if the roll matches 
            the  number picked,  or (even money) if both the bet and  the 
            roll are over or under seven.

        PASCAL.PAS
            This program prints out Pascal triangles.

        PLIFE.PAS
             This is a version of LIFE,  the classic computer  game.  You 
            enter the coordinates of each starting cell until the initial 
            pattern  is set.  You can erase a set cell by re-entering the 
            cell's  coordinates.  Setting coordinates outside  the  basic 
            colony will end the input phase.  The colony will cycle until 
            one  of three things happen;  1) the colony dies out,  2) the 
            colony stabilizes (no births or deaths),  or 3) a key on  the 
            keyboard is pressed.

        SNAKE.PAS
            In  this game,  you try to get the treasure before the  snake 
            roaming the screen can get you. The snake will ignore you for 
            a while,  but will get REAL interested as you build up a cash 
            supply.  Your object is to get the cash and get out the door. 
            Use the U, N, H, and J keys to go up, down, left and right.

        TELEPHON.PAS
            This  program  generates  letter patterns  out  of  telephone 
            numbers.

        TWINKLE.PAS
            This  program fills the screen with stars then erases them in 
            interesting patterns.

        WUMPUS.PAS
            This is a typical game of Hunt the Wumpus.
