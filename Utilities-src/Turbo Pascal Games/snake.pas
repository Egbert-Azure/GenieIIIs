{SNAKE.PAS}

PROGRAM Snake;

{
Description: A "plain vanilla" version of the famous Snake game.  This
             version is intended to run on any computer.  The assumption
             is made that the computer has a standard numeric keypad, so
             the "2", "4", "6" and "8" keys can be used as direction keys.

Author:      Rene Ritter
Date:        1/84
Application: All systems
}

TYPE        direction = SET OF '2'..'8';      { For terminals with a
                                                separate numeric key pad }
            powerptr = ^power;
            power    = RECORD
                           next   : powerptr;
                           x, y   : BYTE;
                           life   : BYTE
                       END;
CONST       cours                 : direction = ['2','6','8','4'];

            { The above definition is for terminals with a separate numeric
              key pad.  Use 2 to move down, 8 for up, 4 for left, 6 for right.
              If you want to use other keys, you have to change the constant
              'cours' and the type 'direction'. }

VAR         playfield             : ARRAY[1..80,1..24] OF BYTE;
            flag                  : BOOLEAN;
            inp, dir, lastchar    : CHAR;
            again                 : CHAR;
            x, y, lastx, lasty    : BYTE;
            length, currlength    : BYTE;
            time, futter, energy  : BYTE;
            m, n, skill           : BYTE;
            del                   : INTEGER;

            points                : REAL;
            today                 : REAL;

            food,start            : powerptr;

{ --------------------------------------------------------------------------- }

PROCEDURE DrawFrame;

VAR  x : BYTE;

BEGIN
  CLRSCR;
  LOWVIDEO;
  FOR x := 1 TO 80 DO WRITE ('#');
  GOTOXY (1,24);
  FOR x := 1 TO 79 DO WRITE ('#');
  FOR x := 1 TO 23 DO
  BEGIN
    GOTOXY (1,x);
    WRITE ('#');
    GOTOXY (80,x);
    WRITE ('#');
  END;
  GOTOXY (27,24);
  WRITE ('Score :    0');
  GOTOXY (50,24);
  WRITE ('Energy: 3');
  HIGHVIDEO;
END; { DrawFrame }

{ --------------------------------------------------------------------------- }

PROCEDURE Initialize;
BEGIN
  FOR x := 1 TO 80 DO
    FOR y := 1 TO 24 DO playfield[x,y] := 0;
  DrawFrame;
  x := 40;
  y := 12;
  length := 0;
  lastx := 40;
  lasty := 12;
  currlength := 10;
  GOTOXY (x,y);
  WRITE ('0');
  lastchar := '.';
  flag := TRUE;
  dir := ' ';
  inp := ' ';
  m := 0;
  n := 0;
  RANDOMIZE;
  futter := 0;
  energy := 3;
  time := 0;
  points := 0;
  food := NIL;
  skill := 12;
  MARK (start)
END; { Initialize }

{ --------------------------------------------------------------------------- }

PROCEDURE Message (i:BYTE);
BEGIN
  GOTOXY (20,1);
  CASE i OF 1: WRITE ('You hit the wall!!');
            2: WRITE ('You bit yourself!!');
            3: write ('You died of hunger!!');
  END;
  flag := FALSE;
  inp := 'e';
END; { Message }

{ --------------------------------------------------------------------------- }

PROCEDURE DisplayScore;
BEGIN
  energy := energy+1+length div 80;
  GOTOXY (58,24);
  WRITE (energy:4);
  points := (13-skill) * energy+points+length div 50;
  GOTOXY (35,24);
  WRITE (points:5:0);
END; { DisplayScore }

{ --------------------------------------------------------------------------- }

PROCEDURE Timer;

VAR i : BYTE;

BEGIN
  time := time+1;
  IF time=240 THEN time := 0;
  IF  (time mod 48)=0 THEN BEGIN
    GOTOXY (58,24);
    energy := energy-1;
    WRITE (energy:4);
  END;
  IF energy=0 THEN Message (3);
  IF length=250 THEN BEGIN
    currlength := 5;
    skill := skill-1;
    GOTOXY (25,1);
    WRITE ('Skill Level:',12-skill:3);
  END;
END; { Timer }

{ --------------------------------------------------------------------------- }

PROCEDURE Producer;

VAR temp : powerptr;
    help : INTEGER;

{ --------------------------------------------------------------------------- }

PROCEDURE Trace;
VAR htemp : powerptr;

BEGIN
  temp := NIL;
  WHILE food<>NIL DO BEGIN
    htemp := food;
    food := food^.next;
    IF ( (htemp^.x<>n) OR (htemp^.y<>m) )
         AND (htemp^.life<>time) THEN BEGIN
      htemp^.next := temp;
      temp := htemp;
     END ELSE
      WITH htemp^DO BEGIN
        GOTOXY (x,y);
          IF playfield[x,y]=1 THEN WRITE (' ') ELSE WRITE ('.');
        playfield[x,y] := playfield[x,y]-1;
        futter := futter-1;
        IF futter=0 THEN release (start);
      END;
  END;
  food := temp;
END; { Trace }

BEGIN { Producer }
  IF  (futter=0) or (RANDOM>0.995) THEN BEGIN
    NEW (temp);
    futter := futter+1;
    WITH temp^ DO BEGIN
      help := (time+RANDOM(70)+80) mod 240;
      life := help;
      REPEAT
        x := RANDOM (78) +2;
        y := RANDOM (22) +2;
      UNTIL not ODD (playfield[x,y]);
      playfield[x,y] := playfield[x,y]+1;
      GOTOXY (x,y);
      WRITE ('*');
      next := food;
    END;
    food := temp;
  END;
  Trace;
END;

{ --------------------------------------------------------------------------- }

PROCEDURE Remove;

VAR help : BYTE;

BEGIN
  GOTOXY (lastx,lasty);
  length := length-1;
  write (' ');
  help := playfield[lastx,lasty];
  IF ODD (help)  THEN BEGIN
    help := help-1;
    GOTOXY (lastx,lasty);
    WRITE ('*');
  END;
  playfield[lastx,lasty] := playfield[lastx,lasty]-help;
  case help OF 2: lasty := lasty+1;
               4: lastx := lastx-1;
               6: lastx := lastx+1;
               8: lasty := lasty-1;
       END;
END;

{ --------------------------------------------------------------------------- }

FUNCTION move (c : CHAR) : BOOLEAN;
BEGIN
  GOTOXY (x,y);
  IF  (x<>80) or (y<>24) THEN WRITE (lastchar);
  playfield[x,y] := playfield[x,y]+ord (c) -ord ('0');
  case c OF '2' :y := y+1;
            '4' :x := x-1;
            '6' :x := x+1;
            '8' :y := y-1;
           END;
  GOTOXY (x,y);
  IF c in ['4','6'] THEN BEGIN
      lastchar := '-';
      del := 5;
  END ELSE BEGIN
      lastchar := '!';
      del := 10;
  END;
  length := length+1;
  WRITE ('0');
  IF playfield[x,y]>1 THEN move := FALSE ELSE move := TRUE;
  IF ODD (playfield[x,y])  THEN BEGIN
    currlength := currlength+5;
    DisplayScore;
    delay (del*15);
    n := x;
    m := y;
  END;
  WHILE length>currlength DO Remove;
END;

BEGIN { M A I N }
 today := 0;
 REPEAT
   Initialize;
   Producer;
     REPEAT
       IF keypressed THEN READ (kbd,inp);
       IF inp in cours THEN dir := inp;
       IF dir>' ' THEN BEGIN
         flag := move (dir);
         Producer;
         Timer;
         delay (skill*del);
       END;
       IF  (x=1) or (x=80) or (y=1) or (y=24)  THEN Message (1);
     UNTIL  (not flag);
     IF inp<> 'e' THEN Message (2);
     delay (2000);
     CLRSCR;
     WRITELN;
     HIGHVIDEO;
     WRITELN ('Your Score: ',points:10:0);
     LOWVIDEO;
     IF points>today THEN BEGIN
        WRITELN ('Champion of today!!');
        today := points;
     END;
     HIGHVIDEO;
     delay (3000);
     WRITE ('Play again? ');
     READ (kbd,again);
  UNTIL AGAIN IN ['n','N'];
END.
N BEGIN
                NumEntries := SUCC(NumEntries);
                NEW(DP