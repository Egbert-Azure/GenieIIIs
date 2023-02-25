PROGRAM Drucker;    (*   Druckereinstellung  Vers. 2.1  (c) G.Bless  1/87
                  Auswahl von Schriftart, Zeichensatz und Randeinstellung
                  ------------------------------------------------------- *)

   VAR    ObRand, UntRand        : INTEGER;
          LiRand,s,a,x           : INTEGER;
          Wahl,n                 : CHAR;
          Zeichens,Schrift,Art   : STRING[15];

  PROCEDURE Auswahl;

    PROCEDURE Clear;

      VAR  i   : INTEGER;

      BEGIN
        FOR i := 15 TO 24 DO
        BEGIN
          GOTOXY(1,i);CLREOL;
        END;
      END;

    PROCEDURE Zeichensatz;
      BEGIN
        GOTOXY(1,20); CLREOL;
        GOTOXY(6,16);
        WRITELN ('*** Zeichensatzauswahl ***':40);
        WRITELN;
        WRITELN ('        0  >  USA             4  >  Daenemark');
        WRITELN ('        1  >  Frankreich      5  >  Schweden');
        WRITELN ('        2  >  Deutschland     6  >  Italien');
        WRITELN ('        3  >  England         7  >  Spanien');
        GOTOXY(12,23);
        WRITE ('Bitte Auswahl treffen :  ');
        REPEAT READ(kbd,n)
        UNTIL n IN ['0'..'7'];
        CASE n OF
          '0' : Zeichens := 'USA';
          '1' : Zeichens := 'Frankreich';
          '2' : Zeichens := 'Deutschland';
          '3' : Zeichens := 'England';
          '4' : Zeichens := 'Daenemark';
          '5' : Zeichens := 'Schweden';
          '6' : Zeichens := 'Italien';
          '7' : Zeichens := 'Spanien';
        END;
        Clear;
      END;

    PROCEDURE Schriftart;
      BEGIN
        GOTOXY(1,20); CLREOL;
        GOTOXY(6,16);
        WRITELN ('***  Schriftauswahl  ***     ':40);
        WRITELN;
        WRITELN ('      1  >  Pica');
        WRITELN ('      2  >  Elite');
        WRITELN ('      3  >  Schmalschrift');
        WRITELN ('      4  >  NLQ');
        GOTOXY (12,23);
        WRITE ('Bitte Auswahl treffen :  ');
        REPEAT READ(kbd,Wahl)
        UNTIL Wahl IN ['1'..'4'];
        s := (ORD(Wahl)-48);
        CASE s OF
          1 : BEGIN
                Schrift := 'Pica';
                x := 80;
              END;
          2 : BEGIN
                Schrift := 'Elite';
                x := 96;
              END;
          3 : BEGIN
                Schrift := 'Schmalschrift';
                x := 136;
              END;
          4 : BEGIN
                Schrift := 'NLQ';
                x := 80;
                a := 1;
              END;
        END;
        Clear;
      END;

    PROCEDURE Druck;
      BEGIN
        GOTOXY(1,20); CLREOL;
        GOTOXY(6,16);
        WRITELN ('***  Druckauswahl  ***     ':40);
        WRITELN;
        WRITELN ('      1  >  Normal');
        WRITELN ('      2  >  Fettddruck');
        WRITELN ('      3  >  kursiv');
        WRITELN ('      4  >  proportional');
        GOTOXY(12,23);
        WRITE ('Bitte Auswahl treffen :  ');
        REPEAT READ(kbd,Wahl)
        UNTIL Wahl IN ['1'..'4'];
        a := ORD(Wahl)-48;
        CASE a OF
          1 : Art := 'Normal';
          2 : Art := 'Fettdruck';
          3 : Art := 'kursiv';
          4 : Art := 'proportional';
        END;
        Clear;
      END;

BEGIN
  WRITELN ('        ***  Druckereinstellung Vers. 2.1  (c) G.Bless  1/87 ***');
  WRITELN ('---------------------------------------------------------------------');
  WRITELN;
  WRITELN ('A u s w a h l :':25,'Eingestellt ist:':30);
  WRITELN;
  WRITELN ('        1  >  Oberer Rand :');
  WRITELN ('        2  >  Unterer Rand :');
  WRITELN ('        3  >  Linker Rand :');
  WRITELN ('        4  >  Schriftart :');
  WRITELN ('        5  >  Druckart :');
  WRITELN ('        6  >  Zeichensatz :');
  WRITELN;
  WRITE ('        0  >  Druckereinstellung');CLREOL;
  REPEAT
      GOTOXY(39,6); WRITE (ObRand :2,'  Zeilen');
      GOTOXY(39,7); WRITE (UntRand :2,'  Zeilen');
      GOTOXY(39,8); WRITE (LiRand :2,'  Zeichen');
      GOTOXY(40,9); WRITE (Schrift,'      ');
      IF s = 4 THEN Art := 'Normal';
      GOTOXY(40,10);WRITE (Art,'          ');
      GOTOXY(40,11);WRITE (Zeichens,'        ');
      GOTOXY(2,22);
      WRITE   ('Die Einstellung ergibt max ',x- LiRand, ' Zeichen pro Zeile');
      WRITE (' und ', 72 - ObRand - UntRand,' Zeilen pro Seite');
      GOTOXY (8,15);WRITE(' Q  >  Abbruch ');
      GOTOXY (6,17);WRITE ('Bitte Auswahl treffen :  ');CLREOL;
      REPEAT READ(kbd,Wahl)
      UNTIL Wahl IN ['0'..'6','q','Q'];
      Clear;
      CASE Wahl OF
        '1' : BEGIN
                GOTOXY (6,17);
                WRITE ('Bitte oberen Rand eingeben, ( dann RETURN ) :  ');
                REPEAT READ (ObRand)
                UNTIL (ObRand >=0) AND (ObRand < 72);
              END;
        '2' : BEGIN
                GOTOXY (6,17);
                WRITE ('Bitte unteren Rand eingeben, ( dann RETURN ) :  ');
                REPEAT READ (UntRand)
                UNTIL (UntRand >=0) AND (UntRand < 72);
              END;
        '3' : BEGIN
                GOTOXY (6,17);
                WRITE ('Bitte linken Rand eingeben ( dann RETURN ) :    ');
                REPEAT READ (LiRand)
                UNTIL (LiRand >=0) AND (LiRand < 80);
                END;
        '4' : Schriftart;
        '5' : Druck;
        '6' : Zeichensatz;
      END;
      UNTIL Wahl IN ['0','q','Q'];
    END;

PROCEDURE Init;
  BEGIN
    CLRSCR;
    GOTOXY (30,10);
    WRITE ('Bitte Drucker einschalten');
    WRITE (LST,CHR(27),CHR(64));                      (* Grundeinstellung *)
    WRITE (LST,CHR(27),CHR(82),CHR(ORD(ObRand)));     (* oben *)
    WRITE (LST,CHR(27),CHR(78),CHR(ORD(UntRand)));    (* unten *)
    WRITE (LST,CHR(27),CHR(77),CHR(ORD(LiRand)));     (* links *)
    WRITE (LST,CHR(27),CHR(66),CHR(s));               (* Schrift *)
    IF s < 4 THEN
    BEGIN
      CASE a OF                                       (* Art *)
        2 :WRITE (LST,CHR(27),CHR(71));               (* Fettdruck *)
        3 :WRITE (LST,CHR(27),CHR(52));               (* kursiv *)
        4 :WRITE (LST,CHR(27),CHR(112),CHR(1));       (* proportional *)
      END;
    END;
    WRITE (LST,CHR(27),CHR(55),CHR(ORD(n)-48));          (* Zeichensatz *)
    WRITE (LST,CHR(27),CHR(61));                      (* L|schen Bit 8 *)
    GOTOXY (30,10);
    WRITE ('F E R T I G');CLREOL;
  END;

BEGIN  { Hauptprogramm }
  CLRSCR;
  ObRAND   := 3;
  UntRand  := 5;
  LiRand   := 8;
  Art := 'Normal';
  a := 1;
  Schrift  := 'Elite';
  x := 96;
  s := 2;
  Zeichens := 'Deutschland';
  n := '2';
  Auswahl;
  IF Wahl = '0' THEN Init;
END.
