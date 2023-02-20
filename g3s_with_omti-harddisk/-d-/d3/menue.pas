PROGRAM Menue;

VAR   Files         : array[0..64] of string[8];
      Name          : string[12];
      Anzahl        : byte;
      Wahl          : integer;
      Laenge        : byte;
      ch            : char;
      Programm      : file;
      Laufwerk      : byte;
      Wechsel       : boolean;
      i             : integer;

PROCEDURE Dir;

   VAR    Puffer        : array[0..127] of char;
          fcb           : array[0..31] of char;
          Zaehler,z     : byte;

  PROCEDURE Ordne;
    BEGIN
      Anzahl := succ (Anzahl);
      Zaehler := Anzahl;
      Files [Zaehler] := '';
      REPEAT
        IF Name < (Files[Zaehler-1]) THEN
        BEGIN
          Files[Zaehler] := Files[Zaehler-1];
          Zaehler := Zaehler-1;
          Files[Zaehler] := '';
        END ELSE
        BEGIN
          Files[Zaehler] := Name;
          Zaehler := 0;
        END;
      UNTIL Zaehler = 0;
    END;

   BEGIN
     Anzahl := 0;
     Files[0] := '';
     fcb := #0'???????????'#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0;
     BDOS (13);                     (* Diskettensystem zuruecksetzen *)
     BDOS (14,Laufwerk);            (* Bezugslaufwerk festlegen *)
     BDOS(26,addr(Puffer));         (* Datenuebergabe-Adresse *)
     z := BDOS(17,addr(fcb));       (* Ersten Eintrag holen *)
     WHILE z<255 DO                 (* solange fcb nicht leer *)
     BEGIN
       Name := copy (Puffer,z * 32+2,11);
       IF (copy (Name, 1, 2) <> 'D ')
          AND (copy (Name, 1, 6) <> 'MENUE ') THEN
       IF copy (Name, 9, 3) = 'COM' THEN
       BEGIN
         DELETE (Name, 9, 3);
         Laenge := pos (' ',Name);
         IF Laenge > 0 THEN
         Name := copy (Name,1,Laenge-1);
         Ordne;
       END;
       z  := BDOS(18,addr(fcb));    (* naechsten Eintrag holen *)
     END;
   END;

PROCEDURE Display (z : byte);

   VAR   x,y   : byte;

   BEGIN
     x := (z-1) mod 6 * 12 + 5;
     y := (z-1) div 6 + 5;
     GOTOXY (x,y);
     WRITE (Files [z]);
   END;

PROCEDURE Auswahl;

   PROCEDURE Beep;
     BEGIN
       WRITE (^G);
     END;

   BEGIN
     REPEAT
       NORMVIDEO;
       READ (kbd, ch);
       Display (Wahl);
       CASE ch OF
         #24 : IF Wahl > Anzahl - 6 THEN Beep
               ELSE BEGIN
                 Wahl := Wahl + 6;
               END;
         #19 : IF Wahl = 1 THEN Beep
               ELSE BEGIN
                 Wahl := Wahl - 1;
               END;
         #4 : IF Wahl = Anzahl  THEN Beep
               ELSE BEGIN
                 Wahl := Wahl + 1;
               END;
         #5 :  IF Wahl < 7 THEN Beep
               ELSE BEGIN
                 Wahl := Wahl - 6;
               END;
         'L','l'  : Wechsel := true;
       END;
       LOWVIDEO;
       Display (Wahl);
     UNTIL ch in [#13,#3,'L','l'];
  END;

PROCEDURE Laufwerkwechsel;

  BEGIN
    GOTOXY (5,22);
    NORMVIDEO;
    CLREOL;
    GOTOXY (18,23);
    LOWVIDEO;
    WRITE (' Laufwerk A, B, C oder D eingeben   ');
    NORMVIDEO;
    WRITE ('  ');
    REPEAT
      READ (kbd,ch);
      ch := UPCASE (ch);
    UNTIL ch IN ['A'..'D'];
    CASE ch OF
     'A' : Laufwerk := 0;
     'B' : Laufwerk := 1;
     'C' : Laufwerk := 2;
     'D' : Laufwerk := 3;
    END;
  END;

  BEGIN
    Wechsel := true;
    Laufwerk := BDOS(25);           (* Bezugslaufwerk ermitteln *);
    REPEAT
      Wechsel := false;
      CLRSCR;
      Dir;
      WRITELN ('ITT 3030  M E N U E - Programm':51);
      WRITELN ('(c) G.Bless  Vers. 1/87':48);
      WRITELN ('COM - Files  Laufwerk  ':47, chr(Laufwerk + 65));
      GOTOXY (6,22);
      WRITELN('Auswahl mit Cursor-Tasten,   Start mit RETURN,   Abbruch mit ^C');
      GOTOXY (22,23);
      WRITE ('Laufwerk / Diskettenwechsel mit  L  ');
      IF Anzahl = 0 THEN
      BEGIN
        Files[1] := '';
        GOTOXY(5,8);
        WRITE ('Kein "COM"- File auf der Diskette, ');
        WRITELN ('bitte Diskette oder Laufwerk wechseln');
        WRITE ('    und "L" druecken  ');
      END ELSE
      BEGIN
        LOWVIDEO;
        Display (1);
        NORMVIDEO;
        FOR i := 2 TO Anzahl  DO
          Display (i);
        Wahl := 1;
        GOTOXY (LENGTH (Files[1])+5,5);
      END;
      Auswahl;
      IF ch in ['L','l'] THEN Laufwerkwechsel;
    UNTIL Wechsel = false;
    IF ch = chr(13) THEN
    BEGIN
      Name := Files [Wahl];
      Name := Name + '.COM';
      CLRSCR;
      ASSIGN (Programm,Name);
      EXECUTE (Programm);
    END;
    CLRSCR;
 END.
