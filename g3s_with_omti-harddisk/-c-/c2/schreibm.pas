PROGRAM Schreibmaschine;

{Turbo 3.0: }
(*$I c5:WINDOW.PAR*)
(*$I c5:WINDOW7.BIB*)
(*$I c5:READEXT.BIB*)
(*$I c5:AUSWAHL.BIB*)

TYPE Zeile = string[80];
     MenueStr = string[15];

CONST LR : BYTE = 5;    {Linker Rand}
      RR : BYTE = 65;   {Rechter Rand}
      AktAbstand : INTEGER = 1;
      AktAusrichtung : INTEGER = 1;
      PicaOn  : BOOLEAN = TRUE;
      NLQOn   : BOOLEAN = FALSE;
      SSerifOn: BOOLEAN = FALSE;

      Reset   = #27#0;
      PEOff   = #27'8';                       {Papierende-Sensor deaktiviert}
      UniOn   = #27'U'#1;                        {unidirektionaler Druck ein}
      LMargin = #27#108;                                        {Linker Rand}
      RMargin = #27#81;                                        {Rechter Rand}
      Spacing = #27'A';                                      {Zeilen-Abstand}
      Format  = #27'a';                                  {Zeilenformatierung}
      NLQ     = #27'x';                         {Near-Letter-Quality Ein/Aus}
      SSerif  = #27'k';                          {Schriftart Roman/SansSerif}
      Pica    = #27'P';                {Schriftgroesse: Pica = 10 Zeichen/Zoll}
      Elite   = #27'M';               {Schriftgroesse: Elite = 12 Zeichen/Zoll}

      Einst : ARRAY[1..9] of MenueStr =
          ('Editieren','Raender','Zeilenabstand', 'Zeichengroesse',
           'Ausrichtung','Druckqualitaet','Zeichensatz','Seitenvorschub','Quit');
      Abstand : ARRAY[1..3] OF MenueStr =
          ('1-zeilig', '1,5-zeilig', '2-zeilig');
      Groesse : ARRAY[1..2] OF MenueStr = ('12 (Elite)','10 (Pica)');
      Quality : ARRAY[1..2] OF MenueStr = ('Draft','Near-Letter');
      NLQMode : ARRAY[1..2] OF MenueStr = ('Roman','Sans Serif');
      Ausrichtung : ARRAY[1..4] OF MenueStr =
          ('linksbuendig','zentriert','rechtsbuendig','Blocksatz');

VAR Z : Zeile; Ende : BOOLEAN; Wahl : INTEGER;

PROCEDURE BildSchirmAufbau;
  VAR i : INTEGER;
  BEGIN
    OpenWindow(1,1,80,4);     {Titelfenster}
    Write('       >>>>        Schreibmaschinen - ');
    Write('Simulationsprogramm         <<<<');
    OpenWindow(1,5,80,11);    {Einstellungen-Fenster}
    Gotoxy(18,5); Write('Bitte Drucker einschalten und dann RETURN druecken.');
    Readln; ClrScr;
    OpenWindow(1,12,80,20);   {Fenster mit gedruckten Zeilen}
    OpenWindow(1,21,80,24);   {Editierfenster}
    OpenWindow(30,11,50,21);  {Auswahlfenster}
  END;

PROCEDURE InitDrucker;
  (* Drucker initialisieren mit den voreingestellten Werten                *)
  BEGIN
    write(lst,Reset, PEOff, UniOn, LMargin, chr(LR), RMargin, chr(RR),
              Spacing, chr(succ(AktAbstand)*6),
              Format, chr(pred(AktAusrichtung)),
              NLQ, ord(NLQOn), SSerif, ord(SSerifOn));
    IF PicaOn THEN write(lst, Pica) ELSE write(lst,Elite);
  END;

PROCEDURE ShowEinstellungen;
  (* Aktuelle Druckereinstellungen in das Einstellungen-Fenster            *)
  BEGIN
    SelWindow(2); ClrScr;
    WriteLn('Linker Rand:   ',LR:11,
            ' ':25,'Ausrichtung: ',Ausrichtung[AktAusrichtung]);
    WriteLn('Rechter Rand:  ',RR:11,
            ' ':25,'Druckqualitaet: ',Quality[succ(ord(NLQOn))]);
    WriteLn('Zeilenabstand: ',Abstand[AktAbstand]:11,
            ' ':25,'Zeichensatz: ',NLQMode[succ(ord(SSerifOn))]);
    Write('Zeichengroesse: ',Groesse[succ(ord(PicaOn))]:11);
  END;

PROCEDURE Editiere;
  (* Zeile editieren, die gedruckt werden soll                             *)
  VAR Z : Zeile;
  BEGIN
     SelWindow(3); ClrScr; SelWindow(4);
     gotoxy(25,2); write('*** Ende EDIT mit <ESC> ***'); GotoXY(LR,1);
    REPEAT
      Z:=''; ReadStr(Z,RR-LR);
      SelWindow(3); gotoxy(LR,7); writeln(Z); SelWindow(4);
      GotoXy(LR,1); ClrEol; {WriteLn(Z); GotoXY(LR,1);}
      CASE LetztesZeichen OF
        #13 : writeln(lst,Z);
        ^J  : write(lst,^J);                     { Line-Feed }
        ^L  : write(lst,^L);                     { Form-Feed }
      END;
    UNTIL LetztesZeichen = #27; ClrScr;
  END;

BEGIN {Hauptprogramm}
  BildSchirmAufbau; InitDrucker; ShowEinstellungen; Ende:=false;
  EndeZeichen:=EndeZeichen+[^P,^J,^L];
  REPEAT
    SelWindow(5); Wahl:=1;
    Auswahl(1,1,15,1,SizeOf(MenueStr),Einst,9,Wahl);
    CASE Wahl OF
      1 : BEGIN
            CloseWindow; Editiere; OpenWindow(30,11,50,21);
          END;
      2 : BEGIN
            OpenWindow(31,14,49,17);
            REPEAT
              write('Links: '); ReadByte(LR,2); writeln;
              write('Rechts: '); ReadByte(RR,2);
              ClrScr;
            UNTIL (RR<80) AND (RR>LR) AND (LR>0);
            CloseWindow; write(lst,RMargin,chr(RR),LMargin,chr(LR));
          END;
      3 : BEGIN
            OpenWindow(31,15,49,19);
            Auswahl(1,1,15,1,SizeOf(MenueStr),Abstand,3,Wahl);
            IF Wahl<>0 THEN AktAbstand:=Wahl;
            CloseWindow; write(lst,Spacing, chr(succ(AktAbstand)*6));
          END;
      4 : BEGIN
            OpenWindow(31,16,49,19);
            Auswahl(1,1,15,1,SizeOf(MenueStr),Groesse,2,Wahl);
            CloseWindow; PicaOn:=Wahl=2;
            IF PicaOn THEN write(lst,Pica) ELSE write(lst,Elite)
          END;
      5 : BEGIN
            OpenWindow(31,17,49,22);
            Auswahl(1,1,15,1,SizeOf(MenueStr),Ausrichtung,4,Wahl);
            IF Wahl<>0 THEN AktAusrichtung:=Wahl;
            CloseWindow; write(lst,Format, chr(pred(AktAusrichtung)));
          END;
      6 : BEGIN
            OpenWindow(31,18,49,21);
            Auswahl(1,1,15,1,SizeOf(MenueStr),Quality,2,Wahl);
            CloseWindow; NLQOn:=Wahl=2; write(lst,NLQ,ord(NLQOn));
          END;
      7 : BEGIN
            OpenWindow(31,19,49,22);
            Auswahl(1,1,15,1,SizeOf(MenueStr),NLQMode,2,Wahl);
            CloseWindow; SSerifOn:=Wahl=2; write(lst,SSerif,ord(SSerifOn));
          END;
      8 : write(lst,^L);
      9 : Ende:=true
    END; (* case *)
    ShowEinstellungen
  UNTIL Ende;
  write(lst,#27#0); {Drucker Reset}
  while MaxScreen > 0 DO CloseWindow;
END.