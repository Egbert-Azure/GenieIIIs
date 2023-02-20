PROGRAM AuswahlDemo;

  (*$I c5:readchar.inc *)
  (*$I c5:AUSWAHL.BIB *)

  VAR Menue : ARRAY[1..160] OF STRING[10];
      MenueWahl : INTEGER;

  PROCEDURE InitialisiereFeld;
    VAR i : INTEGER;
    BEGIN
      FOR i:=1 TO 160 DO BEGIN
        str(i,Menue[i]);
        Menue[i]:='Punkt-'+Menue[i]
      END (* FOR *)
    END (* InitialisiereFeld *);

  BEGIN
    InitialisiereFeld;
    ClrScr; MenueWahl:=1; write('Bitte w{hlen Sie!');
    Auswahl(1,3,10,8,SizeOf(Menue[1]),Menue,160,MenueWahl);
    gotoxy(1,24);
    IF MenueWahl>0 THEN writeln('Ihre Wahl: ',Menue[MenueWahl]) ELSE
    IF MenueWahl=0 THEN writeln('Keine Entscheidung? ')
    ELSE writeln('Sie haben Fragen zu ',Menue[MenueWahl AND 255])
  END.