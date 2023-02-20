PROGRAM Tabelle;

  { Programm erzeugt eine Tabelle mit horizontalen und vertikalen
    Linien aus einer normalen ASCII-Datei. }

CONST
  { Zeichendefinitionen an den Drucker, hier: EPSON FX-80 }
  ZDefLen = 169;
  ZDefDat : ARRAY [1..ZDefLen] OF byte
    = (27,58,0,0,0,                   { ROM-Zeichensatz kopieren    }
      27,38,0,45,45,                  { Zeichendefinitionen:        }
      138,8,0,8,0,8,0,8,0,8,0,8,      { "-"  waagrechte Linie       }
      27,38,0,33,33,
      138,0,0,0,0,255,0,0,0,0,0,0,    { "!"  lange senkrechte Linie }
      27,38,0,47,47,
      138,0,0,0,0,224,0,0,0,0,0,0,    { "/"  kurze senkrechte Linie }
      27,38,0,49,57,
      138,0,0,0,0,248,0,8,0,8,0,8,    { "1"  links unteres Eck      }
      138,8,0,8,0,248,0,8,0,8,0,8,    { "2"  Abzweigung nach oben   }
      138,8,0,8,0,248,0,0,0,0,0,0,    { "3"  rechts unteres Eck     }
      138,0,0,0,0,255,0,8,0,8,0,8,    { "4"  Abzweigung nach rechts }
      138,8,0,8,0,255,0,8,0,8,0,8,    { "5"  Kreuzung               }
      138,8,0,8,0,255,0,0,0,0,0,0,    { "6"  Abzweigung nach links  }
      138,0,0,0,0,15,0,8,0,8,0,8,     { "7"  links oberes Eck       }
      138,8,0,8,0,15,0,8,0,8,0,8,     { "8"  Abzweigung nach unten  }
      138,8,0,8,0,15,0,0,0,0,0,0);    { "9"  rechts oberes Eck      }

  { verschiedene Steuersequenzen (EPSON FX-80) }
  CR : char = ^M;                     { Carriage Return, Zeilenende }
  UniDir : string [2] = ^['<';        { drucke Zeile unidirektional }
  DModus : string [2] = ^['!';        { Druckmodus                  }
  LiRand : string [2] = ^['l';        { linker Rand                 }
  KlVorsch : string [3] = ^['J'^I;    { kleiner Vorschub (3 Nadeln) }
  GrVorsch : string [3] = ^['J'^X;    { grosser Vorschub (8 Nadeln) }
  RomZSatz : string [4] = ^['%'^@^@;  { Auswahl ROM-Zeichensatz     }
  UsrZSatz : string [4] = ^['%'^A^@;  { Auswahl User-Zeichensatz    }

  { String-Konstanten zur Umwandlung der Linealzeile je nach Zweck }
  UStrLen = 7;
  LinStr : string [UStrLen] = ' Ll-!Rr'; { Elemente der Linealzeile }
  AnfStr : string [UStrLen] = ' 77-899';    { Tabellenanfang  }
  MitStr : string [UStrLen] = ' 44-566';    { Tabellenmitte   }
  EndStr : string [UStrLen] = ' 11-233';    { Tabellenende    }
  KlAbst : string [UStrLen] = ' // ///';    { kleiner Abstand }
  GrAbst : string [UStrLen] = ' !! !!!';    { grosser Abstand }

  { Umwandlungsdaten bei Linealzeilenwechsel innerhalb der Tabelle }
  UebDaten : ARRAY [1..UStrLen,1..UStrLen] OF char
    = (' 77-899','1442555','1442555','-88-888',
       ' 442566','3552566','3552566');


TYPE
  Zeile  = string [127];
  Name   = string [40];
  UmsStr = string [UStrLen];
  VString = string [10];


VAR
  ein,aus : text;
  TabAktiv,DateiOK        : boolean;
  EinName,AusName         : Name;
  EinZeil,LinZeil,ErgZeil : Zeile;
  Teilung,ZeilNr,i,DM,LR  : integer;


PROCEDURE LinZeilKorr (VAR Z:Zeile);
  { Prozedur testet Linealzeile auf zugelassene Zeichen. Jedes
    falsche Zeichen wird durch einen Leerraum ersetzt. }
  VAR  i:integer;
  BEGIN
    FOR i:=1 TO length(Z) DO
      IF pos(Z[i],LinStr)=0 THEN Z[i]:=' ';
  END;


PROCEDURE Umsetzung (Um:UmsStr; InfoZeile:boolean; Vorschub:Vstring);
  { Prozedur erzeugt die Ergebniszeile ErgZeil aus der Linealzeile
    LinZeil entsprechend dem Umsetzstring Um und gibt sie an die
    Ausgabedatei aus. InfoZeile gibt an, ob auch die Eingabezeile
    mit ausgegeben wird. }
  VAR  i:integer;
  BEGIN
    ErgZeil:=LinZeil;
    FOR i:=1 TO length(LinZeil) DO
      ErgZeil[i]:=Um[pos(LinZeil[i],LinStr)];
    write (aus,UniDir,UsrZSatz,ErgZeil,RomZSatz,CR);
    IF InfoZeile THEN
      BEGIN
        write (aus,EinZeil,CR);
        ZeilNr:=succ(ZeilNr);
      END;
    write (aus,Vorschub);
  END;


PROCEDURE Uebergang;
  { Prozedur erzeugt die Ergebniszeile ErgZeile aus der alten Lineal-
    zeile LinZeil und einer neuen Linealzeile EinZeil. Am Schluss
    wird die alte Linealzeile durch die neue ersetzt und die Zeilen-
    nummer der automatischen Unterteilung auf Null gesetzt. }
  VAR
    i,j,len:integer;
  BEGIN
    len:=length(LinZeil);
    i:=length(EinZeil);
    WHILE length(EinZeil)<len DO EinZeil:=EinZeil+' ';
    WHILE length(LinZeil)<i DO LinZeil:=LinZeil+' ';
    IF len<i THEN len:=i;
    LinZeilKorr (EinZeil);
    ErgZeil:=LinZeil;
    FOR i:=1 TO len DO
      ErgZeil[i]:=UebDaten
        [pos(LinZeil[i],LinStr),pos(EinZeil[i],LinStr)];
    LinZeil:=EinZeil;
    write (aus,UniDir,UsrZSatz,ErgZeil,RomZSatz,CR,GrVorsch);
    ZeilNr:=0;
  END;


PROCEDURE IntEingabe (Frage:Zeile; VAR Wert:integer;
  min,max:integer);
  { Prozedur liest eine Integer-Variable von der Tastatur. }
  VAR
    ValCode : integer;
    Antwort : string [31];
  BEGIN
    REPEAT
      write (Frage);  readln (Antwort);
      val (Antwort,Wert,ValCode);
      IF (ValCode<>0) OR (Wert<min) OR (Wert>max) THEN
        BEGIN
          writeln ('Eingabe fehlerhaft...');
          writeln ('Geben Sie eine ganze Zahl im Bereich von ',
            min,' bis ',max,' an.');
          ValCode:=1;
        END;
    UNTIL ValCode=0;
  END;


BEGIN  { Hauptprogramm }
  writeln;
  writeln ('TAB - Programm zum Erzeugen einer gerahmten Tabelle');
  REPEAT
    writeln;
    write ('Name der Datei zum Bearbeiten: '); readln (EinName);
    write ('Name der Ergebnisdatei.......: ');

  IF pos('.',EinName)=0
     THEN BEGIN EinName:=EinName+'.tab';AusName:='' END
     ELSE read(AusName);
  IF AusName=''
     THEN BEGIN
          AusName:=copy(EinName,1,pos('.',EinName)-1)+'.EPS';
          writeln(AusName) END
     ELSE writeln;

    assign (ein,EinName);
    {$I-}  reset (ein)  {$I+};
    DateiOK:=(IOresult=0);
    IF NOT DateiOK THEN
      writeln ('Datei ',EinName,' nicht gefunden !');
  UNTIL DateiOK;

  writeln;
  IntEingabe ('Druckmodus der Tabelle...: ',DM,0,255);
  IntEingabe ('Linker Rand des Druckers : ',LR,0,255);
  IntEingabe ('Automatische Unterteilung: ',Teilung,1,MaxInt);
  writeln;
  write ('Programm arbeitet...');

  assign (aus,AusName);  rewrite (aus);
  FOR i:=1 TO ZDefLen DO write (aus,chr(ZDefDat[i]));
  write (aus,DModus,chr(DM));
  write (aus,LiRand,chr(LR));
  LinZeil:='';  TabAktiv:=false;

  WHILE NOT EOF(ein) DO BEGIN
    readln (ein,EinZeil);
    IF length(EinZeil)=0 THEN
      BEGIN
        IF TabAktiv THEN
          BEGIN
            { Leerzeile bewirkt Tabellenende }
            Umsetzung (EndStr,false,GrVorsch);
            LinZeil:='';  TabAktiv:=false;
          END
        ELSE
          writeln (aus);
      END
    ELSE  { EinZeil nicht leer }
      CASE EinZeil[1] OF
        'L','l' : IF TabAktiv THEN
                    Uebergang  { neue Linealzeile in lfd. Tabelle }
                  ELSE
                    BEGIN
                      LinZeil:=EinZeil;
                      LinZeilKorr (LinZeil);
                      TabAktiv:=true;  ZeilNr:=0;
                      Umsetzung (AnfStr,false,GrVorsch);
                    END;
        '+' : BEGIN
                Umsetzung (MitStr,false,GrVorsch);
                ZeilNr:=0;
              END;
        '/' : Umsetzung (KlAbst,false,KlVorsch);
        '!' : Umsetzung (GrAbst,false,GrVorsch);
        ELSE  { Datenzeile beginnt mit anderem Zeichen }
          IF ZeilNr=0 THEN
            Umsetzung (GrAbst,true,GrVorsch)
          ELSE  { ZeilNr > 0 }
            IF ZeilNr>=Teilung THEN
              BEGIN
                { automatische Unterteilung vor der Tabellenzeile }
                Umsetzung (MitStr,false,GrVorsch);
                ZeilNr:=0;
                Umsetzung (GrAbst,true,GrVorsch);
              END
            ELSE  { 0 < ZeilNr < Teilung }
              BEGIN
                { kleiner Abstand vor der Tabellenzeile
                  verhindert Aneinanderkleben der Zeilen }
                Umsetzung (KlAbst,false,KlVorsch);
                Umsetzung (GrAbst,true,GrVorsch);
              END;
      END;  { case }
  END;  { while }

  IF TabAktiv THEN
    Umsetzung (EndStr,false,GrVorsch);
  close (ein);
  close (aus);
  write ('  fertig.');
END.
