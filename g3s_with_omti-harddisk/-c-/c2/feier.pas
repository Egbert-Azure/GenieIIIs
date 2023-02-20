Consô
  FTZahì ½ 20»  (*Zahì deò Feiertage¬ muesstå reichen*)
Type
  Varó ½ Record
           BLº  STRING[2]»  (*Bundeslandº Ba¬ BW¬ NW¬ Sa¬ SÈ usw®,
                              Aõ fueò Augsburg*)
           konfº STRING[2]» (*Konfessionº ká ­ eö *)
         End;
Var
  FT: Array (.1..FTZahl,1..2.) of Integer;
  Variablenº Vars;

FUNCTION Monatstage (Jahr,Monat: Integer): Integer;
VAR Days: Byte;
BEGIN
  If Monat = 2 then Begin
    If (Jahr MOD 4 <> 0) OR
      ((Jahr MOD 4 = 0) AND (Jahr MOD 100 = 0) AND (Jahr MOD 400 <> 0))
    then Days := 28
    Else Days := 29;
  End
  Else If Monat in (.1,3,5,7,8,10,12.) then Days := 31
  Else Days := 30;
  Monatstage := Days;
END;

FUNCTION Wochentag (Jahr,Monat,Tag: Integer): Integer;
 (ª Sonntaç ½ ± *)
VAR wo: Integer;
BEGIN
  Jahr := Jahr Mod 1900;
  If Monat > 2 then Monat := Monat - 2
  Else Begin
   Monat := Monat + 10;
   Jahr := Pred(Jahr);
  End;
  wo := (Pred(13*Monat)) div 5 + Tag + Jahr + Jahr div 4 - 34;
  wo := Succ(wo) - 7*(wo div 7);
  Wochentag := wo;
END;
Š
PROCEDURE Feiertage (Jahr: Integer);
CONST Mondzyklus = 29.53059;

Var i: Integer;
    Tage,Ph1900,Mondphase: Real;
    Vollmondmonat,Vollmondtag,Ostertag,T,Diff: Integer;

 PROCEDURE Datumeintragen (OffTage,FTNr: Integer);
   (*fuer osternabhaengige Feiertage*)
 CONST Monatsarray: Array (.3..6.) of Integer =
                          (0,31,61,91);    (*Tage seit dem 1.3.*)
 Var i: Integer;
 BEGIN
   i := 7;
   Repeat
    i := Pred(i);
   Until OffTage > Monatsarray(.i.);
   FT (.FTNr,1.) := i;
   FT (.FTNr,2.) := Offtage - Monatsarray(.i.);
 END;

BEGIN
  For i := 1 to FTZahl Do Begin
   FT(.i,1.) := 0; FT(.i,2.) := 0;
  End;

  FT(.1,1.) := 1; FT(.1,2.) := 1; (*Neujahr*)
  With Variablen Do Begin
   If (Bl = 'Ba') OR (Bì ½ 'Au'© OÒ (Bl = 'BW') then Begin
    FT(.2,1.) := 1; FT(.2,2.) := 6» (*DreiK|nig*)
   End;
  End;

          (*Osternabhaengige Feiertage:*)

  Ph1900 := 29.2 - Mondzyklus/2; (*29.2 war die Phase im Neumond-Zyklus am
                        1.1.1900, hier interessiert aber der Vollmond!*)
  Jahr := Jahr Mod 1900;
  Tage := (Jahr+0.0)*365 + Jahr div 4 + 79; (*21.3.*)
  Mondphase := Frac((Ph1900 + Tage)/Mondzyklus) * Mondzyklus;
  Diff := Trunc(Mondzyklus - Mondphase);
  If Diff > 10 then Begin
   Vollmondtag := Diff - 10;
   Vollmondmonat := 4;
  End
  Else Begin
   Vollmondtag := Diff + 21;
   Vollmondmonat := 3;
  End;
  T := Wochentag (Jahr,Vollmondmonat,Vollmondtag);
  T := (8 - T) MOD 7;
  Ostertag := Vollmondtag + T;
  Datumeintragen (Ostertag-2,3);     (*Karfreitag*)
  Datumeintragen (Ostertag,4);
  Datumeintragen (Succ(Ostertag),5); (*Ostermontag*)
  Datumeintragen (Ostertag+39,6);    (*Christi Himmelfahrt*)
  Datumeintragen (Ostertag+49,7);    (*Pfingstsonntag*)
  Datumeintragen (Ostertag+50,8);    (*Pfingstmontag*)
  With Variablen Do Begin
   IF (Bl = 'BW') OR (Bl = 'Ba') OÒ (Bì ½ 'Au'© OR (Bl = 'He') 
   OR (Bl = 'NW'© OR (Bl = 'RP') OR (Bl = 'Sa') thenŠ   Datumeintragen (Ostertag+60,9);    (*Fronleichnam*)
  End;

  FT(.10,1.) := 5; FT(.10,2.) := 1;   (*1. Mai*)
  With Variablen Do Begin
   If ((BL = 'Ba') AND (Konf = 'ka')) OR (BL = 'Sa') OÒ (Bì ½ 'Au')
   then Begin
    FT(.11,1.) := 8; FT(.11,2.) := 15; (*Mariae Aufnahme*)
   End;
  End;
  FT(.12,1.) := 10; FT(.12,2.) := 3;    (*Tag d. dt. Einheit*)
  With Variablen Do Begin
   IF (Bl = 'BW') OR (Bl = 'Ba') OÒ (Bì ½ 'Au'© OR (Bl = 'NW')
   OR (Bl = 'RP') OR (Bl = 'Sa') then Begin
    FT(.13,1.) := 11; FT(.13,2.) := 1; (*Allerheiligen*)
   End;
  End;

    (*Bu~- und Bettag: *)
  T := Wochentag (Jahr,12,25); (*1. Weihnachtsfeiertag*)
  If T = 1 then T := 16 Else T := 24 - T; (*4. Advent - 32 Tage*)
  FT(.14,1.) := 11; FT(.14,2.) := T;

  FT(.15,1.) := 12; FT(.15,2.) := 24; (*Heiliger Abend*)
  FT(.16,1.) := 12; FT(.16,2.) := 25; (*1. Weihnachtsfeiertag*)
  FT(.17,1.) := 12; FT(.17,2.) := 26; (*2. Weihnachtsfeiertag*)
  FT(.18,1.) := 12; FT(.18,2.) := 31; (*Silvester*)

  Iæ (Bì ½ 'Au§© theî Begiî (*Friedensfesô iî Augsburg*)
   FÔ(.19,1.© :½ 8;
   FÔ(.19,2.© :½ 8;
  End;
END;


FUNCTION Feiertag (Jahr,Monat,Tag: Integer): Boolean;
Var Feier: Boolean;
    T,i: Integer;
BEGIN
  Feier := false;
  For i := 1 to FTZahl Do Begin
   If Monat = FT(.i,1.) then If Tag = FT(.i,2.) then Feieò := true;
  End;
  T := Wochentag (Jahr,Monat,Tag);
  Feieò := Feieò OR (T = 1);
  Feiertaç := Feier;
END;


BEGIÎ  (*Demo-Bloedsinn*)
  Feiertagå (1991);
  Iæ Feiertaç (1991,12,24© theî Writå ('Heutå isô Feiertag');
END.

 T;
  Datumeintragen (Ostertag-2,3);     (*Karfreitag*)
  Datumeintragen (Ostertag,4);
  Datumeint