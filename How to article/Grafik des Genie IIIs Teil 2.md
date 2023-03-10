# Grafik des Genie IIIs unter Holte CP/M+ #
Volker Dose, Egbert Schröer, November 1993

Im ersten Beitrag zur kleinen Artikelserie über die Ansteuerung  der
Grafik des Genie IIIs unter CP/M wurde eine Möglichkeit in Assembler
vorgestellt.  Auch mit  Hochsprachen  wie  Turbo  Pascal  ist  dies
möglich, wenn man dazu die BDOS-Funktion 50 nutzt. Die entsprechende
Routine ist  ebenso im Include GRAFIK.INC  implementiert  wie  alle
Funktionen zum Zeichnen/Löschen von Punkten, Linien etc.

``` pas
(* INCLUDE-File GRAFIK.INC
   in das Programm einzufügen mit

   ($I GRAFIK.INC)

  Es folgen einige Unterroutinen, mit denen die HRG des
  Genie IIIs angesprochen werden kann. Es sind folgende
  Funktionen, bzw Proceduren implementiert:

 Procedure  Gra_On (seite)
 Procedure  Gra_Off(seite)
 Procedure  Gra_Cls(farbe)

 Procedure  Set_Point(xKoor,yKoor,seite)
 Procedure  Res_Point(xKoor,yKoor,seite)
 Function   Ask_Point(xKoor,yKoor,seite)
 Procedure  Hrg_Line(xStart,yStart,xEnd,yEnd,farbe,seite)

 Procedure  Set_Point_Normiert(xkoor,ykoor,seite)
 Procedure  Res_Point_Normiert(xkoor,ykoor,seite)
 Procedure  Hrg_Line_Normiert(xStart,yStart,xEnd,yEnd,farbe,seite)
 Procedure  Hrg_Circle(xKoor,yKoor,radius,farbe,seite)

  Bei den nicht normierten Proceduren gilt

 xKoor, xStart und xEnd  :  INTEGER im Bereich von 0-639
 yKoor, yStart und yEnd  :  INTEGER im Bereich von 0-275
                         :  (25 Zeilen * 11 Scanlinien)
 seite                   :  BYTE im Bereich 0-1
 farbe                   :  Byte 0 = schwarz   1 = weiss

  Bei den normierten Proceduren gilt abweichend

 yKoor, yStart und yEnd  INTEGER im Bereich von 0-449

!  ACHTUNG ! ACHTUNG ! ACHTUNG ! ACHTUNG ! ACHTUNG ! ACHTUNG   !
!    Es wird nicht ueberprueft, ob die uebergebenen Parameter  !
!    im oben angegebenen Breich liegen. Eventuell kann der     !
!    Rechner bei einigen Proceduren abstuerzen.                !
!  ACHTUNG ! ACHTUNG ! ACHTUNG ! ACHTUNG ! ACHTUNG ! ACHTUNG   !

*)
(* ------------------------------------------- *)
(* Direkter BIOS-Aufruf ueber BDOS-Funktion 50 *)
(* geklaut aus CPM-IBM.PAS aus der c't         *)

function UBIOS(fn,pa,pbc,pde,phl:integer):integer;
var biospb : record
               func,a   : byte;
               bc,de,hl : integer;
               end;
    result : integer;
begin
  with biospb do begin
    func:=fn; a:=pa;
    bc:=pbc; de:=pde; hl:=phl;
    end;

      result:=BDOS(50,addr(biospb));

    ubios:=result;
  end;

(*Schaltet eine Grafikseite ein*)

Procedure Gra_On (seite : byte);
var  sys0,sys1   :    byte;
begin
  sys0 := port[249];
   if (seite = 0) then   port[249] := sys0 and 239
                  else   port[249] := sys0 or   16;
  sys1 := port[250];
  port[250] := sys1 or 2;
end;

(*-------------------------------------------------------------*)

(*Schaltet eine Grafikseite aus*)

Procedure Gra_Off (seite : byte);
var  nixwert   :    integer;
begin
nixwert:=ubios(30,0,(seite*256)+25,0,0);
end;
(* --------------------------------------------------------------*)

(*Fuellt die Grafikseite mit FARBE*)

procedure Gra_Cls(seite,farbe : byte);
var  nixwert   :    integer;
begin
nixwert := UBIOS(30,farbe,(seite*256)+26,0,0);
end;
(* -------------------------------------------------------------- *)

(*Setzt einen nicht normierten Punkt auf SEITE*)

Procedure Set_Point(xkoor,ykoor : integer; seite : byte);
var  nixwert   :    integer;
begin
nixwert := ubios(30,1,(seite*256)+27,xkoor,ykoor);
end;
(* -------------------------------------------------------------- *)

(*Loescht einen nicht normierten Punkt auf SEITE*)

Procedure Res_Point(xkoor,ykoor : integer; seite : byte);
var  nixwert   :    integer;
begin
nixwert := UBIOS(30,0,(seite*256)+27,xkoor,ykoor);
end;
(* -------------------------------------------------------------- *)

(*Gibt 1 zurueck wenn der nicht normierte Punkt gesetzt ist*)

Function Ask_Point(xkoor,ykoor : integer;seite : byte) :  integer;
begin
ask_point := UBIOS(30,0,(seite*256)+28,xkoor,ykoor);
end;
(* -------------------------------------------------------------- *)

(*Zieht eine Linie auf SEITE mit FARBE*)

Procedure Hrg_Line(xstart,ystart,xend,yend : integer; farbe,seite : byte);
var  parablock : array[0..4] of integer;
     nixwert,z :                integer;

begin
parablock[1] := xstart;
parablock[2] := ystart;
parablock[3] := xend;
parablock[4] := yend;
z := addr(parablock[1]);
nixwert := UBIOS(30,farbe,(seite*256)+29,0,z);
end;
(* -------------------------------------------------------------- *)

(*Setzt normierten Punkt auf SEITE*)

Procedure Set_Point_Normiert(xkoor,ykoor : integer; seite : byte);
var  nixwert   :    integer;
begin
nixwert := ubios(30,17,(seite*256)+27,xkoor,ykoor);
end;
(* -------------------------------------------------------------- *)

(*Loescht normierten Punkt auf SEITE*)

Procedure Res_Point_Normiert(xkoor,ykoor : integer; seite : byte);
var  nixwert   :    integer;
begin
nixwert := UBIOS(30,16,(seite*256)+27,xkoor,ykoor);
end;
(* -------------------------------------------------------------- *)

(*Zeichnet normierte Linie mit FARBE auf SEITE*)

Procedure Hrg_Line_Normiert(xstart,ystart,
                            xend,yend : integer; farbe,seite :byte);
var  parablock : array[0..4] of integer;
     nixwert,z :                integer;
begin
parablock[1] := xstart;
parablock[2] := ystart;
parablock[3] := xend;
parablock[4] := yend;
z := addr(parablock[1]);
nixwert := UBIOS(30,16+farbe,(seite*256)+29,0,z);
end;
(* -------------------------------------------------------------- *)

(*Zeichnet einen Kreis auf SEITE mit FARBE*)

Procedure Hrg_Circle(xkoor,ykoor,radius : integer; farbe,seite : byte);
var parablock : array[1..3] of integer;
    nixwert,z :                integer;
begin
parablock[1] := xkoor;
parablock[2] := ykoor;
parablock[3] := radius;
z := addr(parablock[1]);
nixwert := UBIOS(30,16+farbe,(seite*256)+30,0,z);
end;
(* -------------------------------------------------------------- *)
```

Mit diesem Include File schafft man sich die Voraussetzungen zur
weiteren Programmierung. Nicht implementiert ist eine Funktion, die
eigentlich beim Genie IIIs unter Holte CP/M nie genutzt wurde -  die
Window-Funktion.  Window Technik unter CP/M ist ja möglich (es sei
nur an die CHIP Turbo Pascal Hefte erinnert), aber Speicherintensiv
und mühselig zu programmieren. Als Genie IIIs Besitzer muss man sich
darum nicht kümmern, denn  eine  Window  Funktion  ist  im   BIOS
implemtiert.  Wie das unter Turbo Pascal aussieht, zeigt der Include
File WINDOWG.BIB.

``` pas
{ ================================================================ }
{            WINDOWG.BIB                                           }
{ Bibliotheks-Modul fuer Windows unter Holte CP/M+                 }
{ zusaetzlich wird, wie bei WINDOW(7).BIB die Routine WINDOW.PAR   }
{ benoetigt. Es wurde versucht weitgehend die Syntax von           }
{ WINDOW.BIB einzuhalten.                                          }
{ ================================================================ }
{ Egbert Schroeer u. Volker Dose Januar bis Juni 1993              }
{ ================================================================ }

var     anystring          :      string[255];
        buffer             :      array[0..2000] of byte;
        TempScr            :      array[0..2000] of byte;
        i,j                :      byte;
        a1,b1,a2,b2,
        WindowBreite,
        WindowHoehe        : integer;

(*Direkter BIOS-Aufruf ueber BDOS-Funktion 50*)
(*wurde hier auch definiert wenn GRAFIK.INC*)
(*nicht ben|tigt*)

function UBIOS(fn,pa,pbc,pde,phl:integer):integer;
var biospb : record
               func,a   : byte;
               bc,de,hl : integer;
               end;
    result : integer;
begin
  with biospb do begin
    func:=fn; a:=pa;
    bc:=pbc; de:=pde; hl:=phl;
    end;
  result:=0;
  case fn of
    2,3,7,13..15,17..19,24 : result:=BDOS(50,addr(biospb));
    9,16,20,22,25          : result:=BDOSHL(50,addr(biospb));
    else                     BDOS(50,addr(biospb));
    end;
  ubios:=result;
  end;

procedure Save_Screen(zeiger_buffer : integer);
var   nixwert : integer;
begin
nixwert := ubios(30,0,22,0,zeiger_buffer);
end;

procedure Restore_Screen(zeiger_buffer : integer);
var nixwert : integer;
begin
  nixwert := ubios(30,1,22,0,zeiger_buffer);
end;

procedure Reduzier_Window;

begin
  a1:=a1+1;
  b1:=b1-1;
  a2:=a2+1;
  b2:=b2-1;
end;

(******************************************************************)
(*        OpenWindow - Oeffnen eines Fensters                     *)
(******************************************************************)
{ Procedure  OpenWindow
  eroeffnet auf dem Bildschirm einen Bereich mit neu definierten
  Bildfenstergroessen. Alle Bildschirmausgaben nach Aufruf
  dieser Funktion beziehen sich nur noch auf die neuen Bildschirm-
  groessen.                                                        }

procedure OpenWindow (Window_Number,top_line,bottom_line,
                      left_column,right_column : byte);

const   window_char  =      'F';   { Die uebergebenen Parameter }
        set_top      =      'I';   { muessen mit 32 addiert     }
        set_bottom   =      'J';   { werden, steht so im HOLTE  }
        set_left     =      'K';   { Handbuch CPM3.DOC.         }
        set_right    =      'L';   { Durch die Definition der   }
        escape       =      #27;   { Konstanten innerhalb der   }
                                   { Prozedur kann sie einfach  }
                                   { 'so' mit $I eingebunden    }
                                   { werden                     }
begin
  a1 := top_line;        { zunaechst Daten sichern zur
                         Initialisierung des eigentlichen Fensters }
  b1 := bottom_line;
  a2 := left_column;
  b2 := right_column;

  Cursor_Off;                { der stoert jetzt nur }

  { Breite und Hoehe des Fensters ermitteln         }
  { Davon 2 Zeichen fuer LinksOben, RechtsOben usw. }
  { abziehen                                        }

  WindowBreite := right_column - left_column-1;
  WindowHoehe  := bottom_line - top_line-1;

  { Fenster fuer Rahmen initialisieren }

  write(escape,window_char,chr(Window_Number+32));
  write(escape,set_top,chr(top_line+32));
  write(escape,set_bottom,chr(bottom_line+32));
  write(escape,set_left,chr(left_column+32));
  write(escape,set_right,chr(right_column+32));
  ClrScr;      { Fenster sauber machen  }
  
  { Rahmen zeichnen        }
  
  write(LinksOben);
  for i:=1 to WindowBreite do write(WaagrechtO);
  writeln(RechtsOben);
  for j := 1 to WindowHoehe do
    begin
      write(SenkrechtL);
      gotoxy(WindowBreite+2,j+1);
      writeln(SenkrechtR);
    end;
  write(LinksUnten);
  for i:=1 to WindowBreite do write(WaagrechtU);
  write(RechtsUnten);

{ Und nun die Fenstergroesse reduzieren um den
  Rahmen zu erhalten }

  Reduzier_Window;
  write(escape,window_char,chr(Window_Number+32));
  write(escape,set_top,chr(a1+32));
  write(escape,set_bottom,chr(b1+32));
  write(escape,set_left,chr(a2+32));
  write(escape,set_right,chr(b2+32));
  ClrScr;
  Cursor_On;       { den brauchen wir jetzt wieder }
end;

(*****************************************************************)
(*     ExitWindows - stellt urspruenglichen Bildschirminhalt her *)
(*****************************************************************)

procedure ExitWindow;
const   window_char  =      'F';   { Die uebergebenen Parameter }
        set_top      =      'I';   { muessen mit 32 addiert     }
        set_bottom   =      'J';   { werden, steht so im HOLTE  }
        set_left     =      'K';   { Handbuch CPM3.DOC.         }
        set_right    =      'L';   { Durch die Definition der   }
        escape       =      #27;   { Konstanten innerhalb der   }
                                   { Prozedur kann sie einfach  }
                                   { 'so' mit $I eingebunden    }
                                   { werden                     }
begin
  write(escape,window_char,chr(0+32));
  write(escape,set_top,chr(0+32));
  write(escape,set_bottom,chr(24+32));
  write(escape,set_left,chr(0+32));
  write(escape,set_right,chr(80+32));
  clrscr; (*alles sauber hinterlassen*)
  Restore_Screen(addr(buffer)); (*urspr. Anzeige restaurieren*)
end;

Nun besitzen wir zwei Include Files, die es uns erm|glichen alle  im
Holte BIOS implementierten Grafik-Funktionen auszunutzen. Als erstes
Beispiel  das Programm GRAFIK.PAS, das einfach einige  Aktionen  mit
der Grafik durchf}hrt:

program grafik;

var   fn,pa,pbc,pde,phl  :          integer;
      xkoor,ykoor        :          integer;
      dummi              :          integer;
      sys0,sys1          :             byte;
      radius,i           :          integer;
      taste              :             char;
      hugo               :          boolean;

{$I c5:grafik.inc}

procedure raumschiff(xkoor,ykoor : integer; farbe,seite : byte);
begin
  hrg_line(xkoor   ,ykoor,  xkoor+29,ykoor,  farbe,seite);
  hrg_line(xkoor+4 ,ykoor+1,xkoor+25,ykoor+1,farbe,seite);
  hrg_line(xkoor+4 ,ykoor-1,xkoor+25,ykoor-1,farbe,seite);
  hrg_line(xkoor+13,ykoor-2,xkoor+16,ykoor-2,farbe,seite);
  hrg_line(xkoor+11,ykoor+2,xkoor+12,ykoor+2,farbe,seite);
  hrg_line(xkoor+17,ykoor+2,xkoor+18,ykoor+2,farbe,seite);
  hrg_line(xkoor+12,ykoor+3,xkoor+12,ykoor+3,farbe,seite);
  hrg_line(xkoor+17,ykoor+3,xkoor+17,ykoor+3,farbe,seite);
  hrg_line(xkoor+13,ykoor+4,xkoor+16,ykoor+4,farbe,seite);
end;

begin                  (*Hauptprogramm*)
clrscr;

writeln('Testprogramm um die HRG einzuschalten ');

gotoxy(1,2);  clreol;
writeln('Jetzt wird die HRG eingeschaltet !');
  gra_cls(0,0);
  gra_cls(1,0);
  gra_on(0);
repeat until keypressed;

gotoxy(1,2);  clreol;
writeln('Jetzt werden Punkte gesetzt ! Und zwar normiert !');
  set_point_normiert(200,100,0);
  set_point_normiert(201,102,0);
  set_point_normiert(202,100,0);
  set_point_normiert(203,102,0);
  set_point_normiert(204,100,0);
  set_point_normiert(205,102,0);
  set_point_normiert(206,100,0);
  set_point_normiert(207,102,0);
  set_point_normiert(208,100,0);
  set_point_normiert(209,102,0);
repeat until keypressed;

gotoxy(1,2);  clreol;
writeln('Jetzt werden sie wieder gel|scht !');
  res_point_normiert(200,100,0);
  res_point_normiert(201,102,0);
  res_point_normiert(202,100,0);
  res_point_normiert(203,102,0);
  res_point_normiert(204,100,0);
  res_point_normiert(205,102,0);
  res_point_normiert(206,100,0);
  res_point_normiert(207,102,0);
  res_point_normiert(208,100,0);
  res_point_normiert(209,102,0);
repeat until keypressed;

gotoxy(1,2);  clreol;
writeln('Jetzt werden nicht normierte Punkte gesetzt.');
  set_point(638,174,0);
  set_point(639,174,0);
  set_point(638,175,0);
  set_point(639,175,0);

randomize;

for i := 0 to 500 do begin
                       xkoor := random(639);
                       ykoor := random(275);
                       set_point(xkoor,ykoor,0);
                     end;

repeat until keypressed;

gotoxy(1,2);  clreol;
write('Der normierte Punkt (639,174) ist ');

if ask_point(639,174,0)=1 then writeln('gesetzt.')
                          else writeln('nicht gesetzt.');

repeat until keypressed;

gotoxy(1,2);  clreol;
writeln('Und jetzt wird mit SetPoint eine Linie gezogen !');

for xkoor := 200 to 250 do
  for ykoor := 300 to 350 do set_point_normiert(xkoor,ykoor,0);

repeat until keypressed;

gotoxy(1,2);  clreol;
writeln('Und jetzt Linien mit HrgLine !!        Schneller !!');
  hrg_line_normiert(0,225,639,225,1,0);
  hrg_line_normiert(315,0,315,449,1,0);

  for xkoor:= 200 to 300 do hrg_line_normiert(xkoor,
                                              110,xkoor,210,1,0);

repeat until keypressed;

gotoxy(1,2);  clreol;
write   ('Jetzt noch einige Kreise !!');
writeln ('  Sie werden immer normiert gezeichnet.');
  hrg_circle(50,50,25,1,0);
  hrg_circle(100,50,25,1,0);
  hrg_circle(150,50,25,1,0);
  hrg_circle(50,100,25,1,0);
  hrg_circle(100,100,25,1,0);
  hrg_circle(150,100,25,1,0);
  hrg_circle(320,220,188,1,0);
repeat until keypressed;

gotoxy(1,2);  clreol;
writeln('Jetzt wird Seite 1 eingeschaltet !!');
  gra_cls(1,0);
  gra_on(1);
repeat until keypressed;

gotoxy(1,2);  clreol;
writeln('Wir loeschen den Schirm mit weiss !!');
  gra_cls(1,1);
repeat until keypressed;

gotoxy(1,2);   clreol;
writeln('Es folgen einige Grafikspielereien.');

randomize;

for i := 0 to 4000 do
    begin
      xkoor := 200+random(239);
      ykoor := 100+random(249);
      res_point_normiert(xkoor,ykoor,1);
    end;

for ykoor := 130 to 320 do hrg_line_normiert(230,ykoor,
                                             410,ykoor,1,1);

radius := 1;
while  radius < 60 do begin
                         radius := radius +3;
                         hrg_circle(160,225,radius,0,1);
                         hrg_circle(480,225,radius,0,1);
                         hrg_circle(320,225,radius,0,1);
                       end;

radius := 64;
while radius >1 do     begin
                         radius := radius  -3;
                         hrg_circle(160,225,radius,1,1);
                         hrg_circle(480,225,radius,1,1);
                         hrg_circle(320,225,radius,1,1);
                       end;

for i:= 1 to 100 do    begin
                         radius := 2 + random(20);
                         xkoor := 23 + random(600);
                         ykoor := 23 + random(410);
                         hrg_circle(xkoor,ykoor,radius,0,1);
                       end;

gra_cls(1,0);

randomize;

for i := 0 to 200 do begin  xkoor := random(639);
                            ykoor := random(274);
                            set_point(xkoor,ykoor,1);
                            end;

xkoor := 200;
ykoor := 100;

for i := 1 to 25 do begin xkoor := xkoor + 15;
                          ykoor := ykoor +  5;
                          raumschiff(xkoor,ykoor,1,1);
                          raumschiff(xkoor,ykoor,0,1);
                          end;

for i:= 1 to 70 do begin  ykoor := ykoor - 3;
                          raumschiff(xkoor,ykoor,1,1);
                          raumschiff(xkoor,ykoor,0,1);
                          end;

raumschiff(xkoor,ykoor,1,1);

gotoxy(1,2);  clreol;
writeln('Raumschiff bewegen mit Ziffernblock, ENDE mit <x>.');

hugo := true;

while ( hugo = true ) do begin
                           repeat until keypressed;
                           read(kbd,taste);
                           case taste of
                           '4' : begin
                                   raumschiff(xkoor,ykoor,0,1);
                                   xkoor:=xkoor-10;
                                   raumschiff(xkoor,ykoor,1,1);
                                 end;
                           '6' : begin
                                   raumschiff(xkoor,ykoor,0,1);
                                   xkoor := xkoor+10;
                                   raumschiff(xkoor,ykoor,1,1);
                                 end;
                           '8' : begin
                                   raumschiff(xkoor,ykoor,0,1);
                                   ykoor := ykoor +5;
                                   raumschiff(xkoor,ykoor,1,1);
                                 end;
                           '2' : begin
                                   raumschiff(xkoor,ykoor,0,1);
                                   ykoor := ykoor -5;
                                   raumschiff(xkoor,ykoor,1,1);
                                 end;
                           '7' : begin
                                   raumschiff(xkoor,ykoor,0,1);
                                   ykoor := ykoor + 5;
                                   xkoor := xkoor -10;
                                   raumschiff(xkoor,ykoor,1,1);
                                 end;
                           '1' : begin
                                   raumschiff(xkoor,ykoor,0,1);
                                   ykoor := ykoor - 5;
                                   xkoor := xkoor -10;
                                   raumschiff(xkoor,ykoor,1,1);
                                 end;
                           '9' : begin
                                   raumschiff(xkoor,ykoor,0,1);
                                   ykoor := ykoor + 5;
                                   xkoor := xkoor +10;
                                   raumschiff(xkoor,ykoor,1,1);
                                 end;
                           '3' : begin
                                   raumschiff(xkoor,ykoor,0,1);
                                   ykoor := ykoor - 5;
                                   xkoor := xkoor +10;
                                   raumschiff(xkoor,ykoor,1,1);
                                 end;
                           'x' : hugo := false;
                          end;
                         end;
gra_off(1);
gotoxy(1,2);
clreol;
writeln('Das wars dann.');
end.
```
Die maechtige Window-Funktion kann man beispielsweise im  MINITED.PAS
einsetzen,  einem kleinen Texteditor. Durch den Einsatz  der  Window
Technik  bekommt  er gleich ein sehr professionelles  Aussehen.  Auf
alle  anderen Include-Files wollen wir hier aus  Platzgr}nden  nicht
eingehen.   Es   sei   da   auf   die   im   Programmtext   genannte
Originalliteratur verwiesen.

``` pas
{ ============================================================= }
{ Diese Version von MiniTed ist nur fuer Genie 3s unter Holte   }
{ CP/M 3.0. Herr Holte hat ueber Bios Funktion 30 den Zugriff   }
{ auf Fenstertechnik eingebaut.                                 }
{ Um Verbreitung unter Genie 3s Benutzern wird ausdruecklich    }
{ gebeten.                                                      }
{ Cursor Position muesste noch gespeichert werden, um nach      }
{ restore_screen den Cursor an die "richtige" Position zu       }
{ bringen.                                                      }
{ Copyright: Die urspruengliche Version von MiniTed wurde in
             CHIP Turbo Pascal Special Ausgabe 9 veroeffentlicht.
             Von mir stammt nur die Erweiterung um die Fenster-
             technik. Ansprung des Bios und erste Gehversuche
             mit Holte CP/M und Window stammen von Volker Dose.
             Egbert Schroeer
             Joachimstrasse 18
             4270 Dorsten                                       }

PROGRAM MiniTed;

(*$I c5:CPM-80.BIB*)  {                                     }
(*$I c5:WINDOW.PAR*)  { hier Modifikationen fuer Zeichensatz}
(*$I c5:WINDOWG.BIB*)  { Window fuer Genie 3s                }
(*$I c5:WINDEFMI.INC*)  { Window Definitionen fuer TED        }
(*$I c5:READCHAR.INC*)  { Angepasste Tastatureingabe          }
(*$I c5:DYNSTR.BIB*)  { Dynamische Strings fuer Turbo-Pascal}
(*$I c5:TED-1.INC*)  { Deklarationen von TED               }
(*$I c5:TED-2.INC*)  { Der erste Teile von EditText        }
(*$I c5:CTRLQ.INC*)  { Das Ctrl-Q-Menue                    }
(*$I c5:CTRLK.INC*)  { Das Ctrl-K-Menue                    }
(*$I c5:TED-3.INC*)  { Der zweite Teil von EditText        }

procedure Logo;
  begin
      save_screen(addr(buffer));
      ClrScr; Write  ('    MINITED    fuer Genie 3s');
              Writeln('  Version 1.0 - '#152' ES (Juni 1993) ');
  end;

(*Hauptproramm*)

VAR T : TextListe;
    f : Text;
    X, Y, Z : INTEGER;
    c : char;
    s : string[080];

BEGIN (*MiniTed*)
  CBreak:=FALSE;
  IF ParamCount=0 THEN
    BEGIN
      Logo;
      SelWindow(1);
      ClrScr;
      Write(' Editfile : '); readln(s);
      assign(f,s);
      SelWindow(2);gotoxy(1,1); (*Textbereich oeffnen*)
      ClrScr; Cursor_On;
    end
  else assign(f,ParamStr(1));
  (*$I-*) reset(f); (*$I+*)
  IF IOResult=0 THEN BEGIN close(f);
                           LiesText(f,T) end ELSE NeuerText(T);
  X:=1; Y:=1; Z:=1; ClrScr; EditText(T,X,Y,Z,79,25,TRUE);
  SelWindow(1);
  gotoxy(1,1); Write('Speichern ? (J/N) '); clreol;
  repeat read(kbd,c); c:=upcase(c) until c in ['J','N'];
  write(c);
  if c='J' then SchreibText(f,T);
  LoescheText(T); ClrScr;
  ExitWindow;
  Cursor_On
end.  (*MiniTed*)
```

## Demnächst ##

Im nächsten  Teil  werden wir ein Beispiel in der  Sprache  ANSI  C
vorstellen.  Hier hat Alexander Schmidt - wie man an seinem  PCX-
Reader im letzten  Info  sehen  konnte  -  einiges  an   Vorarbeit
geleistet.
