program grafik;

var   fn,pa,pbc,pde,phl  :          integer;
      xkoor,ykoor        :          integer;
      dummi              :          integer;
      sys0,sys1          :             byte;
      radius,i           :          integer;
      taste              :             char;
      hugo               :          boolean;

{$I grafik.inc}

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



begin                  (* Hauptprogramm  ------------------------------------*)
clrscr;

writeln('Testprogramm um die HRG einzuschalten ');

gotoxy(1,2);  clreol;
writeln('Jetzt wird die HRG eingeschaltet !');
  gra_cls(0,0);
  gra_cls(1,0);
  gra_on(0);
repeat until keypressed;


gotoxy(1,2);  clreol;
writeln('Jetzt werden einige Punkte gesetzt ! Und zwar normiert !');
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
writeln('Und jetzt Linien mit HrgLine !!           Schneller !!');
  hrg_line_normiert(0,225,639,225,1,0);
  hrg_line_normiert(315,0,315,449,1,0);

  for xkoor:= 200 to 300 do hrg_line_normiert(xkoor,110,xkoor,210,1,0);

repeat until keypressed;


gotoxy(1,2);  clreol;
writeln('Jetzt noch einige Kreise !! Sie werden   immer normiert gezeichnet.');
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


for ykoor := 130 to 320 do hrg_line_normiert(230,ykoor,410,ykoor,1,1);

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
