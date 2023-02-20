type      st255=string[255];

var       poslaenge: integer;

function  POSITION(suche,quelle: st255):integer;
const     maxteil=20;        {** Maximale Teilstrings in dem Suchbegriff **}
          max_wortlaenge=80; {** Maximale L„nge der Teilstrings ***********}
var       p1,p2,p3:    integer;
          teil:        array[1..maxteil] of string[max_wortlaenge];
          anfang,ende: integer;
          weiter:      boolean;
begin     for p1:=1 to length(suche) do suche[p1]:=upcase(suche[p1]);
          for p1:=1 to length(quelle) do quelle[p1]:=upcase(quelle[p1]);
          p2:=1; poslaenge:=0;
          if pos('*',suche)>0
             then begin
                     if not(suche[length(suche)]='*') then suche:=suche+'*';
                     while pos('*',suche)>0
                        do begin
                              teil[p2]:=copy(suche,1,pos('*',suche)-1);
                              delete(suche,1,pos('*',suche));
                              p2:=p2+1;
                           end;
                     p2:=p2-1;
                  end
             else begin
                     p2:=1;
                     teil[1]:=suche;
                  end;
          ende:=1; weiter:=true;
          anfang:=pos(teil[1],quelle);
          if anfang>0
             then begin
                     if p2>1
                        then begin p1:=1;
                                repeat
                                   p3:=pos(teil[p1],quelle);
                                   if (p3<ende) and (p3>0)
                                      then repeat
                                               quelle[p3]:='_';
                                               p3:=pos(teil[p1],quelle);
                                           until (p3=0) or (p3>anfang);
                                   if p3=0 then weiter:=false;
                                   if weiter then ende:=p3+length(teil[p1])-1;
                                   p1:=p1+1;
                                until (p1>p2) or (not weiter);
                             end
                        else begin weiter:=true;
                                ende:=anfang+length(teil[1])-1;
                             end;
                  end
             else weiter:=false;
          if weiter then begin POSITION:=anfang; poslaenge:=ende-anfang+1; end
                    else begin POSITION:=0; poslaenge:=0; end;
end; {* Position *}