program gratest;

procedure hrgon;
begin
inline    (     $3E/$01/            {       ld       a,1       }
                $06/$00/            {       ld       b,0       }
                $0E/$19/            {       ld       c,25      }
                $CD/*+4/            {       call     sys       }
                $C3/*+14/           {       jp       end       }
                $C5/                { sys   push     bc        }
                $DD/$2A/$01/$00/    {       ld       ix,(0001h)}
                $01/$57/$00/        {       ld       bc,87     }
                $DD/$09/            {       add      ix,bc     }
                $C1/                {       pop      bc        }
                $DD/$E9);           {       jp       (ix)      }
                                    { end   equ      $         }
end;


begin
writeln('Die Grafikseite soll eingeschaltet werden.');

hrgon;

writeln('Das wars schon !');
end.
