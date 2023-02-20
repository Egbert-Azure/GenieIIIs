PROCEDURE suche(VAR l : lab; x,y : integer);
  VAR z:integer;
      w: BOOLEAN;

  FUNCTION CheckFeld(X,Y : INTEGER):BOOLEAN;
    BEGIN
      CheckFeld:=FALSE;
      IF (X>=1) AND (X<=mx) AND (Y>=1) AND (Y<=my)
      THEN CheckFeld:=l[X,Y].feld IN [frei,Ende]
    END; (* CheckFeld *)

  FUNCTION CheckNr(X,Y,Z : INTEGER):BOOLEAN;
    BEGIN
      CheckNr:=FALSE;
      IF (X>=1) AND (X<=mx) AND (Y>=1) AND (Y<=my)
      THEN CheckNr:=l[X,Y].Nr=Z
    END; (* CheckNr *)

  PROCEDURE ZeigeWeg(X,Y : INTEGER);
    VAR min, r : INTEGER;
    BEGIN
      min:=MAXINT;
      REPEAT
        gotoxy(xa+pred(x),ya+pred(y)); write('*');
        IF y<my THEN IF l[x,y+1].Nr<min THEN BEGIN min:=l[x,y+1].Nr; r:=1 END;
        IF y>1  THEN IF l[x,y-1].Nr<min THEN BEGIN min:=l[x,y-1].Nr; r:=2 END;
        IF x<mx THEN IF l[x+1,y].Nr<min THEN BEGIN min:=l[x+1,y].Nr; r:=3 END;
        IF x>1  THEN IF l[x-1,y].Nr<min THEN BEGIN min:=l[x-1,y].Nr; r:=4 END;
        CASE r OF
          1 : y:=y+1;
          2 : y:=y-1;
          3 : x:=x+1;
          4 : x:=x-1
        END;
      UNTIL min=0; gotoxy(xa+pred(x),ya+pred(y)); write('*');
    END;

  BEGIN
    z:=0; l[x,y].Nr:=z;
    REPEAT (* geh, soweit die F}~e tragen *)
      gotoxy(xa+pred(x),ya+pred(y)); write('.');
      w:=FALSE;
      if l[x,y].feld = ende then BEGIN ZeigeWeg(x,y); exit END;
      if CheckFeld(x,y+1) then BEGIN y:=y+1; w:=TRUE END ELSE
      if CheckFeld(x,y-1) then BEGIN y:=y-1; w:=TRUE END ELSE
      if CheckFeld(x+1,y) then BEGIN x:=x+1; w:=TRUE END ELSE
      if CheckFeld(x-1,y) then BEGIN x:=x-1; w:=TRUE END;
      IF w THEN BEGIN
        IF l[x,y].feld=frei THEN l[x,y].feld:=belegt;
        z:=z+1;
        l[x,y].Nr:=z;
      END ELSE BEGIN (* geh zur}ck *)
        z:=z-1;
        if CheckNr(x,y+1,z) then y:=y+1 ELSE
        if CheckNr(x,y-1,z) then y:=y-1 ELSE
        if CheckNr(x+1,y,z) then x:=x+1 ELSE
        if CheckNr(x-1,y,z) then x:=x-1
      END
    UNTIL z<0;
  END;