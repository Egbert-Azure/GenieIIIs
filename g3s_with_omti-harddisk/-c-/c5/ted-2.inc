(****************************************************************************)
(*  TED-2.INC  Der erste Teile der Prozedur EditText f}r den Turbo-Editor   *)
(****************************************************************************)
PROCEDURE EditText(VAR T : TextListe; VAR X,Y,Z : INTEGER;
                   Spalten, ZeilenGanz : INTEGER; Status : Boolean);
  CONST MaxSpalten=255;
  VAR Zeile : StatString; (* Die Zeile, innerhalb der ediert wird. *)
      l : BYTE ABSOLUTE Zeile; (* Die ZeilenL{nge *)
      temp, Anfang : TextListe; (* Der Zeiger auf letzte Zeile *)
      c : CHAR; (* Das eingegebene Zeichen *)
      LR, RO : INTEGER; (* Linker und oberer Rand *)
      Zeilen : INTEGER; (* Die nutzbaren Zeilen *)

  CONST Buchstaben : SET OF CHAR = ['A'..'Z','a'..'z','0'..'9'];
        NaechsterBefehl : CHAR = #0;
        Mode : (InsertMode,OverwriteMode) = InsertMode;

  PROCEDURE AppendLine(VAR T : TextListe; Z : DynString);
    VAR temp : TextListe;
    BEGIN
      NEW(temp);
      WITH temp^ DO BEGIN
        Zeile:=Z; Rueck:=T;
        IF T<>NIL
        THEN BEGIN Vor:=T^.Vor; T^.Vor:=temp;
               IF Vor<>NIL THEN Vor^.Rueck:=temp
             END
        ELSE Vor:=NIL
      END; (* WITH *)
      T:=temp
    END; (* InsertLine *)

  PROCEDURE DeleteLine(VAR T : TextListe);
    BEGIN
      IF T<>NIL THEN BEGIN
        WITH T^ DO BEGIN
          ForgetDynString(Zeile);
          IF Vor<>NIL THEN Vor^.Rueck:=Rueck;
          IF Rueck<>NIL THEN Rueck^.Vor:=Vor;
          dispose(T); T:=NIL;
        END (* WITH *)
      END (* IF *)
    END; (* DeleteLine *)

  PROCEDURE StatusZeile;
    BEGIN
      gotoxy(1,1); LowVideo;
      write('   Line      Col     ');
      IF Mode=InsertMode THEN write('Insert Indent') ELSE Write('Overwrite');
      HighVideo
    END; (* StatusZeile *)

  PROCEDURE SchreibStatus;
    VAR ZahlStr : STRING[5]; zl : BYTE ABSOLUTE ZahlStr;
    BEGIN
      gotoxy(9,1); STR(Z,ZahlStr); write(Z,'':5-zl);
      gotoxy(18,1); STR(X,ZahlStr); write(X,'':3-zl)
    END; (* SchreibStatus *)

  PROCEDURE ChangeMode;
    BEGIN
      IF Mode=OverwriteMode THEN Mode:=InsertMode ELSE Mode:=OverwriteMode;
      IF Status THEN BEGIN
        LowVideo; gotoxy(22,1);
        IF Mode=OverwriteMode THEN write('Overwrite')
                              ELSE write('Insert Indent');
        ClrEol; HighVideo
      END (* IF Status *)
    END; (* ChangeMode *)

  PROCEDURE print(VAR S : StatString; X,Y : INTEGER);
    BEGIN
      gotoxy(X-LR,Y+RO);
      Write(copy(s,X,1+LR+Spalten-X)); ClrEol;
    END; (* print *)

  PROCEDURE ShowPage(T : TextListe; ab : INTEGER);
    VAR I : INTEGER;
    BEGIN
      FOR I:=ab TO succ(Zeilen) DO BEGIN
        IF NaechsterBefehl<>#0 THEN IF KeyPressed THEN
          BEGIN LiesZeichen(C); IF C=NaechsterBefehl THEN EXIT END;
        IF T<>NIL THEN BEGIN print(T^.Zeile^,1+LR,I); T:=T^.Vor END
                  ELSE BEGIN gotoxy(1,I+RO); ClrEol END
      END; c:=#0
    END; (* ShowPage *)

  PROCEDURE aktualisiere;
    VAR d : DynString; p : TextListe;
    BEGIN
      WHILE Zeile[l]=' ' DO l:=pred(l);
      IF Zeile=temp^.Zeile^ THEN EXIT;
      MakeDynString(Zeile,d); AppendLine(temp,d);
      p:=temp^.Rueck; IF T=p THEN T:=temp;
      DeleteLine(p);
    END; (* aktualisiere *)

  FUNCTION Top : TextListe;
    VAR i : INTEGER; p : TextListe;
    BEGIN
      p:=temp; FOR i:=2 TO Y DO p:=p^.Rueck; Top:=p
    END; (* Top *)

  PROCEDURE HochSchieben;
    VAR p : TextListe;
    BEGIN
      aktualisiere; p:=Top; NaechsterBefehl:=^Z; IF temp^.Vor=NIL THEN EXIT;
      REPEAT
        IF Y>1 THEN Y:=pred(Y)
        ELSE BEGIN Z:=succ(Z); temp:=temp^.Vor END;
        p:=p^.Vor; ShowPage(p,1)
      UNTIL (C=#0) OR (temp^.Vor=NIL);
      NaechsterBefehl:=#0; Zeile:=temp^.Zeile^; IF C<>#0 THEN ShowPage(p,1);
    END; (* HochSchieben *)

  PROCEDURE TiefSchieben;
    VAR p : TextListe;
    BEGIN
      aktualisiere; p:=Top; NaechsterBefehl:=^W; IF p=T THEN EXIT;
      REPEAT
        IF Y<Zeilen THEN Y:=succ(Y)
        ELSE BEGIN Z:=pred(Z); temp:=temp^.Rueck END;
        p:=p^.Rueck; ShowPage(p,1)
      UNTIL (C=#0) OR (p=T);
      NaechsterBefehl:=#0; Zeile:=temp^.Zeile^; IF C<>#0 THEN ShowPage(p,1);
    END; (* TiefSchieben *)

  PROCEDURE ZeileVor;
    VAR i : INTEGER; p : TextListe;
    BEGIN
      IF temp^.Vor=NIL THEN EXIT;
      NaechsterBefehl:=^X; aktualisiere; p:=Top;
      REPEAT
        IF Y=Zeilen THEN BEGIN
                           ShowPage(temp^.Vor,Y);
                           p:=p^.Vor; ShowPage(p,1)
                         END
                    ELSE BEGIN Y:=succ(Y); C:=#0 END;
        temp:=temp^.Vor; Z:=succ(Z);
      UNTIL (C=#0) OR (temp^.Vor=NIL);
      NaechsterBefehl:=#0; Zeile:=temp^.Zeile^; IF C<>#0 THEN ShowPage(p,1)
    END; (* ZeileVor *)

  PROCEDURE ZeileRueck;
    VAR p : TextListe;
    BEGIN
      IF temp^.Rueck=NIL THEN EXIT;
      NaechsterBefehl:=^E; aktualisiere; p:=Top;
      REPEAT
        IF Y=1 THEN BEGIN p:=p^.Rueck; ShowPage(p,1) END
               ELSE BEGIN Y:=pred(Y); C:=#0 END;
        temp:=temp^.Rueck; Z:=pred(Z);
      UNTIL (C=#0) OR (temp^.Rueck=NIL);
      NaechsterBefehl:=#0; Zeile:=temp^.Zeile^; IF C<>#0 THEN ShowPage(p,1)
    END; (* ZeileRueck *)

  PROCEDURE SeiteRueck;
    var i,j : integer;
        p : TextListe;
    begin
      IF Z=1 THEN EXIT;
      NaechsterBefehl:=^R; aktualisiere;
      REPEAT
        IF Z>Zeilen THEN i:=Zeilen ELSE i:=Z;
        Z:=Z-pred(i); FOR j:=2 TO i DO temp:=temp^.Rueck;
        IF Z<Y THEN Y:=Z;
        ShowPage(Top,1)
      UNTIL C<>^R;
      NaechsterBefehl:=#0; Zeile:=temp^.Zeile^
    end; (* SeiteRueck *)

  PROCEDURE SeiteVor;
    var i, j : integer;
        p : TextListe;
    begin
      IF temp^.Vor=NIL THEN EXIT;
      NaechsterBefehl:=^C; aktualisiere;
      REPEAT
        i:=0;
        WHILE (i<pred(Zeilen)) AND (temp^.Vor<>NIL) DO BEGIN
          temp:=temp^.Vor; i:=succ(i);
        END; Z:=Z+i; IF temp^.Vor=NIL THEN Y:=1;
        ShowPage(Top,1)
      UNTIL C<>^C;
      NaechsterBefehl:=#0; Zeile:=temp^.Zeile^
    end; (* SeiteVor *)

  PROCEDURE WortVor;
    BEGIN
      IF X>L THEN
        IF temp^.Vor=NIL THEN EXIT
        ELSE BEGIN ZeileVor; X:=1; IF Zeile[X] IN Buchstaben THEN EXIT END;
      WHILE (X<=L) AND (Zeile[X] IN Buchstaben) DO X:=succ(X);
      WHILE (X<=L) AND NOT(Zeile[X] IN Buchstaben) DO X:=succ(X);
    END; (* WortVor *)

  PROCEDURE WortRueck;
    BEGIN
      IF X>L THEN X:=succ(L);
      X:=pred(X);
      WHILE (X>0) AND NOT(Zeile[X] IN Buchstaben) DO X:=pred(X);
      IF X=0
        THEN BEGIN IF temp^.Rueck<>NIL THEN BEGIN ZeileRueck; X:=L END END
        ELSE WHILE (X>0) AND (Zeile[X] IN Buchstaben) DO X:=pred(X);
      X:=succ(X)
    END; (* WortRueck *)

  PROCEDURE AppendNextLine;
    VAR p : TextListe;
    BEGIN
      IF temp^.Vor=NIL THEN EXIT; (* Es gibt keine naechste Zeile *)
      p:=temp^.Vor;
      if L+length(p^.Zeile^)<256 then Zeile:=Zeile+p^.Zeile^
        else begin write(#7); EXIT end;
      print(Zeile,X,Y);
      DeleteLine(p); p:=temp^.Vor; ShowPage(p,Y+1)
    END; (* AppendNextLine *)

  PROCEDURE InsChar;
    BEGIN
      IF (l>=MaxSpalten) OR (X>=MaxSpalten) THEN BEGIN write(^G); EXIT END;
      while l<X-1 do Zeile:=Zeile+' ';
      IF Mode=InsertMode
      THEN BEGIN
        move(Zeile[X],Zeile[X+1],succ(l-X)); l:=succ(l);
        Zeile[X]:=C; print(Zeile,X,Y);
      END ELSE begin Zeile[X]:=C; write(C); if l<X THEN l:=x END;
      X:=succ(X)
    END; (* InsChar *)

  PROCEDURE DeleteChar;
    BEGIN
      delete(Zeile,X,1); print(Zeile,X,Y);
    END; (* DeleteChar *)

  PROCEDURE LoeschWort;
    VAR i : INTEGER;
    BEGIN
      IF X>l THEN BEGIN
        WHILE l<X-1 DO Zeile:=Zeile+' ';
        AppendNextLine; EXIT
      END;
      i:=X;
      IF Zeile[i] IN Buchstaben
        THEN WHILE (i<=l) AND (Zeile[i] IN Buchstaben) DO i:=succ(i)
        ELSE i:=succ(i);
      WHILE (i<=l) AND (Zeile[i]=' ') DO i:=succ(i);
      delete(Zeile,x,i-x); print(Zeile,X,Y)
    END; (* LoeschWort *)

  PROCEDURE ZeileLoeschen;
    VAR p, q : TextListe; i : integer;
    BEGIN
      IF temp^.Vor=NIL
      THEN BEGIN Zeile:=''; aktualisiere; print(Zeile,1+LR,Y) END
      ELSE BEGIN
             NaechsterBefehl:=^Y;
             WHILE (C=^Y) AND (temp^.Vor<>NIL) DO BEGIN
               p:=temp; q:=temp^.vor; DeleteLine(temp); temp:=q;
               ShowPage(temp,Y); IF T=p THEN T:=q
             END;
             NaechsterBefehl:=#0; IF C=^Y THEN ShowPage(Top,1)
           END;
      Zeile:=temp^.Zeile^; X:=1
    END; (* ZeileLoeschen *)

  PROCEDURE ZeileEinfuegen;
    VAR d : DynString; p : TextListe;
    BEGIN
      REPEAT
        MakeDynString(copy(Zeile,X,succ(l-X)),d);
        Zeile:=copy(Zeile,1,pred(X));
        p:=temp; AppendLine(p,d); aktualisiere; ShowPage(temp,Y)
      UNTIL C=#0; NaechsterBefehl:=#0;
    END; (* ZeileEinfuegen *)

  PROCEDURE NaechsteZeile;
    BEGIN
      IF Mode=InsertMode THEN ZeileEinfuegen;
      X:=1; ZeileVor;
    END; (* NaechsteZeile *)

  PROCEDURE InitEditor;
    VAR i : INTEGER;
    BEGIN
      LR:=0; HighVideo;
      IF Status THEN BEGIN RO:=1; Zeilen:=ZeilenGanz-2 END
                ELSE BEGIN RO:=0; Zeilen:=ZeilenGanz-1 END;
      IF T=NIL THEN BEGIN
        new(temp); T:=temp;
        with temp^ do begin Vor:=NIL; Rueck:=NIL; MakeDynString('',Zeile) END;
        X:=1; Y:=1; Z:=1
      END
      ELSE BEGIN
             IF X>=Spalten THEN LR:=succ(X-Spalten);
             temp:=T; i:=1;
             WHILE (temp^.Vor<>NIL) AND (i<Z) DO BEGIN
               i:=succ(i); temp:=temp^.Vor
             END;
             IF i<Z THEN Z:=i; IF Y>Z THEN Y:=Z;
             Anfang:=temp; for i:=2 TO Y DO Anfang:=Anfang^.Rueck;
             ShowPage(Anfang,1)
           END;
      Zeile:=temp^.Zeile^; IF Status THEN StatusZeile
    END; (* InitEditor *)