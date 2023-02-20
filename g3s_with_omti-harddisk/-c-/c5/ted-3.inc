(****************************************************************************)
(*  TED-3.INC   Zweiter Teil der Prozedur EditText f}r den Turbo-Editor TED *)
(****************************************************************************)

  BEGIN (* EditText *)
    InitEditor;
    REPEAT
      IF Status THEN SchreibStatus;
      IF HeapAvail<1000 THEN BEGIN gotoxy(1,1); write(^G'SpeicherVoll!') END;
      IF X>Spalten+LR THEN BEGIN
        aktualisiere; LR:=X-Spalten; ShowPage(Top,1) END ELSE
      IF X<=LR THEN BEGIN aktualisiere; LR:=pred(X); ShowPage(Top,1) END;
      gotoxy(X-LR,Y+RO); LiesZeichen(C);
      CASE C OF
       #32..#255 : InsChar;
       #13 : NaechsteZeile;
       ^S : IF X>1 THEN X:=pred(X);
       ^D : IF X<MaxSpalten THEN X:=succ(X);
       ^X : ZeileVor;
       ^E : ZeileRueck;
       ^R : SeiteRueck;
       ^C : SeiteVor;
       ^F : WortVor;
       ^A : WortRueck;
       ^H : IF (X>1)
            THEN BEGIN X:=pred(X); gotoxy(X,Y); IF (X<=l) THEN DeleteChar END
            ELSE IF Z>1 THEN BEGIN ZeileRueck; X:=succ(l); AppendNextLine END;
       ^G : IF X<=l THEN DeleteChar
            ELSE BEGIN WHILE l<X-1 DO Zeile:=Zeile+' '; AppendNextLine END;
       ^T : LoeschWort;
       ^Y : ZeileLoeschen;
       ^W : TiefSchieben;
       ^Z : HochSchieben;
       ^Q : CtrlQ_Menue(FALSE);
       ^L : CtrlQ_Menue(TRUE);
       ^K : CtrlK_Menue;
       ^V : ChangeMode;
       ^I : IF X<pred(MaxSpalten-8) THEN X:=8*succ(X DIV 8);
       ^N : BEGIN NaechsterBefehl:=^N; ZeileEinfuegen END;
      END; (* CASE *)
    UNTIL C=#27;
    aktualisiere;
  END; (* EditText *)