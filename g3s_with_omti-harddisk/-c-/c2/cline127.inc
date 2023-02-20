(* CLINE127.INC  TR 300889 *)

(* NUR FUER TURBO-3.00!!! ABSOLUTE ADRESSIERUNG! CODE NICHT AENDERN!  *)
(* Universelles Modul fuer Turbo-3.00. Muss unbedingt als erstes      *)
(* Programmcode-Modul im Quelltext stehen! Am besten direkt hinter    *)
(* "PROGRAM xyz;" einfuegen, dann kann nichts passieren.              *)

(* Folgende Prozedur verlaengert die nutzbare CP/M-Kommandozeile auf  *)
(* volle 127 Zeichen. Wird dadurch aktiv, dass im fertigen COM-File   *)
(* wie folgt gepatcht wird: 20E2: 31 00 01 wird zu 20E2: C3 FF 20.    *)
(* Ohne Patchen steht die normale 32-Zeichen-Zeile zur Verfuegung.    *)
(* Zur Beachtung: Datenbereich muss vollstaendig in einer Page sein!  *)
(* Alles Mist, ich weiss, aber es geht wirklich nicht anders!         *)
(* Tilmann Reh, 5900 Siegen, 30. August 1989.                         *)

procedure Change_Commandline_To_Internal_String;
begin
  inline($31/$00/$01/$21/*+37/$22/$9E/$1F/$3E/$7F/$32/$A1/$1F/$32/$A7/$1F/
  $21/$54/$26/$22/$D7/$1F/$AF/$32/$D9/$1F/$21/$80/$00/$11/*+10/$01/$80/$00/
  $ED/$B0/$C3/$E5/$20/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/
  0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/
  0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/
  0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0/0);
  end;
