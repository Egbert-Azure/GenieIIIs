(****************************************************************************)
(*                       Screen Saver                                       *)
(****************************************************************************)
(*                 Egbert Schroeer Juli 1993                                *)
(****************************************************************************)

program Screen_save;

procedure LiesZeichen(var c: char); forward;

(*$I c5:CPM-80.BIB     *)
(*$I c5:WINDOW.PAR     *)
(*$I c5:WINDOW7.BIB    *)
(*$I c5:LAUF.BIB       *)

const
  SchriftZug = 'The - Screen - is - off  --  PAUSE for all Hackers';

var message : string[80];


procedure LiesZeichen;
  begin
    while not KeyPressed do Lauf;
    Read(kbd,c)
  end;

procedure LiesParameter;
  var c : char;
  begin
    repeat LiesZeichen(c) until c='c';
  end; (* LiesParameter *)

begin (* Screen Saver *)
  Cursor_Off;
  ClrScr;
  InitWindows;

{ if you prefer a window you can open one with
  OpenWindow(1,1,80,5); for example }

  Message := chr(151)+SchriftZug+chr(151);

{ InitLaufSchrift(Vertical Line,Horizontal Line,
                  Message,Length of shown message,
                  speed }

  InitLaufSchrift(15,5,Message,50,600);
  LiesParameter;
  ExitWindows;
  Cursor_On

end. (* Screen Saver *)