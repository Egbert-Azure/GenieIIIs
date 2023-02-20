{ MOUSE.PAS:  Misc. procedures for Pascal }
{ turbo pascal V2.0                       }
{ written: 9/88 by Howard Dutton          }

{**************************************************************************}
{ SetMouse(X,Y);                   | Sets current position of mouse        }
{ MouseX / MouseY                  | Current position of mouse             }
{ Mouse1 / Mouse2 / Mouse3         | Current status of mouse buttons       }
{             ---note: above 3 routines require mouse RSX---               }
{**************************************************************************}

PROCEDURE SetMouse(X,Y: INTEGER);
VAR
  L,MADD: INTEGER;
BEGIN
  MADD:=bdoshl(150);           { get address of mouse variable block }
  for L:=0 to 9 do             { clear variables }
    mem[Madd+L]:=0;
  for L:=0 to 1 do
  begin
    mem[Madd+L]:=mem[addr(X)+L];     { put in x address }
    mem[Madd+L+2]:=mem[addr(Y)+L];   { put in y address }
  end;
END;

FUNCTION _GMV(VNum: INTEGER): INTEGER;
VAR
  TEMP,MADD: INTEGER;
BEGIN
  MADD:=bdoshl(150);            { get address of mouse variable block }
  VNum:=(VNum-1)*2;             { compute offset }
  mem[Addr(TEMP)  ]:=mem[VNum+Madd];    { move the integer }
  mem[Addr(TEMP)+1]:=mem[VNum+Madd+1];
  _GMV:=TEMP;
END;

FUNCTION MouseX: INTEGER;
BEGIN
  MouseX:=_GMV(1);
END;
FUNCTION MouseY: INTEGER;
BEGIN
  MouseY:=_GMV(2);
END;
FUNCTION Mouse1: BOOLEAN;
BEGIN
  if _GMV(3)=0 then Mouse1:=FALSE else Mouse1:=TRUE;
END;
FUNCTION Mouse2: BOOLEAN;
BEGIN
  if _GMV(4)=0 then Mouse2:=FALSE else Mouse2:=TRUE;
END;
FUNCTION Mouse3: BOOLEAN;
BEGIN
  if _GMV(5)=0 then Mouse3:=FALSE else Mouse3:=TRUE;
END;
