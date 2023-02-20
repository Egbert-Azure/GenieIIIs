(*
 * NamedDir - Herbert Oppmann 8/92
 * needs Z3Env.INC
 *)

TYPE
  FNameT = STRING[22];

(*
 * Converts filenames for later use with 'assign'.
 * Accepts DIR-form, DU-form, or plain filename.
 * Only a DIR-form is converted to its corresponding DU-form.
 * Be careful with the Turbo Pascal device names (e.g. 'LST:')!
 * If you want these names not to be tried as named directories,
 * remove the comments {+} that surround the comparison below.
 * Erroneous forms don't cause a warning or error message here.
 * They hopefully don't match a DIR entry and are thus left unchanged.
 *)
FUNCTION Dir2Du(fn : FNameT) : FNameT;
LABEL Thatsall;
VAR
  p, i, d, u : INTEGER;
  dir : STRING[8];
BEGIN
  {check that there is a ZCPR environment}
  IF (Env =NIL) THEN Goto Thatsall;
  p := Pos(':',fn);
  {if it is only a plain filename, leave it unchanged}
  IF (p =0) THEN Goto Thatsall;
  {extract DU/DIR}
  dir := Copy(Copy(fn,1,p-1)+'        ',1,8);
  i := 0;
  WHILE (i <Length(dir)) DO
    BEGIN
    dir[i] := UpCase(dir[i]);
    i := Succ(i);
    END;
  {except Turbo Pascal device names}
{+IF (dir = 'CON     ') OR (dir = 'TRM     ') OR
     (dir = 'KBD     ') OR (dir = 'LST     ') OR
     (dir = 'AUX     ') OR (dir = 'USR     ') THEN Goto Thatsall;+}
  WITH Env^ DO
    BEGIN
    i := 0;
    REPEAT
      {check if there is another entry in the list of named directories}
      IF (i =ndrs) OR (ndr^[i].drive =0) THEN Goto Thatsall;
      i := Succ(i);
    UNTIL (dir = ndr^[Pred(i)].name);
    {we found a DIR entry, get the corresponding DU}
    d := ndr^[Pred(i)].drive;
    u := ndr^[Pred(i)].user;
    Delete(fn,1,p-1);
    {cut off the DIR and add the DU}
    fn := Chr(d+$40)+Chr((u DIV 10)+48)+Chr((u MOD 10)+48)+fn;
    END; {WITH}
Thatsall:
  Dir2Du := fn;
END; {Dir2Du}
