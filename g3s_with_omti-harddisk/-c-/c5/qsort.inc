{--------------------------------------------------------------------}
{                                                                    }
{  Q S O R T    Sortiert Arrays                                      }
{                                                                    }
{--------------------------------------------------------------------}

procedure QSort(Von,Bis: integer);
 type LiFo      = ^LiFoListe;
      LiFoListe = record
                   V,B  : integer;
                   Last : LiFo;
                  end;
 var  M         : ListenEintrag;
      I,J       : integer;
      Stack     : LiFo;

 procedure Push(Von,Bis : integer);
  var P : LiFo;
  begin
   New(P); P^.V:=Von; P^.B:=Bis; P^.Last:=Stack; Stack:=P;
  end;

 procedure Pop(Var Von,Bis : integer);
  var  P : LiFo;
  begin
   P:=Stack; Von:=P^.V; Bis:=P^.B; Stack:=P^.Last; Dispose(P);
  end;

 procedure Vertausche(Eintrag1,Eintrag2 : integer);
  var  Temp : ListenEintrag;
  begin
   Temp:= Liste[Eintrag1];
   Liste[Eintrag1]:= Liste[Eintrag2];
   Liste[Eintrag2]:= Temp;
  end;

 function Kleiner(X,Y: ListenEintrag): boolean;
  var  H : boolean;
  begin
   H := false;
   if X.Name<Y.Name then H := true
    else if X.Name=Y.Name then if X.DskNr<Y.DskNr then H := true
     else if X.DskNr=Y.DskNr then H:=X.User<Y.User;
   Kleiner := H;
  end;

 procedure ESort(Von,Bis : integer);
 var  I,J,M : integer;
 begin
  for I:=Von to Pred(Bis) do begin  M:=I;
   for J:=Succ(I) to Bis do if Kleiner(Liste[J],Liste[M]) then M:=J;
    if M<>I then Vertausche(I,M);
  end;
 end;

 begin  { QSort }
  Stack := Nil; Push(Von,Bis);
  repeat
   Pop(Von,Bis);
   if Bis-Von<7 then ESort(Von,Bis) else begin
   I:=Von; J:=Bis; M:=Liste[(I+J) div 2];
   repeat
    if Kleiner(Liste[I],M) then
     repeat I:=Succ(I) until (not Kleiner(Liste[I],M);
    if Kleiner(M,Liste[J]) then
     repeat J:=Pred(J) until (not Kleiner(M,Liste[J]);
    if I<=J then begin
     Vertausche(I,J); I:=Succ(I); J:=Pred(J);
    end;
   until I>J;
   if I<Bis then Push(I,Bis);
   if J>Von then Push(Von,J);
  end
  until Stack=Nil;
 end; { QSort }
