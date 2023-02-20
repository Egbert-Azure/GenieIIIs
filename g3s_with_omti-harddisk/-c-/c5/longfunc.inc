(*         LongFunc.Inc      H”here Funktionen zu LongReal.Pas *)

const
 Leins:LongReal=(Exponent:Bias;Vorzeichen:false;    (* = 1              *)
        Mantisse:(1,0,0,0,0,0,0,0,0,0,0,0,0,0));
 Lhalb:LongReal=(Exponent:$7fff;Vorzeichen:false;   (* = 0.5            *)
        Mantisse:(128,0,0,0,0,0,0,0,0,0,0,0,0,0));
 Euler:LongReal=(Exponent:Bias;Vorzeichen:false;    (* = exp(1)         *)
        Mantisse:(2,183,225,81,98,138,237,42,106,191,113,88,128,157));
LongPi:LongReal=(Exponent:Bias;Vorzeichen:false;    (* = pi             *)
        Mantisse:(3,36,63,106,136,133,163,8,211,19,25,138,46,3));
 Sqrt2:LongReal=(Exponent:Bias;Vorzeichen:false;    (* = sqrt(2)        *)
        Mantisse:(1,106,9,230,103,243,188,201,8,178,251,19,102,235));
 LnW2 :LongReal=(Exponent:$7fff;Vorzeichen:false;   (* = ln(sqrt(2))    *)
        Mantisse:(88,185,11,251,232,231,188,213,228,241,217,204,1,249));
  LPi2:LongReal=(Exponent:Bias;Vorzeichen:false;    (* = pi/2           *)
        Mantisse:(1,146,31,181,68,66,209,132,105,137,140,197,23,2));

procedure WriteAll(var x:LongReal);      (* Zahl, Exponent und Mantisse *)
var i:integer;
begin
  writeln(convert(x,GueltigeZiffern));
  write('Exponent=',x.Exponent-Bias:3,' Mantisse=');
  for i:=1 to MantissenLaenge do write(x.Mantisse[i]:4);
  writeln;
end; (* WriteAll *)

procedure bmult(b:integer; var x,Result:LongReal);(* ByteMultiplikation *)
                                                  (* mit 1..256 entsp.0 *)
var c,i:integer;
begin
  c:=0;
  Result:=x;
  if (b and 255)=0 then begin
      Result.Exponent:=succ(Result.Exponent); exit end;
  for i:=MantissenLaenge downto 1 do begin
    c:=Result.Mantisse[i]*b+c;
    Result.Mantisse[i]:=lo(c);
    c:=hi(c) end;
  if c>0 then begin
    move(Result.Mantisse[1],Result.Mantisse[2],pred(MantissenLaenge));
    Result.Mantisse[1]:=c;
    Result.Exponent:=succ(Result.Exponent) end;
end; (* bmult *)

procedure bdivi(x:LongReal;b:integer;var Result:LongReal);(*ByteDivision*)
                                                          (* mit 1..256 *)
var c,i,j,q,m:integer;
begin
  Result:=x;
  c:=0;
  if (b and 255)=0 then begin
      Result.Exponent:=pred(Result.Exponent);
      exit end;
  if b>127 then begin
      q:=$7fff div b;
      m:=$7fff mod b end;
  with Result do begin
  if  Mantisse[1]<b then begin
      c:=Mantisse[1];
      Exponent:=pred(Exponent);
      move(Mantisse[2],Mantisse[1],pred(MantissenLaenge));
      Mantisse[MantissenLaenge]:=0 end;
  for i:=1 to MantissenLaenge do begin
    c:=swap(c)+Mantisse[i];
    if c>=0 then begin
        Mantisse[i]:=c div b;
        c:=c mod b end
      else begin
        Mantisse[i]:=q+succ(c and $7fff) div b;
        c:=m+succ(c and $7fff) mod b;
        if c>=b then begin
            c:=c-b;
            Mantisse[i]:=succ(Mantisse[i]) end;
      end; (* if *)
    end; (* for i *)
  if c>127 then
    if Mantisse[MantissenLaenge]<255 then
        Mantisse[MantissenLaenge]:=succ(Mantisse[MantissenLaenge]);
  end; (* with *)
end; (* bdivi *)


function testgleich(var a,n:LongReal):boolean;(* fr Iterationsabbruch *)
var i:integer;
begin
  for i:=MantissenLaenge downto 1 do
    if a.Mantisse[i]<>n.Mantisse[i] then begin
        a:=n;
        testgleich:=false; exit end;             (* alt wird neu *)
  testgleich:=true
end; (* testgleich *)

procedure LLOG(x:LongReal;var Result: LongReal);
var   h,p,z : integer;
      s1,s2 : LongReal;

    procedure log1 (x:LongReal;var Result:LongReal);
    var b,bq,alt : LongReal;    (* zw. .84 und 1.2 *)
    begin
      alt:=Leins;
      sub(x,Leins,b);
      add(x,Leins,Result);
      if b.Exponent=0 then begin
          MakeLong(0,Result);
          exit end;
      divi(b,Result,b);
      mult(b,b,bq);
      Result:=b;
      z:=3;
      repeat
        mult(b,bq,b);
        bdivi(b,z,x); z:=z+2;
        add(Result,x,Result);
      until testgleich(alt,Result);
    add(Result,Result,Result);
  end; (* log1 *)

begin
  if ((x.Exponent>-32725) and (x.Exponent<32727))
    or x.Vorzeichen then begin
      Result:=Leins;
      writeln('-!!-FC-error in LLOG-!!-');
      exit end;
  s1:=Lhalb; s2:=s1;
  s1.Mantisse[1]:=152;                          (* s1 ca 0.6  *)
  s2.Mantisse[1]:=216;                          (* s2 ca 0.84 *)
  h:=x.Exponent-Bias;
  x.Exponent:=Bias;                             (* 1<= x < 256 *)
  p:=1;
  while 1 shl p <= x.Mantisse[1] do p:=succ(p);
  bdivi(x,1 shl p,x);                           (* 0.5<= x < 1 *)
  z:=0;
  if not geq(x,s2) then begin
      if geq(x,s1) then begin
          mult(x,Sqrt2,x);
          z:=1 end
        else begin
          add(x,x,x);
          p:=pred(p) end
    end;
  MakeLong(2*(p+8*h)-z,Result);
  mult(Result,LnW2,Result);
  log1(x,x);
  add(Result,x,Result)
end; (* LLOG *)

procedure LEXP(x: LongReal; var Result: LongReal);
var  h       : integer;
     vflag   : boolean;

    procedure exp1(x: LongReal; var Result:LongReal);
    var b,alt : LongReal;    (* exp zw. -0.35 und .35 *)
        i     : integer;
    begin
      Result:=Leins;
      alt:=Leins;
      if x.Exponent=0 then exit;
      b:=Result;
      i:=1;
      repeat
        mult(b,x,b);
        bdivi(b,i,b);
        i:=succ(i);
        add(Result,b,Result);
      until testgleich(alt,Result);
    end; (* exp1 *)

begin
  vflag:=x.Vorzeichen;
  x.Vorzeichen:=false;
  divi(x,LnW2,x);
  h:=MakeInt(x);
  MakeLong(h,Result);
  sub(x,Result,Result);
  mult(Result,LnW2,Result);  (* 0 <= Result < .35  *)
  exp1(Result,Result);
  if odd(h) then mult(Result,Sqrt2,Result);
  h:=h shr 1;
  if h>0 then bmult( 1 shl (h and 7),Result,Result);
  h:=h shr 3;
  if h>0 then Result.Exponent:=Result.Exponent+h;
  if vflag then divi(Leins,Result,Result);
end; (* LEXP *)

procedure WURZ(x:LongReal;var Result:LongReal);
var y,alt:LongReal;
    i,j  :integer;
    minus:boolean;
begin
  Result:=x;
  if Result.Exponent=0 then exit;
  if Result.Vorzeichen then begin
      writeln('-!!-FC error in WURZ-!!-');
      Result:=Leins;
      exit end;
  if not geq(Result,Leins) then begin
      minus:=true;
      divi(Leins,Result,Result);
      x:=Result end
    else minus:=false;
  i:=Result.Exponent-Bias;
  Result.Exponent:=Bias;
  j:=1;
  while (1 shl j) <= Result.Mantisse[1] do j:=succ(j);
  bdivi(Result,1 shl j,Result);
  add(Result,Leins,Result);
  bdivi(Result,2,Result);
  i:=(i shl 3+j ) shr 1;
  if (i and 7) > 0 then bmult(1 shl(i and 7),Result,Result);
  Result.Exponent:=Result.Exponent+(i shr 3);
  if odd(j) then mult(Result,Sqrt2,Result);
  alt:=Leins;
  repeat
    divi(x,Result,y);
    add(Result,y,y);
    bdivi(y,2,Result);
  until testgleich(alt,Result);
  if minus then divi(Leins,Result,Result);
end; (* WURZ *)

procedure LARCTAN(x: LongReal; var Result: LongReal);
const
  at3764:                                    (* arctan(37/64) *)
  LongReal=(Exponent:$7fff;Vorzeichen:false;
  Mantisse:(134,48,162,218,218,30,208,101,211,232,78,213,1,61));
var so,z              : LongReal;
    tflag,vflag,gflag : boolean;

    procedure arctan1(x: LongReal; var Result: LongReal);
    var  xq,z,b : LongReal;    (*zw. 0 und ca 0.27*)
         i      : integer;
         minus  : boolean;
    begin
      Result:=x;
      z:=x;
      mult(x,x,xq);
      x:=Leins;
      i:=3;
      minus:=true;
      repeat
        mult(z,xq,z);
        bdivi(z,i,b);
        if minus then sub(Result,b,Result)
                 else add(Result,b,Result);
        minus:=not minus;
        i:=i+2;
      until testgleich(x,Result);
    end; (* arctan1 *)

begin
  Result:=x;
  if x.Exponent=0 then exit;
  so:=Lhalb;
  so.Mantisse[1]:=69; (* so ca. 0.27 *)
  vflag:=x.Vorzeichen;
  x.Vorzeichen:=false;
  if not geq(Leins,x) then begin
      divi(Leins,x,x);
      gflag:=true end
    else gflag:=false;
  if geq(x,so) then begin
      MakeLong(37,z);
      bdivi(z,64,z);
      sub(x,z,z);
      bdivi(x,64,x);
      bmult(37,x,x);
      add(Leins,x,x);
      divi(z,x,x);
      tflag:=true end
    else tflag:=false;
  arctan1(x,Result);
  if tflag then add(Result,at3764,Result);
  if gflag then begin
      bdivi(LongPi,2,x);
      sub(x,Result,Result) end;
  Result.Vorzeichen:=vflag;
end; (* LARCTAN *)

procedure LARCSIN(x:LongReal; var Result:LongReal);
begin
  mult(x,x,Result);
  if geq(Result,Leins) then
    if geq(Leins,Result) then begin
        Result:=LPi2;
        exit end
      else begin
        writeln('-!!-FC error in arcsin arccos-!!');
        exit end;
  sub(Leins,Result,Result);
  WURZ(Result,Result);
  divi(x,Result,Result);
  if (Result.Exponent<>0) and (Result.Exponent>-32725)
                          and (Result.Exponent<32727) then begin
      writeln('-!!-OV error in arcsin arccos-!!-');
      exit end;
  LARCTAN(Result,Result)
end; (* LARCSIN *)

procedure LARCCOS(x:LongReal; var Result:LongReal);
begin
      LARCSIN(x,Result);
      sub(LPi2,Result,Result)
end; (* LARCCOS *)

procedure LSIN(x:LongReal; var Result:LongReal);
var   l2pi      : LongReal;
      minus     : boolean;
      i         : integer;

    procedure sin1(x:LongReal; var Result:LongReal);
    var b,xq : LongReal;
        minus: boolean;
    begin
      mult(x,x,xq);
      Result:=x;
      b:=x;
      minus:=true;
      i:=2;
      x:=Leins;
      repeat
        mult(b,xq,b);
        bdivi(b,i,b); i:=succ(i);
        bdivi(b,i,b); i:=succ(i);
        if minus then sub(Result,b,Result)
                 else add(Result,b,Result);
        minus:=not minus;
      until testgleich(x,Result);
    end; (* sin1 *)

begin
  if x.Exponent=0 then begin Result:=x; EXIT end;
  minus:=x.Vorzeichen;
  x.Vorzeichen:=false;
  bmult(2,LongPi,l2pi);
  if not geq(l2pi,x) then begin
      divi(x,l2pi,Result);
      i:=MakeInt(Result);
      if i=maxint then begin
          writeln('-!!-OV error in sin/cos-!!-');
          exit end;
      MakeLong(i,Result);
      mult(Result,l2pi,Result);
      sub(x,Result,x) end;
  if geq(x,LongPi) then begin
      sub(l2pi,x,x);
      minus:=not minus end;
  if geq(x,LPi2) then sub(LongPi,x,x);
  sin1(x,Result);
  Result.Vorzeichen:=minus;
end; (* LSIN *)

procedure LCOS(x:LongReal; var Result:LongReal);
begin
  bdivi(LongPi,2,Result);
  sub(Result,x,x);
  LSIN(x,Result)
end; (* LCOS *)

procedure LTAN(x:LongReal; var Result:LongReal);
begin
  LSIN(x,Result);
  LCOS(x,x);
  divi(Result,x,Result)
end; (* LTAN *)