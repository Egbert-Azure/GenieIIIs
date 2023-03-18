program  eraeof;         (*-- Loe\scht die EOF Kennungen in Files, die zum   *)
                         (*-- Teil aus Quellcode und zum Teil aus Objectcode *)
                         (*-- bestehen. Anlaesslich der Konvertierung der    *)
                         (*-- 8 Zoll-Disketten von H.Sick                    *)


{$I BYTEFILE.BIB}

const eof    = $1a;
      ersatz = $00;

var   alt,cpm             :  bytefile;
      quelle,ziel         :  string[13];
      ptr,scan,i,zeichen  : byte;


procedure getsource;
begin
   repeat
   write('Quellfile mit EOFs : ');
   readln(quelle);
   for scan := 1 to length(quelle) do quelle[scan] := upcase(quelle[scan]);
   assignByteFile(alt,quelle);
   {$I-};          resetBYteFile(alt);        {$I+}
   until  ioresultByteFile = 0;
end;

begin
  if paramcount <> 0 then begin
     quelle  :=  paramstr(1);
     for scan := 1 to length(quelle) do
         quelle[scan] := upcase(quelle[scan]);
     assignByteFile(alt,quelle);
     {$I-};   resetBYteFile(alt);    {$i+};
     if ioresultByteFile <> 0 then begin
       writeln('kann ',quelle,' nicht oeffnen!');
     getsource;
     end
  end
  else getsource;

Ziel := '';  scan := 1;
repeat
  ziel := ziel + quelle[scan];
  scan := scan +1;
  if quelle[scan] = '.' then scan := length(quelle);
until scan = length(quelle);

ziel := ziel + '.NEW';
assignByteFile(cpm,ziel);
{$I-};    rewriteByteFile(cpm);   {$I+};
if ioresultByteFile <> 0 then begin
   writeln('kann ',Ziel,' nicht anlegen!');  halt;
end;

writeln;
writeln (#27,'R','Textkonvertierer loescht EOFs (1Ah)',#27,'S','                      (C) Volker Dose 1991');

repeat
   readByteFile(alt,zeichen);
   if (zeichen = eof) then begin
      zeichen := ersatz;
   end;
   writeByteFile(cpm,zeichen);
until eofByteFile(alt);

   closeByteFile(alt);     closeByteFile(cpm);
   writeln(Quelle,' in ',ziel,' konvertiert !');

end.