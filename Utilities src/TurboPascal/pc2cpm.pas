program  pc2cpm;         (*-- Konvertiert Textfiles mit IBM-8bit Zeichensatz *)
                         (*-- in 7bit ASCII wie bei CP/M. Aus Club 80 Info 34*)

{$I BYTEFILE.BIB}

const  umcode : array [0..127] of byte =
      ($43,$7d,$65,$61, $7b,$61,$61,$63, $65,$65,$65,$69, $69,$69,$5b,$65,
       $45,$7b,$5b,$7c, $7c,$6f,$75,$75, $79,$5c,$5d,$63, $20,$59,$20,$20,
       $61,$69,$6f,$75, $6e,$4e,$61,$6f, $20,$20,$20,$20, $20,$20,$3c,$3e,
       $20,$20,$20,$21, $21,$21,$21,$2b, $2b,$21,$21,$2b, $2b,$2b,$2b,$2b,
       $2b,$2d,$2d,$21, $2d,$2b,$21,$21, $2b,$2b,$2d,$2d, $21,$2d,$2b,$2d,
       $2d,$2d,$2d,$2b, $2b,$2b,$2b,$2b, $2b,$2b,$2b,$20, $20,$20,$20,$20,
       $61,$7e,$54,$20, $20,$20,$20,$20, $20,$20,$20,$20, $20,$20,$20,$20,
       $3d,$2b,$3e,$3c, $20,$20,$20,$20, $20,$20,$20,$20, $20,$20,$20,$20);
       (*  Position in der Tabelle ist PC-ASCII-Wert abz}glich 128.
           Wert in der Tabelle ist der entsprechende CPM ASCII Code
           F}r nicht }bertragbare Zeichen wird ein Blank erzeugt.   *)

var   pc,cpm              :  bytefile;
      quelle,ziel         :  string[13];
      ptr,scan,i,zeichen  : byte;

procedure getsource;
begin
   repeat
   write('PC Textfile : ');
   readln(quelle);
   for scan := 1 to length(quelle) do quelle[scan] := upcase(quelle[scan]);
   assignByteFile(pc,quelle);
   {$I-};          resetBYteFile(pc);        {$I+}
   until  ioresultByteFile = 0;
end;

begin
  if paramcount <> 0 then begin
     quelle  :=  paramstr(1);
     for scan := 1 to length(quelle) do
         quelle[scan] := upcase(quelle[scan]);
     assignByteFile(pc,quelle);
     {$I-};   resetBYteFile(pc);    {$i+};
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

ziel := ziel + '.CPM';
assignByteFile(cpm,ziel);
{$I-};    rewriteByteFile(cpm);   {$I+};
if ioresultByteFile <> 0 then begin
   writeln('kann ',Ziel,' nicht anlegen!');  halt;
end;

writeln;
writeln (#27,'R','PC => CP/M Textkonvertierer',#27,'S','                                 (C) H.Bernhardt 1991');

repeat
   readByteFile(pc,zeichen);
   if (zeichen and $80) <> 0 then begin
      ptr := (zeichen and $7f);
      zeichen := umcode[ptr];
   end;
   writeByteFile(cpm,zeichen);
until eofByteFile(pc);

   closeByteFile(pc);     closeByteFile(cpm);
   writeln(Quelle,' in ',ziel,' konvertiert !');

end.