(* ARCEXT.INC  TR 101189 *)

(* angegebene Datei(en) aus Archive extrahieren *)

procedure extarc;
var paths : array[1..10] of char;
    i,n   : byte;

  procedure extfile(path:char);
  var nam  : filenam;
      dt   : array[0..5] of byte;
  begin
    nam:=pstring(header.name);
    if path<>#0 then nam:=path+':'+nam;
    if note then writeln('Extracting File: ',nam);
    if CreateStamp then with header do begin
      dos_date(date,time,dt);
      calc_dt_cpm(dt);
      mem[TimeJumpAdr]:=$C9;
      move(dt[0],mem[SCBBase+$58],5);
      end;
    fopen_write(nam,orgfile);
    unpack(oldarc,orgfile,header);
    fclose(orgfile);
    if CreateStamp then mem[TimeJumpAdr]:=$C3;
    end;

begin  { extarc }
  for n:=1 to pcount do begin
    did[n]:=false;
    i:=pos(':',arg[n]);
    if i=0 then paths[n]:=#0 else begin
      paths[n]:=arg[n][1]; delete(arg[n],1,i); end;
    end;
  if openarc(false) then begin
    if pcount=0 then while readhdr(header,oldarc) do extfile(#0)
    else while readhdr(header,oldarc) do begin
      found:=false; n:=0;
      repeat
        n:=succ(n);
        if match(pstring(header.name),arg[n]) then begin
          found:=true; did[n]:=true; end;
      until found or (n>=pcount);
      if found then extfile(paths[n]) else fskip(oldarc,header.size);
      end;
    note_notfound;
    closearc;
    end
  else writeln(empty);
  end;
