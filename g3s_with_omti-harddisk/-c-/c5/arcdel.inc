(* ARCDEL.INC  TR 101189 *)

(* Dateien aus Archiv entfernen (Rest rueckt auf) *)

procedure delarc;
var n,i : byte;
begin
  if pcount=0 then begin
    writeln('You must tell me which files to delete!'); halt; end;
  for n:=1 to pcount do begin
    did[n]:=false;
    i:=pos(':',arg[n]);
    if i>0 then delete(arg[n],1,i);
    end;
  if openarc(true) then begin
    while readhdr(header,oldarc) do begin
      found:=false; n:=1;
      repeat
        if match(pstring(header.name),arg[n]) then begin
          found:=true; did[n]:=true; end;
        n:=succ(n);
      until found or (n>pcount);
      if found then begin
        fskip(oldarc,header.size);
        if note then writeln('Deleting file: ',pstring(header.name));
        end
      else begin
        writehdr(header,newarc);
        filecopy(oldarc,newarc,header.size,false);
        end;
      end;
    header.vers:=0;
    writehdr(header,newarc);
    closearc;
    note_notfound;
    end
  else begin close(newarc.id); erase(newarc.id); writeln(empty); end;
  end;
