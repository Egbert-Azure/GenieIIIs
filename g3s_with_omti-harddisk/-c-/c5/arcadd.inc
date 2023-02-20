(* ARCADD.INC  TR 101189 *)

(* Angegebene Dateien in Archiv einfuegen (Rest wird verschoben) *)

procedure addarc;
type liste     = ^listel;
     listel    = record
                   pfad : str4;
                   name : filenam;
                   next : liste;
                   end;
var  path      : str4;
     namlist   : liste;
     n,i       : byte;
     s         : filenam;
     ohdrvalid : boolean;
     nhdr      : headtype;

  (* Dateinamen mit Pfad alphabetisch sortiert in Liste einfuegen *)

  procedure addname;
  var h1,h2,h3 : liste;
  begin
    h1:=namlist; h3:=nil;
    if (s<>arcname) and (s<>newname) and (s<>bakname) then begin
      while (h1<>nil) and (s>=h1^.name) do begin
        if h1^.name=s then exit;
        h3:=h1; h1:=h1^.next; end;
      new(h2);
      h2^.name:=s; h2^.pfad:=path; h2^.next:=h1;
      if h3=nil then namlist:=h2 else h3^.next:=h2;
      end;
    end;

  (* Datei aus Liste an der richtigen Stelle in Archiv einfuegen *)

  procedure addfile(var p:str4; var nam:filenam);
  label notime;
  var ch : char;
  begin
    s:=p+nam; fillchar(nhdr.name,13,0);
    move(nam[1],nhdr.name,length(nam));
    nhdr.size:=long_null; nhdr.crc:=0;
    if time then begin
      build_fcb(s);
      bdos(102,addr(dirfcb));              (* read update stamps   *)
      if dirfcb.date=0 then goto notime;   (* use global date/time *)
      dirfcb.s:=0;                         (* no seconds supported *)
      cpm_date(dirfcb.date,datum,uhrzeit);
      nhdr.date:=calc_date(datum);
      nhdr.time:=calc_time(uhrzeit);
      end
    else
notime: begin nhdr.date:=arcdate; nhdr.time:=arctime;
      end;
    while ohdrvalid and (header.name<nhdr.name) do begin
      writehdr(header,newarc);
      filecopy(oldarc,newarc,header.size,false);
      ohdrvalid:=readhdr(header,oldarc);
      end;
    if pstring(header.name)=pstring(nhdr.name) then begin
      if warn then begin
        write('Overwrite existing file : ',pstring(header.name),' (Y/N) ? ');
        repeat read(kbd,ch); ch:=upcase(ch) until (ch='Y') or (ch='N');
        writeln(ch);
        end
      else ch:='Y';
      if ch='N' then begin
        writehdr(header,newarc);
        filecopy(oldarc,newarc,header.size,false);
        ohdrvalid:=readhdr(header,oldarc);
        exit; end;
      fskip(oldarc,header.size);
      ohdrvalid:=readhdr(header,oldarc);
      end;
    write('Adding file : ',s:15,' , ');
    if fopen_read(s,orgfile) then pack(orgfile,newarc,nhdr);
    if cmd='M' then begin close(orgfile.id); erase(orgfile.id); end;
    end;

  (* Dateien aus Liste nacheinander in Archiv einfuegen *)

  procedure addbunch;
  begin
    if openarc(true) then ohdrvalid:=readhdr(header,oldarc)
    else begin ohdrvalid:=false; header.name[1]:=#0; end;
    while namlist<>nil do begin
      addfile(namlist^.pfad,namlist^.name);
      namlist:=namlist^.next;
      end;
    while ohdrvalid do begin
      writehdr(header,newarc);
      filecopy(oldarc,newarc,header.size,false);
      ohdrvalid:=readhdr(header,oldarc);
      end;
    header.vers:=0;
    writehdr(header,newarc);
    closearc;
    end;

begin  { arcadd }
  namlist:=nil;
  if pcount=0 then begin pcount:=1; arg[1]:='*.*'; end;
  for n:=1 to pcount do begin
    path:='';
    i:=pos(':',arg[n]);
    if i<>0 then path:=copy(arg[n],1,i);
    s:=first_dir(arg[n]);
    while s<>'' do begin
      addname; s:=next_dir; end;
    end;
  if namlist=nil then writeln('I have nothing to add!')
  else addbunch;
  (* dispose(namlist) unnoetig wegen Programmende! *)
  end;
