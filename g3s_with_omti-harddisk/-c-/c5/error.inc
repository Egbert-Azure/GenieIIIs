procedure Errorproc(ErrNr,ErrAdr :integer);
  type string4 = string[4];
  var  i : integer;
       m : string[80];

  function hex(Zahl:integer):string4;
    const  HexZiffern : array [0..15] of char = '0123456789ABCDEF';
    begin
      hex:=HexZiffern[hi(Zahl) shr 4]+HexZiffern[hi(Zahl) and 15]+
           HexZiffern[lo(Zahl) shr 4]+HexZiffern[lo(Zahl) and 15];
    end; { hex }

    begin { Errorproc }
      writeln('**************   Fehler !   **************');
      write('Typ     : ',hi(ErrNr));
      case hi(ErrNr) of
        0: begin writeln(' CTRL-C Unterbruch'); m:=''; end;
        1: begin
             writeln(' I/O-Fehler');
             case lo(ErrNr) of
               $01: m:='Datei nicht vorhanden';
               $02: m:='Lesen aus Datei nicht moeglich';
               $03: m:='Ausgabe der Datei nicht moeglich';
               $04: m:='Datei nicht offen';
               $10: m:='Fehler im numerischen Format';
               $20: m:='Operation auf logischem Geraet nicht erlaubt';
               $21: m:='In Direktmodus nicht erlaubt';
               $22: m:='Zuordnung als Standard-Datei nicht zugelassen';
               $90: m:='Unpassende Recordlaenge';
               $91: m:='Suchen nach EOF';
               $99: m:='Unerwartetes EOF';
               $F0: m:='Disketten-Schreibfehler';
               $F1: m:='Directory ist voll';
               $f2: m:='Dateigroessenuueberschreitung';
               $ff: m:='Datei verschwunden'
               else m:='Kein definierter Fehler'
             end
           end;
        2: begin
             writeln(' Laufzeitfehler');
             case lo(ErrNr) of
               $01: m:='Gleitkommaueberlauf';
               $02: m:='Versuch durch Null zu dividieren';
               $03: m:='SQRT Argumentfehler';
               $04: m:='LN Argumentfehler';
               $10: m:='String-Laengenfehler';
               $11: m:='Ungueltiger Stringindex';
               $90: m:='Index ausserhalb des zulaessigen Bereichs';
               $91: m:='Skalar oder Teilbereich nicht zulaessig';
               $92: m:='Ausserhalb des integeren Bereichs';
               $ff: m:='Heap/Stackkollision'
               else m:='Kein definierter Fehler';
             end;
           end;
      end; { case }
      writeln('Nummer  : ',hex(lo(ErrNr)),m:length(m)+2);
      writeln('Adresse : ',hex(ErrAdr));
      halt(99); {DOS-Errorlevel 99}
    end;