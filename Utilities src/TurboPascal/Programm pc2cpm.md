# Programm pc2cpm

Das Programm `pc2cpm` konvertiert Textdateien mit dem IBM-8bit-Zeichensatz in 7-Bit-ASCII-Code wie bei CP/M. Der Programmcode stammt aus Club 80 Info 34.

## Funktionsweise ##
Das Programm arbeitet mit zwei Byte-Dateien: einer Quell- und einer Ziel-Datei. Die Quell-Datei wird als Parameter beim Programmstart übergeben oder vom Benutzer per Eingabeaufforderung eingegeben. Die Ziel-Datei wird automatisch erzeugt und erhält den gleichen Namen wie die Quell-Datei, jedoch mit der Endung ".CPM".

Das Programm liest die Quell-Datei zeichenweise ein und überprüft, ob das jeweilige Zeichen im IBM-8bit Zeichensatz enthalten ist (d.h. ob das höchstwertige Bit gesetzt ist). Ist dies der Fall, wird das Zeichen in einen 7bit ASCII Code konvertiert, der dem entsprechenden Zeichen in CP/M entspricht. Die Umwandlung erfolgt mithilfe einer vorgegebenen Tabelle, die im Programmcode als Konstante "umcode" definiert ist.

Das konvertierte Zeichen wird in die Ziel-Datei geschrieben. Am Ende wird die Quell- und Ziel-Datei geschlossen und eine Erfolgsmeldung ausgegeben.

## Verwendete Bibliotheken

Das Programm verwendet die Bibliothek `BYTEFILE.BIB`.

## Konstanten

Die Konstante `umcode` ist ein Array, das eine Tabelle mit den ASCII-Codes für den IBM-8bit-Zeichensatz enthält.

## Variablen

Das Programm verwendet die folgenden Variablen:

- `pc`: Eine Byte-Datei-Variable, die die Quelldatei repräsentiert.
- `cpm`: Eine Byte-Datei-Variable, die die Zieldatei repräsentiert.
- `quelle`: Eine Zeichenketten-Variable, die den Namen der Quelldatei enthält.
- `ziel`: Eine Zeichenketten-Variable, die den Namen der Zieldatei enthält.
- `ptr`, `scan`, `i`, `zeichen`: Byte-Variablen, die im Programm verwendet werden.

## Prozeduren und Funktionen

Das Programm verwendet keine Prozeduren oder Funktionen.

## Ablauf

Das Programm fragt den Namen der Quelldatei ab und öffnet sie. Wenn kein Dateiname als Parameter angegeben wird, wird der Benutzer aufgefordert, den Dateinamen einzugeben.

Die Quelldatei wird in 7-Bit-ASCII-Code konvertiert und in eine neue Datei mit dem gleichen Namen wie die Quelldatei und der Erweiterung `.CPM` geschrieben.

Am Ende der Konvertierung wird die Quell- und Zieldatei geschlossen und eine Meldung ausgegeben, dass die Konvertierung abgeschlossen ist.

## Beispiel

Dieses Beispiel konvertiert die Datei `test.txt` in den 7-Bit-ASCII-Code und schreibt die konvertierte Datei in `test.CPM`.
