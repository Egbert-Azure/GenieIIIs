# Installation des Holte CP/M+
## Egbert Schröer, Volker Dose April 1993

## 1. Vorwort
Als Besitzer eines Genie 3s hat man die Wahl zwischen zwei Betriebssystemen: Gdos oder CP/M.

Da seit einigen Jahren für unsere old fashioned Computer an (neuer) Software unter Gdos keine Unterstützung mehr geleistet wird, fällt der Umstieg zum CP/M leicht. Ein kleiner Wermutstropfen ist, dass die sehr guten Grafikmöglichkeiten des Genie 3s unter Standard CP/M keine Beachtung finden. Hier ist - wie so oft - Eigeninitiative gefragt.

Auch für das CP/M gibt es für den Genie 3s Computer mehrere Varianten:

CP/M 2.2 von Klaus Kämpf
CP/M 3.0 Klaus Kämpf
CP/M 2.2 von Thomas Holte
CP/M 3.0 Thomas Holte
Herr Kämpf unterstützt - trotz mehrmaliger höflicher Anschreiben, die er allerdings völlig ignorierte - leider nicht mehr seine Betriebssystem-Kreationen. Somit sind Hardware-Erweiterungen wie zum Beispiel der Einbau einer größeren Festplatte von vornherein zum Scheitern verurteilt.

Ganz anders Herr Thomas Holte: Auf Initiative von Fritz Chwolka, der den Autor des wirklich ausgezeichneten CP/M 3.0 mit einer detektivischen Glanzleistung unter seiner neuen Adresse aufspürte, erhielten wir nicht nur das CP/M 3.0, sondern - und an dieser Stelle sei Herrn Holte dafür nochmals gedankt - auch die kompletten Sources. Die Sources beinhalteten nicht nur die reinen CP/M Betriebssystem-Komponenten, sondern auch alle Utility-Programme, die Herr Holte für den Genie 3s geschrieben hatte!

Man besitzt damit (fast) alles, um Hardware-Änderungen und/oder Erweiterungen am Genie 3s vorzunehmen. In unserem konkreten Fall bedeutete das den Einbau eines OMTI-Festplattencontrollers und die Implementation einer 20 MB Festplatte in das CP/M 3.0 Betriebssystem.

Wie wir im Folgenden sehen werden, sind auch andere Erweiterungen im Betriebssystem oder die individuelle Anpassung, wie z.B. RAM-Disk, kein Problem.

Ziel des Artikels ist es die Betriebssystem-Komponenten (Kapitel 2), die (System-)Utilities (Kapitel 3), die Grafik-Möglichkeiten (Kapitel 4) und die Möglichkeit das vorhandenen Systems durch Einsatz eigener Utilities oder Hardware-Erweiterungen (Kapitel 5) zu erweitern möglichst detailliert zu beschreiben, um auch dem nicht so versierten Benutzer dieses Rechnertyps die Installation zu erleichtern. Der Artikel wird in Kapitel 6 ergänzt durch das Holte CP/M 3.0 Handbuch.

## 2. Die Betriebssystem Komponenten
Die Module setzen sich wie folgt zusammen:

### Modul SYS1

``` asm
DRIVER.MAC  *8 6 0 1 1 3*

Funktion:

I/O-DRIVERS FOR THE GENIE IIIs  *

Autor/Version:

Thomas Holte/Version 1.1*

Bemerkungen/[Änderungen im Modul:
F.Chwolka
Änderung der Cursorsteuerung  'ESC     xx  '  Befehle  zur   Wordstarkompatibilitaet
und da ich mit ZCPR arbeite. Falls jemand sich
mal der Grafikmoeglichkeiten in einem Anwenderprogramm annimmt bitte
ich um Nachricht.

Volker Dose
Dies ist ein DRIVER.MAC mit Harddisk-Einbindung einer HD mit  OMTI-
Controller.
Folgende Änderungen sind zu beachten :

1. Harddiskroutinen im Commonbereich sind draußen
Dezember 1992

Egbert Schröer
Uhr auf Software clock eingestellt.
Video an FBAS-Monitor angepasst
(10 SCAN-Lines, zb. SAKATA,Philips)
November 1993
```

### Modul SYS1A:

``` asm
FONT12.MAC *8 5 1 1 2 6*

Funktion:

FONTSET FOR MONITORS            *
WITH 12 SCANLINES PER TEXTLINE*

Autor/Version:

Thomas Holte/Version 1.0  *
```

### Modul SYS2:

``` asm
BOOTER.MAC  *8 5 1 1 2 0*

Funktion:

BOOTSTRAP LOADER FOR CP/M 3

Autor/Version:

Thomas Holte/Version 1.1  *

Bemerkungen/Äderungen im Modul:

Volker Dose
Die Übertragung der Seriennummer vom Bildschirm in den Speicher  zur
Copyright-Protection wird gelöscht. Stattdessen werden NOPs eingebaut,
um dort  eventuell  Code  für  Z180  Betrieb  zu   patchen.
```

### Modul SYS3

``` asm
LDRBIOS.MAC  *8 5 1 1 2 0*

Funktion:

MINIMUM BIOS FOR CPMLDR

Autor/Version:

Thomas Holte/Version 1.0  *

Bemerkungen/Änderungen im Modul:

Egbert Schroeer
Ausserdem wurde die gesamte Copyright-Geschichte entfernt, da Volker mit einem Z180 arbeite, und die Routine ILLEGALs enthaelt !!!!!!
```

### Modul SYS4

``` asm
BNKBIOS.MAC  *8 5 0 7 1 7*

Funktion:

ROOT MODULE OF RELOCATABLE BIOS

Autor/Version:

Thomas Holte/Version 1.0  *
```

### Modul SYS4a:

``` asm
SCB.MAC  *8 5 0 2 0 5*

Funktion:

SYSTEM CONTROL BLOCK DEFINITION

Autor/Version:

Thomas Holte/Version 1.0  *
```

### Modul SYS4b

```as
BOOT.MAC  *8 5 1 0 2 3*

Funktion:

BOOT LOADER MODULE FOR CP/M 3.0 *

Autor/Version:

Thomas Holte/Version 1.0  *
```

### Modul SYS4a

``` as
CHARIO.MAC  *8 5 0 7 1 7*

Funktion:

CHARACTER I/O HANDLER FOR Z80 CHIP
BASED SYSTEM                    *

Autor/Version:

Thomas Holte/Version 1.0  *
```

### Modul SYS4d:

``` as
MOVE.MAC *8 5 0 7 1 7*

Funktion:

MOVE MODULE FOR CP/M3 LINKED BIOS*

Autor/Version:

Thomas Holte/Version 1.0  *
```

### Modul SYS4e:

``` as
DRVTBL.MAC  *8 5 0 9 2 5*

Funktion:

D RIVE TABLE                    *

Autor/Version:

Thomas Holte/Version 1.0  *
```

### Modul SYS4f:

``` as
DISKIO.MAC *8 5 0 9 2 5*

Funktion:

DISK HANDLER                    *

Autor/Version:

Thomas Holte/Version 1.0  *

Bemerkungen/Änderungen im Modul:
Fritz Chwolka
Aus verschiedenen Versionen zusammengelegt und Ansteuerung für Hard-
Disk entfernt, um Pseudo-Laufwerke zur Formatkonvertierung einzubinden.
In den Originaldateien DISKIO.MAC, TABLES.MAC und DRIVER.MAC sind dir Werte für das High-Density Format 'FRITZ' als Standartwerte einzutragen. Zur Portabilität wäre eine identische Formatdefinition
von Vorteil. Beim HD-Format habe ich die Formatgroesse  durch Versuche festgelegt. Die Systemspur beinhaltet auf/für meine CPU 280 eine Formatangabe, so dass das Format auf  meinem  anderen  Rechner automatisch erkannt wird. Da nicht von High Density gebootet wird könnte man die Systemspur auch entfallen lassen (siehe vorher).
Hier möchte ich mich noch für das Entgegenkommen des Herrn Holte bedanken, welcher mir seine Originaldisketten zur Systemerstellung
vertrauensvoll zusandte, wobei ich anschließend das Systembios bei Herrn Holte erstand. Das BIOS darf unter beibehalten der COPYRIGHT-Meldung des Herrn HOLTE weitergegeben werden.
Die Copyrigthrechte der Firma Digital Research betreff dem CPM+, hier speziell der ???????.SPR Dateien ist von obigen  Aussagen  unbeeinträchtigt. Sie sollten auf jeden Fall eine  Systemdiskette  von
Digital Research  erworben haben, um so als Lizenznehmer  die  SPR- Systemdateien der Firma Digital Research zu nutzen.
Bei Problemen oder Fragen bin ich unter obiger Adresse  immer  erreichbar und freue mich für jede Zuschrift.
1989 F.Chwolka

Volker Dose
Änderungen wurden  vorgenommen um zum einen um  eine  Harddisk  mit OMTI-Controller lesen zu können. Die Harddisk Routine für 10MByte Platte mit 2 Köpfen und 612 Cylindern wird jetzt  eingebunden.  Die Routinen sind die BIOS Sources von Helmut Bernhardt aus HD2.MAC. Der DPB wurde mit XCPM3.LIB aus HDDTBL.ASM errechnet.
Außerdem soll die RAM-Floppy 770Kbyte Speicher bekommen, da bei mir der Helmutsche 1MB-Umbau eingebaut ist. Dafür war es notwendig die
XMOVE-Funktion in DRIVER.MAC zu ändern.
Außerdem noch verschiedene Kleinigkeiten in DRIVER.MAC
Jetzt ist ein 8 Zoll Laufwerk als logisches Laufwerk E: eingetragen. Format IBM 3740 Standard 77 Tr. SS SD
Zurzeit ist LW E: wieder raus.
Die Benutzung des IX-Registers in den Routinen zum Lesen und Schreiben vom/auf  das logische Laufwerk P: verträgt sich nicht mit  dem ZPM3, welchem ich den Vorzug gebe. IX ist also durch IY vertauscht
worden.
Das neueste ist jetzt die Aufteilung der HardDisk in zwei logische Laufwerke C: und D:
19.6.93
Es ist jetzt eine Tandon Festplatte mit 20 MB Kapazität eingebaut, also Einteilung in C: mit 7 MB und D: mit 14 MB.
15.9.93

Egbert Schröer
Auf Seagate ST 225 mit 2 Partitionen a 10.4 MB eingestellt.
Ramdisk auf 55K
21.12.93
```

### Modul SYS4g:

``` as
MODEBAUD.MAC *8 4 1 0 0 5*

Funktion:

EQUATES FOR MODE BYTE BIT FIELDS*

Autor/Version:

Thomas Holte/Version 1.0  *
```

### Modul SYS5:

``` as
INITW.MAC  *8 5 1 1 2 6*

Funktion:

UTILITY FOR GENERATING A GENIE IIIs*
CP/M Ver.3.0 HARDDISK SYSTEM*

Autor/Version:

Thomas Holte/Version 2.0  *

Bemerkungen/[nderungen im Modul:

herumgeschnitzt am 09.12.92 und auf TM502 eingestellt hat E.Schroeer      *
beim Original von Holte waren folgende bugs zu finden:*
_main --> auf main gesetzt (siehe auch PCCOPY)                              *
movmem -->  bringt die Copyright Message auf die Festplatte                 *
diese Routine wurde analog der Routine memcpy in format.c       *
ausgelegt. Fehler nicht beseitigt -> Routine geloescht          *
Programm nicht lauffaehig (mit LIBCAP gelinkt)                   *
in LIBCAP ist die Routine_main enthalten in CLIB anscheinend   *
die Routinen movemem und setmem 25.12.92*
setmem  --> wurde analog format.c in memset umbenannt und umgeschrieben     *
keine Wirkung.*
movmem und setmem aus CLIB extrahiert und mit LIBCAP zu NEWLIBC verbunden   *
26.12.92/ fehlerfreies LINKING. Program nicht lauffähig. Als Test mit*
Originalversion (TM252.H etc.) compiliert -> nicht lauffähig. Wie sieht     *
die Version 1.0 des INITW.C aus?*
Routine bios.h eingefügt: muss meiner Meinung nach vorhanden sein ?          *
am 25.12.92 wieder entfernt*
E.Schroeer 11.12.92
```

## 2.1. Die Utilities

``` as
/******************************************************************************

B A C K U P  *U T I L S 0 0 1*  T h o m a s   H o l t e  *8 6 0 9 1 1*

*******************************************************
************************

  - *
- B A C K U P   U T I L I T Y   F O R   L A R G E   M A S S   M E D I A    *
- =====================================================================    *
  - *
- O N   C P / M - B A S E D   M I C R O C O M P U T E R   S Y S T E M S    *
- =====================================================================    *
  - *
  - *
- Thomas Holte                                                 Version 4.0  *
  - *

*******************************************************************************

- Linking mit BACKUPG.MAC | BACKUP.C \ Bibliothek: LIBCAP.REL                 *
- 27.12.92 Egbert Schröer                                                     *
******************************************************************************/
```

``` as
/******************************************************************************

- C H A R G E N  *U T I L S 0 0 2* T h o m a s   H o l t e *8 6 0 9 2 3*

*******************************************************************************

  - *
- F O N T   E D I T O R   F O R   T H E   G E N I E   I I I s         *
- ===========================================================         *
  - *
- M I C R O C O M P U T E R   S Y S T E M                   *
- =======================================                   *
  - *
  - *
- Version 1.0                                                  Thomas Holte  *
  - *

******************************************************************************/
```

``` as
/******************************************************************************
- C O N F I G  *U T I L S 0 0 3*  T h o m a s   H o l t e  *8 6 0 9 1 1*

*******************************************************************************

  - *
- S Y S T E M   C O N F I G U R A T O R   F O R   T H E   G E N I E   I I I s *
- =========================================================================== *
  - *
- M I C R O C O M P U T E R   S Y S T E M                   *
- =======================================                   *
  - *
  - *
- Version 1.1                                                  Thomas Holte  *
  - *

******************************************************************************/
```

``` as
/******************************************************************************

- C O P Y  *U T I L S 0 0 4*  T h o m a s   H o l t e  *8 5 0 9 2 3*

*******************************************************************************

  - *
- B A C K U P   U T I L I T Y   F O R   C P / M - B A S E D          *
- =========================================================          *
  - *
- M I C R O C O M P U T E R   S Y S T E M S                  *
- =========================================                  *
  - *
  - *
- Thomas Holte                                                 Version 3.2  *
  - *

******************************************************************************/
```

``` as
/******************************************************************************

- F K E Y  *U T I L S 0 0 5*  T h o m a s   H o l t e  *8 6 0 9 1 1*

*******************************************************************************

  - *
- F U N C T I O N   K E Y   P R O G R A M M E R   F O R   T H E        *
- =============================================================        *
  - *
- G E N I E   I I I s   M I C R O C O M P U T E R   S Y S T E M        *
- =============================================================        *
  - *
  - *
- Thomas Holte                                                 Version 1.0  *
  - *

******************************************************************************/
```

``` as
/******************************************************************************
- F O R M A T  *U T I L S 0 0 6*  T h o m a s   H o l t e *8 6 0 9 1 1*

*******************************************************************************

  - *
- F O R M A T T I N G   U T I L I T Y   F O R   C P / M - V E R . 3      *
- =================================================================      *
  - *
- O N   T H E   G E N I E   I I I s   M I C R O C O M P U T E R   S Y S T E M *
- =========================================================================== *
  - *
  - *
- Thomas Holte                                                 Version 1.0  *
  - *

*******************************************************************************

- mit CHECKTRK FORMTRK FORMATG und LIBCAP.REL linken!!                       *
- 20.12.92 Egbert Schröer                                                     *
*******************************************************************************/
```

``` as
/******************************************************************************

- B I T S Y  *X U T I L S 0 0 0*  T h o m a s   H o l t e  *8 6 0 9 1 1*

*******************************************************************************

  - *
- C O P Y   U T I L I T Y :   B I T S Y   <------->   G E N I E   I I I s   *
- =======================================================================   *
  - *
  - *
- Version 1.0                                                  Thomas Holte  *
  - *

*******************************************************************************

- Linking mit WINDOW.C | BITSY.C | Bibliothek: LIBCAP.REL                     *
- 27.12.92 Egbert Schröer                                                     *
******************************************************************************/
```

``` as
/******************************************************************************

- D A N A L  *X U T I L S 0 0 1*  T h o m a s   H o l t e  *8 6 0 9 1 1*

*******************************************************************************

  - *
- D I S K   A N A L Y Z E R   F O R   T H E   G E N I E   I I I        *
- =============================================================        *
  - *
- M I C R O C O M P U T E R   S Y S T E M                   *
- =======================================                   *
  - *
  - *
- Version 3.0                                                  Thomas Holte  *
  - *

*******************************************************************************
- folgende Files mit linken:                                                  *
- WINDOW.C | READADDR.MAC | DANALT.MAC | DANAL.C                              *
- Bibliothek: LIBCAP.REL    Egbert Schroeer, 27.12.92                         *
******************************************************************************/
```

``` as
/******************************************************************************

- P C C O P Y  *X U T I L S 0 0 2* T h o m a s   H o l t e *8 6 0 9 1 2*

*******************************************************************************

  - *
- C O P Y   U T I L I T Y :   G E N I E   I I I s   <------>   I B M   P C   *
- ========================================================================   *
  - *
  - *
- Version 1.1                                                  Thomas Holte  *
  - *

*******************************************************************************

- folgende Files müssen mit gelinkt werden:                                  *
- WINDOW.C | CHECKTRK.MAC | FORMTRK.MAC | PCCOPY.C                            *
- Bibliothek: NEWLIBC.REL | ein hartes Stück try and error; Egbert Schröer   *
- 27.12.92                                                                    *
******************************************************************************/
```
