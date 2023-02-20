.he
.pa�
               APPENDIX B -- FATCAT Configuration

�
     {FATCAT and all the files in FATCAT23.LBR are copyright 1986�
      by Steven M. Cohen and therefore remain his property.  You �
      are free to use it and distribute it freely but you may not �
      sell it or bundle it as part of another package to be sold �
      without the express written consent of the author.}
�
   �
     This appendix should give you all the information you need �
to answer the questions posed in the FATCAT configuration module �
which is accessed by typing <P> at the FATCAT main menu.  
�
     A word about configuration file names.  A FATCAT �
configuration file has an extension of .CFG.  The configuration �
you expect to use most frequently should be named STANDARD.CFG �
because that is the filename FATCAT looks for by default when �
setting up a configuration.  Other configs you may desire can �
have different names with the .CFG extension.  You can load these �
in at the command line e.g.
�
     A> FATCAT RAMDISK
�
auto loads the file RAMDISK.CFG from the default area if it can �
be found.  You can also load .CFG files dynamically from within �
FATCAT by typing Ctrl-N while in the configuration module.  �
FATCAT then prompts you for the name of a .CFG file which is �
loaded if possible.

�
     Below is information on the various options which are �
selectable from within the configuration module.


                     Editing Configurations

     Th� P� optio� provide� � convenien� mean� o� changin� th� �
parameter� whic� contro� FATCAT'� differen� mode� o� operation�  �
I� additio� i� ha� facilitie� fo� savin� thes� configuration� t� �
dis� an� fo� loadin� them.
�
     Upo� selectin� thi� optio� yo� ar� projecte� int� � �
"Database� typ� o� Dat� Entr� Scree� wher� th� variou� �
configuratio� option� ar� show� an� ca� b� changed�  Thi� sor� o� �
dat� entr� wil� b� familia� t� mos� compute� user� bu� wil� b� �
explaine� briefl� here:
�
     Typin� regula� ASCI� printabl� character� (No� Contro� �
Characters� wil� caus� the� t� appea� o� scree� i� whateve� fiel� �
th� curso� i� presentl� pointin� to� a� lon� a� th� character� �
yo� typ� d� no� overflo� th� fiel� lengt� a� indicate� b� �
underscore� o� th� screen�  Typin� <RETURN� o� <TAB� move� yo� �
forwar� on� field�  Th� familia� "Wordstar� curso� diamon� i� ��.heAPPENDIX B -- FATCAT Configuration 
als� partiall� employed� ^� move� yo� bac� on� field� ^� doe� th� �
sam� a� <RETURN>�  Similarly� ^� move� th� curso� bac� on� �
characte� withi� � fiel� (withou� deleting� an� ^� move� forward� �
<BACKSPACE� an� <DELETE� d� th� sam� thing� delet� on� characte� �
back� an� ^� delete� th� characte� a� th� curso� position� ^� �
delete� fro� th� curso� t� th� en� o� th� field� an� ^� delete� �
a� entir� field�  I� th� arro� key� ar� installe� (se� �
Installatio� section� the� Left-arro� wil� perfor� th� sam� �
functio� a� ^S� Right-arro� a� ^D� up-arro� a� ^� an� down-arro� �
a� ^X.
�
Man� o� th� field� ar� single-characte� fields�  O� thes� i� �
i� no� necessar� t� typ� th� <RETURN>� jus� typin� th� characte� �
advance� t� th� nex� field�  O� mos� o� th� fields� onl� certai� �
type� o� inpu� ar� acceptable� whic� wil� b� immediatel� �
apparent� o� i� not� indicate� o� th� screen.
�
Typin� Cntl-� a� an� tim� wil� resul� i� you� bein� prompte� fo� �
th� nam� o� � ne� .CF� fil� t� load�  I� FATCA� find� thi� file� �
it� value� wil� b� loade� i� ove� an� tha� ar� currentl� present.
�
Typin� Cntl-� a� an� tim� tell� FATCA� yo� ar� don� alterin� th� �
configuration�  Se� sectio� o� savin� configuratio� below�  Not� �
tha� yo� d� no� hav� t� sav� configurations�  Th� <P� optio� ma� �
als� b� use� fo� "quic� an� dirty� o� th� fl� changes� suc� a� �
changin� temporarily th� driv� t� b� catalogued.
�
Thi� i� probabl� mor� explanatio� tha� i� necessar� -- jus� tr� �
th� <P� option� pla� wit� th� keyboar� an� yo� wil� soo� gras� �
it� functions� 


                      Saving Configurations

Whe� yo� ar� satisfie� tha� yo� hav� � goo� configuration� simpl� �
typ� Cntl-� an� yo� ar� the� aske� i� yo� wan� t� sav� th� confi� �
a� � file�  I� yo� answe� 'Y� yo� ar� the� prompte� fo� � �
filenam� unde� whic� t� sav� it� I� th� fil� alread� exist� yo� �
ar� aske� whethe� yo� mea� t� overwrit� it� 
�
Yo� ar� the� returne� t� th� mai� menu�  Whateve� value� yo� hav� �
chose� ar� th� curren� value� an� wil� remai� s� unles� yo� �
chang� the� again� fo� th� duratio� o� you� curren� sessio� wit� �
FATCAT.
�
I� yo� wan� t� mak� th� configuratio� permanent� th� easies� wa� �
t� d� s� i� t� sav� th� confi� unde� th� nam� o� STANDARD�  Thi� �
confi� wil� b� loade� wheneve� yo� cal� FATCA� subsequently� �
unles� yo� overrid� i� wit� � paramete� indicatin� tha� anothe� �
fil� shoul� b� loaded.
.pa�                      Configuration Options
     
Th� option� availabl� unde� configuratio� ar� explaine� i� detai� �
below:

Catalog Libraries (Y/N)
Th� defaul� valu� her� i� � an� wil� usuall� b� lef� tha� wa� �
unles� yo� don'� wan� � catalo� o� you� librar� fil� members�  I� �
yo� don'� us� librar� file� yo� migh� wan� t� leav� i� off�  I� �
Dis� spac� i� � rea� proble� yo� migh� als� wan� i� off� �
otherwise� leav� i� on�  Onc� se� fo� � particula� catalog� yo� �
shoul� leav� i� tha� way� o� you� librar� catalog� wil� no� b� � �
tru� reflectio� o� wha� i� o� you� disks.
�
Clean-Up Mode (Y/N)
Th� defaul� valu� her� i� � a� well�  Thi� settin� ca� als� b� �
change� fro� th� mai� men� usin� th� T� optio� whic� simpl� �
toggle� i� o� an� off� bu� changin� i� her� wil� giv� i� th� �
initia� valu� yo� desire�  Yo� woul� wan� i� o� especiall� fo� �
cataloguin� ne� disk� wher� yo� ma� hav� jun� file� you'� b� �
bette� of� gettin� ri� of� o� fo� addin� th� zero-lengt� disk-�
nam� file� tha� FATCA� need� t� d� it� work�  Turnin� i� of� wil� �
yiel� � substantial spee� increase.


ReIndex Mode
I� yo� hav� lot� o� tim� t� spar� an� an� don'� min� lettin� you� �
compute� chur� awa� fo� � bi� chun� o� tim� (coul� b� ove� a� �
hou� wit� 500�  file� o� � flopp� system� whil� yo� d� somethin� �
else� the� selec� 'Y� her� t� leav� th� reindexin� toggl� on�  �
Th� advantag� her� i� tha� th� catalo� file� ar� remad� ane� fro� �
th� inde� files� whic� decrease� th� dange� o� file� becomin� �
corrupted�  
        
Mos� peopl� wil� probabl� conclud� tha� thi� i� jus� to� slo� an� �
prefe� t� leav� i� off�  I� tha� case� i� migh� no� b� � ba� ide� �
t� bac� you� catalo� disk� u� ont� othe� disk� whe� through�  I� �
fact� that'� neve� � ba� idea.

Maximum User
Numeri� inpu� require� here� th� highes� use� are� yo� wil� �
cataloguing� Shoul� b� se� t� th� highes� use� are� you� �
operatin� syste� ca� handle� typicall� 1� bu� 3� i� som� ZCPR� �
implementations�  I� yo� ar� sur� yo� won'� us� th� highe� �
numbers� g� ahea� an� se� i� lower� wil� mak� th� progra� g� �
faste� o� hard-dis� cataloguin� (se� optio� H>)� I� fac� her� i� �
on� instanc� wher� � differen� confi� fil� fo� Har� Disk� migh� �
b� ver� useful.
�
Anothe� ide� her� woul� b� fo� us� o� remot� system� where� sa� �
th� onl� accessibl� area� woul� b� thos� o� use� area� 0-7�  I� �
tha� case� fo� preparin� th� catalo� t� b� displaye� t� remot� �
users� se� Maximu� use� t� 7� an� yo� wil� win� u� onl� �
cataloguin� thos� use� are� yo� wan� you� remot� user� t� see.
�Date Stamping
Defaul� valu� i� � an� leav� i� tha� wa� unles� yo� us� th� �
Plu*Perfec� DateStamper�  I� turne� on� thi� change� FATCAT'� �
assumption� abou� wher� i� th� lis� o� file� th� dis� nam� is� s� �
a� no� t� conflic� wit� DateStamper'� assumptio� tha� it� specia� �
fil� i� th� first�  However� n� feature� o� th� DateStampe� ar� �
supporte� b� FATCA� -- don'� loo� fo� an� dat� stampin� i� th� �
catalo� listings�  Ther� ain'� any.
 
User Area to be Catalogued  (32 = Catalog All User Areas)
Thi� require� � littl� mor� comment�  Normall� se� thi� t� 3� an� �
leav� i� there�  3� doe� no� mea� use� 32� bu� tell� FATCA� t� �
proces� al� use� area� a� once�  An� numbe� les� tha� 3� tell� �
FATCA� t� proces� ONLY tha� use� number�  D� no� confus� wit� �
Maximu� Use� whic� give� th� highes� use� are� t� catalog�  Ther� �
shouldn'� ofte� b� reaso� t� chang� this� bu� i� i� include� �
becaus� someon� wil� fin� i� useful.
�
Printer Offset
I� yo� ar� someon� wh� lik� t� pu� th� pape� i� th� printe� an� �
neve� mov� th� tractor� fro� thei� usua� setting� the� yo� ma� �
selec� � numbe� fro� � t� 20� an� whe� printin� you� catalog� th� �
printe� hea� wil� ski� tha� man� space� befor� printin� o� � ne� �
line�  Experimen� an� fin� you� bes� settin� here.

Root File Name for FATCAT Files
Thi� establishe� wha� roo� nam� you� FATCA� catalo� file� wil� �
have� Al� Fatca� file� fo� � particula� catalo� se� wil� hav� th� �
sam� roo� name� onl� thei� fil� type� wil� b� differen� (an� �
thes� ar� se� automaticall� b� th� program)� Defaul� value� �
MASTER�  FATCA� file� wil� thu� hav� name� lik� MASTER.LC� an� �
MASTER.RIX� etc�  Al� file� i� � catalo� se� ca� thu� b� easil� �
transferre� usin� familia� cp/� wildcar� technique� i.e.
�
                 A>PIP B:=A:MASTER.*
�


FILE PLACEMENT CHOICES

Drive to be Catalogued�
Program Files on Drive:�
Index File for Regular Files on Drive      (.RIX)�
Index File for Library Files on Drive      (.LIX)�
Catalog File for Regular Files on Drive    (.RCX)�
Catalog File for Library Files on Drive    (.LCX)�
Index File for Disk Names on Drive         (.DNX)�
Temporary List File on Drive               (.TCX)

�
Thes� option� contro� th� location� o� th� variou� files� Yo� �
configur� thes� t� you� system�  Belo� ar� som� sampl� �
configurations�  Us� thes� a� � guide� quit� likel� you'l� b� �
abl� t� com� u� wit� � bette� confi� fo� you� system� Capacitie� �
ar� ver� roug� estimates� dependin� largel� o� th� numbe� o� ��librar� file� yo� ar� counting� a� librar� file� tak� u� 50� mor� �
roo� tha� regula� file� i� th� catalogs.
.cp20�
SYSTEM 1

          2-floppy system, single sided drives of less than 200K:
          An absolute minimum system for FATCAT.
          Capacity: maybe 3000 files.

          Drive to be Catalogued        B:
          Program files:                A:
          Regular file index            B:
          Library file index            B:
          Catalog for regular files     B:
          Catalog for Library files     B:
          Disk Names Index              B:
          Temporary List File           A:
          
     The trick here is to be sure to insert the disk for the 
     catalog files after the last disk has been catalogued but 
     before actually selecting the U> option;


.cp20
SYSTEM 2

          2-floppy system, double sided drives 3-400K:
          A better system.  
          Capacity: 8-10000 files

          Drive to be Catalogued        B:
          Program files:                A:
          Regular file index            A:
          Library file index            A:
          Catalog for regular files     B:
          Catalog for Library files     B:
          Disk Names Index              A:
          Temporary List File           A:

     As in the first system, you must remember to insert the disk 
     with the catalog files before you select the U> option.  
     By putting the catalog files on their own disk away from the
     index files, more room is created, but we're still feeling 
     some pinch from the need to keep 100 or so K of program 
     files on drive A:

.cp20�
SYSTEM 3

          3 floppy-system 300-400K per drive
          A much better system
          Capacity 12000 - 14000 files.

          Drive to be Catalogued        C:
          Program files:                A:
          Regular file index            B:
          Library file index            B:
          Catalog for regular files     C:
          Catalog for Library files     C:
          Disk Names Index              B:
          Temporary List File           A:

     By putting the program files on their own drive, we 
     significantly expand the space available for catalog files.
     We still must insert the correct disk before updating.


.cp20

SYSTEM 4

          4 floppy system 300-400K per drive
          Optimum floppy system
          Capacity 12000 - 14000 files

          Drive to be Catalogued        D:
          Program files:                A:
          Regular file index            B:
          Library file index            B:
          Catalog for regular files     C:
          Catalog for Library files     C:
          Disk Names Index              B:
          Temporary List File           A:
          
     At last we can avoid any disk swapping,  and put each disk 
     into its proper home before we start.  But no real increase 
     in capacity this time.



.cp20�SYSTEM 5 

          With hard disks, large capacity ram disks, etc., there is 
     not as much point in going into all this detail about 
     configuration.  You have many more options and more room.
     The only requirement is that all program, catalog, and 
     configuration files be located in the same user area.  A 
     sample Hard disk configuration is quite simple:

          Drive to be Catalogued        C:
          Program files:                A:
          Regular file index            A:
          Library file index            A:
          Catalog for regular files     A:
          Catalog for Library files     A:
          Disk Names Index              A:
          Temporary List File           A:


END OF APPENDIX B.

DateStamper is a trademark of Plu*Perfect Systems 
