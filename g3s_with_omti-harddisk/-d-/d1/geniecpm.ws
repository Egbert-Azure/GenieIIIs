.mb 3
.cw 12
.PL 66
.rm 68
.HEGenie IIIs - Installation des Holte CP/M +         Seite #

Installatioî deó Holtå CP/M+
.cw 8
Egberô Schr|er¬ Volkeò Doså Apriì 1993
.cw 12

1® Vorwort

Aló Besitzeò eineó Geniå 3ó haô maî diå Wahì zwischeî zweé Betriebs
systemen:
deí       Gdoó oder
          CP/M

Dá seiô einigeî Jahreî f}ò unserå olä fashioneä Computeò aî  (neuer© 
Softwarå  unteò Gdoó keinå Unterst}tzunç mehò geleisteô wird¬  f{llô 
deò Umstieç zuí CP/Í leicht® Eiî kleineò Wermutstropfeî ist¬ daş diå 
sehò  guteî  Grafikm|glichkeiteî deó Geniå 3ó  unteò  Standarä  CP/Í 
keinå  Beachtunç  finden® Hieò isô ­ wiå sï  ofô  ­  Eigeninitiativå 
gefragt.

Aucè f}ò daó CP/Í gibô eó f}ò deî Geniå 3ó Computeò mehrerå  Varian
ten:

ï CP/Í 2.² voî Klauó K{mpf
ï CP/Í 3.°     Klauó K{mpf
ï CP/Í 2.² voî Thomaó Holte
ï CP/Í 3.°     Thomaó Holte

Herò  K{mpæ unterst}tzô ­ trotú mehrmaligeò  h|flicheò  Anschreiben¬ 
diå  eò allerdingó v|lliç ignoriertå ­ leideò nichô mehò  seinå  Be
triebssysteí-Kreationen®  Somiô sinä Hardwarå-Erweiterungeî wiå  zuí 
Beispieì  deò  Einbaõ eineò gr|~ereî Festplattå voî  vornhereiî  zuí 
Scheiterî verurteilt.

Ganú anderó Herò Thomaó Holte:

Auæ Initiativå voî Fritú Chwolka¬ deò deî Autoò deó wirklicè  ausge
zeichneteî  CP/Í  3.° miô eineò detektivischeî  Glanzleistunç  unteò 
seineò  neueî  Adresså aufsp}rte¬ erhielteî wiò nichô nuò  daó  CP/Í 
3.0¬  sonderî ­ unä aî dieseò Stellå seé Herrî Holtå daf}ò  nocèmaló 
gedankô  ­  aucè diå kompletteî Sources®  Diå  Sourceó  beinhalteteî 
nichô  nuò diå reineî CP/Í Betriebssysteí-Komponenten¬ sonderî  aucè 
alle“ Utilitù-Programme¬ diå Herò Holtå f}ò deî Geniå 3ó  geschriebeî 
hattå !

Maî  besitzô  damiô (fast© alles¬ uí  Hardwarå  [nderungeî  und/odeò 
Erweiterungeî  aí  Geniå 3ó vorzunehmen® Iî unsereí  konkreteî  Falì 
bedeutetå  daó deî Einbaõ eineó OMTÉ-Festplattencontrolleró unä  diå 
Implementatioî  eineò  2° MÂ Festplattå iî daó  CP/Í  3.°  Betriebs
system.

Wiå wiò ií folgendeî seheî werden¬ sinä aucè anderå Erweiterungeî ií 
Betriebssysteí  odeò diå individuellå Anpassung¬ wiå z.B®  RAÍ-Disk¬ 
keiî Problem.

Zieì deó Artikeló isô eó diå Betriebssysteí-Komponenteî (Kapiteì 2)¬ 
diå   (Systeí-)Utilitieó  (Kapiteì  3)¬   diå   Grafië-M|glichkeiteî 
(Kapiteì  4© unä diå M|glichkeiô daó vorhandeneî Systemó durcè  Eiî
satú  eigeneò Utilitieó odeò Hardwarå-Erweiterungeî (Kapiteì  5©  zõ 
erweiterî m|glichsô detallierô zõ beschreiben¬ uí aucè deí nichô  sï 
versierteî Benutzeò dieseó Rechnertypó diå Installatioî zõ  erleicèŠtern® Deò Artikeì wirä iî Kapiteì ¶ erg{nzô durcè daó Holtå CP/Í 3.° 
Handbuch.

2® Diå Betriebssysteí Komponenten

Diå Modulî setzeî sicè wée folgô zusammen:

Moduì SYS1:

DRIVER.MAC  *  8 6 0 1 1 3 *

Funktionº 

É/O-DRIVERS FOR THE GENIÅ IIIs	*

Autor/Version:

Thomas Holtå/Version 1.1*

Bemerkungen/[nderungeî ií Modul:
F.Chwolka
[nderunç deò  Cursorsteuerunç  'ESÃ	xø  §  Befehlå	zuò   Word
starkompatibilit{ô  unä dá icè miô ZCPÒ  arbeite® Falló jemanä  sicè 
maì deò Grafikm|glichkeiteî iî eineí Anwenderprogramí annimmô  bittå 
icè uí Nachricht.

Volkeò Dose
Dieó  isô eiî DRIVER.MAÃ miô Harddisë-Einbindunç eineò HÄ miô  OMTÉ-
Controller.
Folgende [nderungen sind zu beachten :
1. Harddiskroutinen im Commonbereich sind draussen
Dezember 1992

Egberô Schr|er
Uhò auæ Softwarå clocë eingestellt.
Videï aî FBAÓ-Monitoò angepassô 
(1° SCAÎ-Lines¬ zb® SAKATA,Philips)
Novembeò 1993 

Moduì SYS1A:

ÆÏÎÔ±².MAC ª ¸ µ ± ± ² ¶ *

Funktion:

ÆÏÎÔÓÅÔ ÆÏÒ ÍÏÎÉÔÏÒÓ		*
×ÉÔÈ ±² ÓÃÁÎÌÉÎÅÓ ĞÅÒ ÔÅØÔÌÉÎE	*

Autor/Version:

Thomaó Holte/Versioî 1.°  *

Moduì SYS2:

ÂÏÏÔÅÒ.MAC  ª  ¸ µ ± ± ² ° *

Funktion:

ÂÏÏÔÓÔÒÁĞ ÌÏÁÄÅÒ ÆÏÒ ÃĞ¯Í ³ Š
Autor/Version:

Thomaó Holte/Versioî 1.±  *

Bemerkungen/[nderungeî ií Modul:

Volkeò Doså
Diå ]bertragunç deò Seriennummeò voí Bildschirí iî deî Speicheò  zuò 
Copyrighô-Protectioî wirä gel|scht® Statô desseî werdeî NOPó  einge
baut¬   uí  dorô  eventuelì  Codå  f}ò  Z18°  Betrieâ  zõ   patchen®      

Moduì SYS3:

ÌÄÒÂÉÏÓ.MAC  ª ¸ µ ± ± ² ° *

Funktion:

ÍÉÎÉÍÕM	ÂÉÏÓ ÆÏÒ ÃĞÍÌÄR

Autor/Version:

Thomaó Holte/Versioî 1.°  *

Bemerkungen/[nderungeî ií Modul:

Egberô Schr|er
Ausserdeí wurdå diå gesamtå Copyrighô-Geschichtå entfernt¬ 
dá Volker miô eineí Z18° arbeite¬ unä diå Routinå ILLEGALó enth{lô !!!!!!

Moduì SYS4:

ÂÎËÂÉÏÓ.MAC  ª ¸ µ ° · ± · *

Funktion:

ÒÏÏÔ ÍÏÄÕÌÅ ÏÆ ÒÅÌÏÃÁÔÁÂÌÅ ÂÉÏÓ

Autor/Version:

Thomaó Holte/Versioî 1.°  *

Moduì SYS4a:

ÓÃÂ.MAC  ª   ¸ µ ° ² ° µ   *

Funktion:

ÓÙÓÔÅÍ ÃÏÎÔÒÏÌ ÂÌÏÃË ÄÅÆÉÎÉÔÉÏÎ 

Autor/Version:

Thomaó Holte/Versioî 1.°  *

Moduì SYS4b:

ÂÏÏÔ.MAC  ª  ¸ µ ± ° ² ³  *

Funktion:Š
ÂÏÏÔ ÌÏÁÄÅÒ ÍÏÄÕÌÅ ÆÏÒ ÃĞ¯Í ³®0	*

Autor/Version:

Thomaó Holte/Versioî 1.°  *

Moduì SYS4a:

ÃÈÁÒÉÏ.MAC  ª ¸ µ ° · ± · *

Funktion:

ÃÈÁÒÁÃÔÅÒ É¯Ï ÈÁÎÄÌÅÒ ÆÏÒ Ú¸° ÃÈÉĞ
ÂÁÓÅÄ ÓÙÓÔÅM			*

Autor/Version:

Thomaó Holte/Versioî 1.°  *

Moduì SYS4d:

ÍÏÖÅ.MAC ª  ¸ µ ° · ± ·  *

Funktion:

ÍÏÖÅ ÍÏÄÕÌÅ ÆÏÒ ÃĞ¯Í³ ÌÉÎËÅÄ ÂÉÏÓ*

Autor/Version:

Thomaó Holte/Versioî 1.°  *

Moduì SYS4e:

ÄÒÖÔÂÌ.MAC  ª ¸ µ ° ¹ ² µ *

Funktion:

Ä ÒÉÖÅ ÔÁÂÌE			*

Autor/Version:

Thomaó Holte/Versioî 1.°  *

Moduì SYS4f:

ÄÉÓKÉÏ.MAC ª ¸ µ ° ¹ ² µ *

Funktion:

ÄÉÓË ÈÁÎÄÌÅR			*

Autor/Version:

Thomaó Holte/Versioî 1.°  *

Bemerkungen/[nderungeî ií Modul:

Fritú ChwolkaŠAuó verschiedeneî Versioneî zusammengelegô unä Ansteuerunç f}ò Harä-
Disë entfernô uí Pseudï-Laufwerkå zuò Formatkonvertirunç  einzubinä
en®  Iî deî Originaldateieî  DISKIO.MAC¬ TABLES.MAÃ  unä  DRIVER.MAÃ 
sinä diò Wertå f}ò daó Higè-Densitù Formaô 'FRITZ§ aló Standartwertå 
einzõtragen®  Zuò Portabilitaeô waerå einå identischå  Formatdefini
tioî  voî  Vorteil® Beií HÄ-Formaô habå icè  diå  Formatgr|~å  durcè 
Versuchå festgelegt® Diå Systemspuò beinhalteô auf/f}ò meinå CPÕ 28° 
einå  Formatangabe¬  sï daş daó Formaô auæ  meineí  andereî  Rechneò 
automátiscè  erkannô wird® Dá nichô voî Higè Densitù  gebooteô  wirä 
k|nntå maî diå Systemspuò aucè entfalleî lassen® (siehå vorher).
Hieò  m|chtå  icè micè nocè f}ò daó Entgegenkommeî deó  Herrî  Holtå 
bedanken¬  welcheò miò seinå Originaldisketteî zuò  Systemerstellunç 
vertrauensvolì  zusandte¬ wobeé icè anschlie~enä daó Systembioó  beé 
Herrî Holtå erstandt® Daó BIOÓ daræ unteò beibehalteî deò  COPYRIGHÔ 
­ Meldunç deó Herrî HOLTÅ weitergegebeî werden.
Diå  Copyrechtå deò Firmá Digitaì Researcè betrefæ deí CPM«  ¬  hieò 
spezielì  deò ???????.SPÒ Dateieî isô voî obigeî  Aussageî  unbeeiî
tr{chtigt®  Siå  sollteî  auæ jedeî  Falì  einå  Systemdiskettå  voî 
Digitaì  Researcè  erworbeî haben¬ uí sï aló Lizensnehmeò  diå  SPÒ-
Systemdateieî deò Firmá Digitaì Researcè zõ nutzen.
Beé  Problemeî  odeò Frageî biî icè unteò obigeò Adresså  immeò  eò
reichbaò unä freuå micè f}ò jedå Zuschrift.
198¹ F.Chwolka

Volkeò Dose
[nderungeî  wurdeî  vorgenommeî uí zuí eineî uí  einå  Harddisë  miô 
OMTÉ-Controlleò  leseî  zõ k|nnen® Diå Harddiskroutinå  f}ò  10MBytå 
Plattå  miô ² K|pfeî unä 61² Cylinderî wirä jetzô  eingebunden®  Diå 
Routineî sinä diå BIOÓ Sourceó voî Helmuô Bernhardô auó HD2.MAC® Deò 
DPÂ wurdå miô XCPM3.LIÂ auó HDDTBL.ASÍ errechnet.
Ausserdeí solì diå RAÍ-Floppù 770Kbytå Speicheò bekommen¬ dá beé mir
deò Helmutschå 1MÂ-Umbaõ eingebauô ist® Daf}ò waò eó notwendiç die
XMOVÅ-Funktioî iî DRIVER.MAÃ zõ {ndern.
Au~erdeí nocè verschiedenå Kleinigkeiteî iî DRIVER.MAC
Jetzô isô eiî ¸ Zolì Laufwerë aló logischeó Laufwerë Eº eingetragen.
Formaô IBÍ 374° Standarä 7· Tr® SÓ SD
Zuò Zeiô isô L× Eº wiedeò raus.
Diå Benutzunç deó IØ-Registeró iî deî Routineî zuí Leseî unä Schreé
beî  vom/auæ  daó logischå Laufwerë Pº vertr{gô sicè nichô  miô  deí 
ZPM3¬  welcheí icè deî Vorzuç gebe® IØ isô alsï durcè IÙ  vertauschô 
worden.
Daó neuestå isô jetzô diå Aufteilunç deò HardDisë iî zweé logische
Laufwerkå Cº unä D:®					
19.6.93
Eó  isô jetzô einå Tandoî Festplattå miô 2° MÂ Kapazit{ô  eingebaut¬ 
alsï Einteilunç iî Cº miô · MÂ unä Dº miô 1´ MB.
15.9.93

Egberô Schr|er
Auæ Seagatå SÔ 22µ miô ² Partitioneî á 10.´ MÂ eingestellt.
Ramdisë auæ 55K
21.12.93

Moduì SYS4g:

ÍODEÂÁÕÄ.MAC ª ¸ ´ ± ° ° µ *

Funktion:
ŠÅÑÕÁÔÅÓ ÆÏÒ ÍÏÄÅ ÂÙÔÅ ÂÉÔ ÆÉÅÌÄÓ*

Autor/Version:

Thomaó Holte/Versioî 1.°  *


Moduì SYS5:

ÉÎÉÔ×.MAC  *  ¸ µ ± ± ² ¶   *

Funktion:

ÕÔÉÌÉÔÙ ÆÏÒ ÇÅÎÅÒÁÔÉÎÇ Á ÇÅÎÉÅ ÉÉÉó*
ÃĞ¯Í Öåò®³®° ÈÁÒÄÄÉÓË ÓÙÓÔÅM	*

Autor/Version:

Thomaó Holte/Versioî 2.°  *

Bemerkungen/[nderungeî ií Modul:

herumgeschnitzô aí 09.12.9² unä auæ TM50² eingestellô haô E.Schroeeò      *
beií Originaì voî Holtå wareî folgendå bugó zõ findenº                      *
_maiî --¾ auæ maiî gesetzô (siehå aucè PCCOPY©                              *
movmeí --¾  bringô diå copyrighô messagå auæ diå Festplattå                 *
dieså Routinå wurdå analoç deò Routinå memcpù iî format.ã       *
ausgelegt® Fehleò nichô beseitigô -¾ Routinå geloeschô          *
Programí nichô lauff{hiç (miô LIBCAĞ gelinkt©                   *
iî LIBCAĞ isô diå Routinå _maiî enthalteî iî CLIÂ anscheinenä   *
diå Routineî movemeí unä setmeí 25.12.9²                        *
setmeí  --¾ wurdå analog format.ã iî memseô umbenannô unä umgeschriebeî     *
keinå Wirkung®                                                  *
movmeí unä setmeí auó CLIÂ extrahierô unä miô LIBCAĞ zõ NEWLIBÃ verbundeî   *
26.12.92¯ fehlerfreieó LINKING® Prograí nichô lauff{hig® Aló Tesô miô       *
Originalversioî (TM252.È etc.© compilierô -¾ nichô lauff{hig® Wiå siehô     *
die Versioî 1.° deó INITW.Ã auó ¿                                           *
Routinå bios.è eingef}gtº muş meineò Meinunç nacè vorhandeî seiî ¿          *
aí 25.12.9² wiedeò entfernô                                                 *
E.Schroeeò  11.12.9²                                                        *



2.1. Diå Utilites

/******************************************************************************
ª  Â Á Ã Ë Õ Ğ  ª  Õ Ô É Ì Ó ° ° ±  ª  Ô è ï í á ó   È ï ì ô å  ª ¸ ¶ ° ¹ ± ± *
*******************************************************************************
ª 									      *
ª    Â Á Ã Ë Õ Ğ   Õ Ô É Ì É Ô Ù   Æ Ï Ò   Ì Á Ò Ç Å   Í Á Ó Ó   Í Å Ä É Á    *
ª    ====================================================================½    *
ª 									      *
ª    Ï Î   Ã Ğ ¯ Í ­ Â Á Ó Å Ä   Í É Ã Ò Ï Ã Ï Í Ğ Õ Ô Å Ò   Ó Ù Ó Ô Å Í Ó    *
ª    ====================================================================½    *
ª 									      *
ª 									      *
ª   Thomaó Holte			                         Versioî 4.°  *
ª 									      *
*******************************************************************************
ª Linkinç miô BACKUPG.MAÃ ü BACKUP.Ã Ü Bibliothekº LIBCAP.REÌ                 *
ª 27.12.9² Egberô Schr|eò                                                     *
******************************************************************************/

/******************************************************************************
ª  Ã È Á Ò Ç Å Î  ª  Õ Ô É Ì Ó ° ° ²  ª Ô è ï m á ó   È ï ì ô å ª ¸ ¶ ° ¹ ² ³ *
*******************************************************************************
ª  									      *
ª    	  Æ Ï Î Ô   Å Ä É Ô Ï Ò   Æ Ï Ò   Ô È Å   Ç Å Î É Å   É É É ó         *
ª         ==========================================================½         *
ª                                                                             *
ª            	    Í É Ã Ò Ï Ã Ï Í Ğ Õ Ô Å Ò   Ó Ù Ó Ô Å M		      *
ª                   ======================================½       	      *
ª  									      *
ª                            						      *
ª  Versioî 1.°                                                  Thomaó Holtå  *
ª  									      *
******************************************************************************/

/******************************************************************************
ª  Ã Ï Î Æ É Ç  ª  Õ Ô É Ì Ó ° ° ³  ª  Ô è ï í á ó   È ï ì ô å  ª ¸ ¶ ° ¹ ± ± *
*******************************************************************************
*									      *
ª Ó Ù Ó Ô Å Í   Ã Ï Î Æ É Ç Õ Ò Á Ô Ï Ò   Æ Ï Ò   Ô È Å   Ç Å Î É Å   É É É ó *
ª ==========================================================================½ *
*									      *
ª  		    Í É Ã Ò Ï Ã Ï Í Ğ Õ Ô Å Ò   Ó Ù Ó Ô Å M		      *
*		    =======================================		      *
*									      *
*									      *
ª  Versioî 1.1							Thomaó Holtå  *
*									      *
******************************************************************************/

/******************************************************************************
ª  Ã Ï Ğ Ù  ª  Õ Ô É Ì Ó ° ° ´  ª  Ô è ï í á ó   È ï ì ô å  ª   ¸ µ ° ¹ ² ³   *Š*******************************************************************************
ª 									      *
ª 	   Â Á Ã Ë Õ Ğ   Õ Ô É Ì É Ô Ù   Æ Ï Ò   Ã Ğ ¯ Í ­ Â Á Ó Å D	      *
ª          ========================================================½          *
ª 									      *
*		   Í É Ã Ò Ï Ã Ï Í Ğ Õ Ô Å Ò   Ó Ù Ó Ô Å Í Ó 		      *
ª                  ========================================½                  *
ª 									      *
ª 									      *
ª   Thomaó Holte			                         Versioî 3.²  *
ª 									      *
******************************************************************************/

/******************************************************************************
ª  Æ Ë Å Ù  ª  Õ Ô É Ì Ó ° ° µ  ª  Ô è ï í á ó   È ï ì ô å  ª   ¸ ¶ ° ¹ ± ±   *
*******************************************************************************
ª                                                                             *
ª        Æ Õ Î Ã Ô É Ï Î   Ë Å Ù   Ğ Ò Ï Ç Ò Á Í Í Å Ò   Æ O Ò   Ô È Å        *
ª        ============================================================½        *
ª                                                                             *
ª        Ç Å Î É Å   É É É ó   Í É Ã Ò Ï Ã Ï Í Ğ Õ Ô Å Ò   Ó Ù Ó Ô Å Í        *
ª        ============================================================½        *
*									      *
ª                                                                             *
ª   Thomaó Holtå                                                 Versioî 1.°  *
ª                                                                             *
******************************************************************************/

/******************************************************************************
ª  Æ Ï Ò Í Á Ô  ª  Õ Ô É Ì Ó ° ° ¶  ª  Ô è ï í á ó   È ï ì ô å ª ¸ ¶ ° ¹ ± ±  *
*******************************************************************************
ª 									      *
ª      Æ Ï Ò Í Á Ô Ô É Î Ç   Õ Ô É Ì É Ô Ù   Æ Ï Ò   Ã Ğ ¯ Í ­ Ö Å Ò ® ³      *
ª      ================================================================½      *
ª 									      *
ª Ï Î   Ô È Å   Ç Å Î É Å   É É É ó   Í É Ã Ò Ï Ã Ï Í Ğ Õ Ô Å Ò   S Ù Ó Ô Å Í *
ª ==========================================================================½ *
ª 									      *
ª 									      *
ª   Thomaó Holte			                         Versioî 1.°  *
ª 									      *
*******************************************************************************
ª miô CHECKTRË FORMTRË FORMATÇ unä LIBCAP.REÌ linkeî !¡                       *
ª 20.12.9² Egberô Schr|eò                                                     *
*******************************************************************************/

/******************************************************************************
ª  Â É Ô Ó Ù  ª  Ø Õ Ô É Ì Ó ° ° °  ª  Ô è ï í á ó   È ï ì ô å  ª ¸ ¶ ° ¹ ± ± *
*******************************************************************************
*									      *
ª   Ã Ï Ğ Ù   Õ Ô É Ì É Ô Ù º   Â É Ô Ó Ù   <-------¾   Ç Å Î É Å   É É É ó   *
ª   ======================================================================½   *
*									      *
*									      *
ª  Versioî 1.0							Thomaó Holtå  *
*									      *
*******************************************************************************
ª Linkinç miô WINDOW.Ã ü BITSY.Ã ü Bibliothekº LIBCAP.REÌ                     *
ª 27.12.9² Egberô Schr|eò                                                     *Š******************************************************************************/

/******************************************************************************
ª  Ä Á Î Á Ì  ª  Ø Õ Ô É Ì Ó ° ° ±  ª  Ô è ï í á ó   È ï ì ô å  ª ¸ ¶ ° ¹ ± ± *
*******************************************************************************
ª  									      *
ª        Ä É Ó Ë   Á Î Á Ì Ù Ú Å Ò   Æ Ï Ò   Ô È Å   Ç Å Î É Å   É É É        *
ª        ============================================================½        *
ª                                                                             *
ª            	    Í É Ã Ò Ï Ã Ï Í Ğ Õ Ô Å Ò   Ó Ù Ó Ô Å M		      *
ª                   ======================================½       	      *
ª  									      *
ª                            						      *
ª  Versioî 3.°                                                  Thomaó Holtå  *
ª  									      *
*******************************************************************************
ª folgendå Fileó miô linkenº                                                  *
ª WINDOW.Ã ü READADDR.MAÃ ü DANALT.MAÃ ü DANAL.Ã                              *
ª Bibliothekº LIBCAP.REÌ    Egberô Schroeer¬ 27.12.9²                         *
******************************************************************************/

/******************************************************************************
ª  Ğ Ã Ã Ï Ğ Ù  ª  Ø Õ Ô É Ì Ó ° ° ²  ª Ô è ï m á ó   È ï ì ô å ª ¸ ¶ ° ¹ ± ² *
*******************************************************************************
*									      *
ª  Ã Ï Ğ Ù   Õ Ô É Ì É Ô Ù º   Ç Å Î É Å   É É É ó   <------¾   É Â Í   Ğ Ã   *
ª  =======================================================================½   *
*									      *
*									      *
ª  Versioî 1.1							Thomaó Holtå  *
*									      *
*******************************************************************************
ª folgendå Fileó muesseî miô gelinkô werdenº                                  *
ª WINDOW.Ã ü CHECKTRK.MAÃ ü FORMTRK.MAÃ ü PCCOPY.Ã                            *
ª Bibliothekº NEWLIBC.REÌ ü eiî harteó Stuecë trù anä error» Egberô Schr|eò   *
ª 27.12.9²                                                                    *
******************************************************************************/
