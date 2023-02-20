.he
.paŠ
               APPENDIX B -- FATCAT Configuration


     {FATCAT and all the files in FATCAT23.LBR are copyright 1986
      by Steven M. Cohen and therefore remain his property.  You 
      are free to use it and distribute it freely but you may not 
      sell it or bundle it as part of another package to be sold 
      without the express written consent of the author.}

   
     This appendix should give you all the information you need 
to answer the questions posed in the FATCAT configuration module 
which is accessed by typing <P> at the FATCAT main menu.  

     A word about configuration file names.  A FATCAT 
configuration file has an extension of .CFG.  The configuration 
you expect to use most frequently should be named STANDARD.CFG 
because that is the filename FATCAT looks for by default when 
setting up a configuration.  Other configs you may desire can 
have different names with the .CFG extension.  You can load these 
in at the command line e.g.

     A> FATCAT RAMDISK

auto loads the file RAMDISK.CFG from the default area if it can 
be found.  You can also load .CFG files dynamically from within 
FATCAT by typing Ctrl-N while in the configuration module.  
FATCAT then prompts you for the name of a .CFG file which is 
loaded if possible.


     Below is information on the various options which are 
selectable from within the configuration module.


                     Editing Configurations

     Thå P¾ optioî provideó á convenienô meanó oæ changinç thå 
parameteró whicè controì FATCAT'ó differenô modeó oæ operation®  
Iî additioî iô haó facilitieó foò savinç theså configurationó tï 
disë anä foò loadinç them.

     Upoî selectinç thió optioî yoõ arå projecteä intï á 
"Database¢ typå oæ Datá Entrù Screeî wherå thå variouó 
configuratioî optionó arå showî anä caî bå changed®  Thió sorô oæ 
datá entrù wilì bå familiaò tï mosô computeò useró buô wilì bå 
explaineä brieflù here:

     Typinç regulaò ASCIÉ printablå characteró (Noô Controì 
Characters© wilì causå theí tï appeaò oî screeî iî whateveò fielä 
thå cursoò ió presentlù pointinç to¬ aó lonç aó thå characteró 
yoõ typå dï noô overflo÷ thå fielä lengtè aó indicateä bù 
underscoreó oî thå screen®  Typinç <RETURN¾ oò <TAB¾ moveó yoõ 
forwarä onå field®  Thå familiaò "Wordstar¢ cursoò diamonä ió Š.heAPPENDIX B -- FATCAT Configuration 
alsï partiallù employedº ^Å moveó yoõ bacë onå field¬ ^Ø doeó thå 
samå aó <RETURN>®  Similarly¬ ^Ó moveó thå cursoò bacë onå 
characteò withiî á fielä (withouô deleting© anä ^Ä moveó forward® 
<BACKSPACE¾ anä <DELETE¾ dï thå samå thing¬ deletå onå characteò 
back¬ anä ^Ç deleteó thå characteò aô thå cursoò position® ^Ô 
deleteó froí thå cursoò tï thå enä oæ thå field¬ anä ^Ù deleteó 
aî entirå field®  Iæ thå arro÷ keyó arå installeä (seå 
Installatioî section© theî Left-arro÷ wilì perforí thå samå 
functioî aó ^S¬ Right-arro÷ aó ^D¬ up-arro÷ aó ^Å anä down-arro÷ 
aó ^X.

Manù oæ thå fieldó arå single-characteò fields®  Oî theså iô 
ió noô necessarù tï typå thå <RETURN>¬ jusô typinç thå characteò 
advanceó tï thå nexô field®  Oî mosô oæ thå fields¬ onlù certaiî 
typeó oæ inpuô arå acceptable¬ whicè wilì bå immediatelù 
apparent¬ oò iæ not¬ indicateä oî thå screen.

Typinç Cntl-Î aô anù timå wilì resulô iî youò beinç prompteä foò 
thå namå oæ á ne÷ .CFÇ filå tï load®  Iæ FATCAÔ findó thió file¬ 
itó valueó wilì bå loadeä iî oveò anù thaô arå currentlù present.

Typinç Cntl-Ú aô anù timå telló FATCAÔ yoõ arå donå alterinç thå 
configuration®  Seå sectioî oî savinç configuratioî below®  Notå 
thaô yoõ dï noô havå tï savå configurations®  Thå <P¾ optioî maù 
alsï bå useä foò "quicë anä dirty¢ oî thå flù changes¬ sucè aó 
changinç temporarily thå drivå tï bå catalogued.

Thió ió probablù morå explanatioî thaî ió necessarù -- jusô trù 
thå <P¾ option¬ plaù witè thå keyboarä anä yoõ wilì sooî grasğ 
itó functions® 


                      Saving Configurations

Wheî yoõ arå satisfieä thaô yoõ havå á gooä configuration¬ simplù 
typå Cntl-Ú anä yoõ arå theî askeä iæ yoõ wanô tï savå thå confiç 
aó á file®  Iæ yoõ answeò 'Y§ yoõ arå theî prompteä foò á 
filenamå undeò whicè tï savå it® Iæ thå filå alreadù existó yoõ 
arå askeä whetheò yoõ meaî tï overwritå it® 

Yoõ arå theî returneä tï thå maiî menu®  Whateveò valueó yoõ havå 
choseî arå thå currenô valueó anä wilì remaiî sï unlesó yoõ 
changå theí again¬ foò thå duratioî oæ youò currenô sessioî witè 
FATCAT.

Iæ yoõ wanô tï makå thå configuratioî permanent¬ thå easiesô waù 
tï dï sï ió tï savå thå confiç undeò thå namå oæ STANDARD®  Thió 
confiç wilì bå loadeä wheneveò yoõ calì FATCAÔ subsequently¬ 
unlesó yoõ overridå iô witè á parameteò indicatinç thaô anotheò 
filå shoulä bå loaded.
.paŠ                      Configuration Options
     
Thå optionó availablå undeò configuratioî arå explaineä iî detaiì 
below:

Catalog Libraries (Y/N)
Thå defaulô valuå herå ió Ù anä wilì usuallù bå lefô thaô waù 
unlesó yoõ don'ô wanô á cataloç oæ youò librarù filå members®  Iæ 
yoõ don'ô uså librarù fileó yoõ mighô wanô tï leavå iô off®  Iæ 
Disë spacå ió á reaì probleí yoõ mighô alsï wanô iô off¬ 
otherwise¬ leavå iô on®  Oncå seô foò á particulaò catalog¬ yoõ 
shoulä leavå iô thaô way¬ oò youò librarù catalogó wilì noô bå á 
truå reflectioî oæ whaô ió oî youò disks.

Clean-Up Mode (Y/N)
Thå defaulô valuå herå ió Ù aó well®  Thió settinç caî alsï bå 
changeä froí thå maiî menõ usinç thå T¾ optioî whicè simplù 
toggleó iô oæ anä off¬ buô changinç iô herå wilì givå iô thå 
initiaì valuå yoõ desire®  Yoõ woulä wanô iô oî especiallù foò 
cataloguinç ne÷ diskó wherå yoõ maù havå junë fileó you'ä bå 
betteò ofæ gettinç riä of¬ oò foò addinç thå zero-lengtè disk-
namå fileó thaô FATCAÔ needó tï dï itó work®  Turninç iô ofæ wilì 
yielä á substantial speeä increase.


ReIndex Mode
Iæ yoõ havå lotó oæ timå tï sparå anä anä don'ô minä lettinç youò 
computeò churî awaù foò á biç chunë oæ timå (coulä bå oveò aî 
houò witè 500°  fileó oî á floppù system© whilå yoõ dï somethinç 
else¬ theî selecô 'Y§ herå tï leavå thå reindexinç togglå on®  
Thå advantagå herå ió thaô thå cataloç fileó arå remadå ane÷ froí 
thå indeø files¬ whicè decreaseó thå dangeò oæ fileó becominç 
corrupted®  
        
Mosô peoplå wilì probablù concludå thaô thió ió jusô toï slo÷ anä 
prefeò tï leavå iô off®  Iî thaô case¬ iô mighô noô bå á baä ideá 
tï bacë youò cataloç diskó uğ ontï otheò diskó wheî through®  Iî 
fact¬ that'ó neveò á baä idea.

Maximum User
Numeriã inpuô requireä here¬ thå highesô useò areá yoõ wilì 
cataloguing® Shoulä bå seô tï thå highesô useò areá youò 
operatinç systeí caî handle¬ typicallù 1µ buô 3± iî somå ZCPR³ 
implementations®  Iæ yoõ arå surå yoõ won'ô uså thå higheò 
numbers¬ gï aheaä anä seô iô lower¬ wilì makå thå prograí gï 
fasteò oî hard-disë cataloguinç (seå optioî H>)® Iî facô herå ió 
onå instancå wherå á differenô confiç filå foò Harä Diskó mighô 
bå verù useful.

Anotheò ideá herå woulä bå foò uså oî remotå systemó where¬ saù 
thå onlù accessiblå areaó woulä bå thoså oî useò areaó 0-7®  Iî 
thaô case¬ foò preparinç thå cataloç tï bå displayeä tï remotå 
users¬ seô Maximuí useò tï 7¬ anä yoõ wilì winä uğ onlù 
cataloguinç thoså useò areá yoõ wanô youò remotå useró tï see.
ŠDate Stamping
Defaulô valuå ió Î anä leavå iô thaô waù unlesó yoõ uså thå 
Plu*Perfecô DateStamper®  Iæ turneä on¬ thió changeó FATCAT'ó 
assumptionó abouô wherå iî thå lisô oæ fileó thå disë namå is¬ sï 
aó noô tï conflicô witè DateStamper'ó assumptioî thaô itó speciaì 
filå ió thå first®  However¬ nï featureó oæ thå DateStampeò arå 
supporteä bù FATCAÔ -- don'ô looë foò anù datå stampinç iî thå 
cataloç listings®  Therå ain'ô any.
 
User Area to be Catalogued  (32 = Catalog All User Areas)
Thió requireó á littlå morå comment®  Normallù seô thió tï 3² anä 
leavå iô there®  3² doeó noô meaî useò 32¬ buô telló FATCAÔ tï 
procesó alì useò areaó aô once®  Anù numbeò lesó thaî 3² telló 
FATCAÔ tï procesó ONLY thaô useò number®  Dï noô confuså witè 
Maximuí Useò whicè giveó thå highesô useò areá tï catalog®  Therå 
shouldn'ô ofteî bå reasoî tï changå this¬ buô iô ió includeä 
becauså someonå wilì finä iô useful.

Printer Offset
Iæ yoõ arå someonå whï likå tï puô thå papeò iî thå printeò anä 
neveò movå thå tractoró froí theiò usuaì setting¬ theî yoõ maù 
selecô á numbeò froí ° tï 20¬ anä wheî printinç youò catalog¬ thå 
printeò heaä wilì skiğ thaô manù spaceó beforå printinç oî á ne÷ 
line®  Experimenô anä finä youò besô settinç here.

Root File Name for FATCAT Files
Thió establisheó whaô rooô namå youò FATCAÔ cataloç fileó wilì 
have® Alì Fatcaô fileó foò á particulaò cataloç seô wilì havå thå 
samå rooô name» onlù theiò filå typeó wilì bå differenô (anä 
theså arå seô automaticallù bù thå program)® Defaulô valueº 
MASTER®  FATCAÔ fileó wilì thuó havå nameó likå MASTER.LCØ anä 
MASTER.RIX¬ etc®  Alì fileó iî á cataloç seô caî thuó bå easilù 
transferreä usinç familiaò cp/í wildcarä techniqueó i.e.

                 A>PIP B:=A:MASTER.*



FILE PLACEMENT CHOICES

Drive to be Catalogued
Program Files on Drive:
Index File for Regular Files on Drive      (.RIX)
Index File for Library Files on Drive      (.LIX)
Catalog File for Regular Files on Drive    (.RCX)
Catalog File for Library Files on Drive    (.LCX)
Index File for Disk Names on Drive         (.DNX)
Temporary List File on Drive               (.TCX)


Theså optionó controì thå locationó oæ thå variouó files® Yoõ 
configurå theså tï youò system®  Belo÷ arå somå samplå 
configurations®  Uså theså aó á guide» quitå likelù you'lì bå 
ablå tï comå uğ witè á betteò confiç foò youò systemº Capacitieó 
arå verù rougè estimates¬ dependinç largelù oî thå numbeò oæ Šlibrarù fileó yoõ arå counting¬ aó librarù fileó takå uğ 50¥ morå 
rooí thaî regulaò fileó iî thå catalogs.
.cp20
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

.cp20Š
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



.cp20ŠSYSTEM 5 

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