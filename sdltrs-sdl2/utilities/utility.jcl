. Note: Use TRSDOS 6.2.1 to get old-style dates and
. thus avoid password problems with other DOSes.
format :#d#(sden,sides=1,cyl=80,dir=17,name="xtrsutil",q=n,abs)
import export.cmd export/cmd:#D#
import -n export.z80 export/z80:#D#
import import.cmd import/cmd:#D#
import -n import.z80 import/z80:#D#
import -n settime.z80 settime/z80:#D#
import settime.cmd settime/cmd:#D#
import -n xtrsemt.ccc xtrsemt/ccc:#D#
import -n xtrsemt.h xtrsemt/h:#D#
import -n settime.ccc settime/ccc:#D#
import -n m1format.fix m1format/fix:#D#
import xtrshard.dct xtrshard/dct:#D#
import -n xtrshard.z80 xtrshard/z80:#D#
import xtrs8.dct xtrs8/dct:#D#
import -n xtrs8.z80 xtrs8/z80:#D#
import xtrsmous.cmd xtrsmous/cmd:#D#
import -n xtrsmous.z80 xtrsmous/z80:#D#
import -n cd.ccc cd/ccc:#D#
import -n pwd.ccc pwd/ccc:#D#
import -n unix.ccc unix/ccc:#D#
import -n mount.ccc mount/ccc:#D#
import -n umount.ccc umount/ccc:#D#
import cd.cmd cd/cmd:#D#
import pwd.cmd pwd/cmd:#D#
import unix.cmd unix/cmd:#D#
import mount.cmd mount/cmd:#D#
import umount.cmd umount/cmd:#D#
import truedam.cmd truedam/cmd:#D#
import cd6.cmd cd6/cmd:#D#
import pwd6.cmd pwd6/cmd:#D#
import unix6.cmd unix6/cmd:#D#
import mount6.cmd mount6/cmd:#D#
import umount6.cmd umount6/cmd:#D#
import truedam6.cmd truedam6/cmd:#D#
import -n expall.bas expall/bas:#D#
import -n do6.jcl do6/jcl:#D#
