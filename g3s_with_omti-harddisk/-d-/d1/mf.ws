					  #####
					########
				       ###     #
				      ###
		 ###		      ###    #
		 ###########	  ###########
		 ############	 ###########
		 ###  ###  ###	#     ###
		 ###  ###  ###	      ###
		 ###  ###  ###	      ###
		 ###  ###  ###	      ###
		 ###  ###  ###	      ###
		      ###  ###	      ###
			   ###	     ###


	      A multi-format disk handler for CP/M+

		    Public-domain software by

			   Jon Saxton
			  P.O. Box 242
			 Dural, NSW 2158
			    AUSTRALIA
			 +61 2 654 1945
			 +61 2 654 1271



THE  SOFTWARE ACCOMPANYING THIS DOCUMENT IS PUBLIC DOMAIN AND  IS
FREE FROM ANY COPYRIGHT RESTRICTIONS.	IT MAY BE USED AS YOU SEE
FIT.   IT MAY BE FREELY COPIED AND DISTRIBUTED IN ANY  FORM.   IF
YOU  USE THIS SOFTWARE TO DEVELOP A MULTI-FORMAT DISK HANDLER FOR
ANY PARTICULAR HARDWARE CONFIGURATION THEN PLEASE DISTRIBUTE  THE
ENTIRE	SOURCE CODE FOR MF AND THE BIOS SO THAT OTHERS MAY  LEARN
AND/OR MAKE IMPROVEMENTS.

This  suite of programs was developed by me for my  own  purposes
and  for  my own hardware.  MF was written in Hi-Tech C  and  its
associated  Z80  assembler;  the  BIOS modules were  written  for
assembly  with	M80  and RMAC.	 MF may  be  adaptable	to  other
hardware and software but I make no promises.  I am providing the
complete  source code for the disk handler and for those portions
of the BIOS which are not subject to third-party copyright so you
can make up your own mind.

The hardware which I used with MF is:

     Computer:		      Colex 850
     Processor: 	      Z80A
     Floppy disk controller:  WD1797
     Floppy disk drives:      1 x 80 cyl 5.25" (800K)
			      1 x 40 cyl 5.25" (400K)
			      1 x 77 cyl 8"   (1200K)

Other  features such as hard disk,  memory configuration and  STD
bus are not particularly relevant.   At times I have connected an
80-cylinder  3.5" drive in place of the 8".   I also have one  of
those  AT-style 5.25" drives but I have not connected it  to  the
Colex  yet as I have not finished building the adaptor which will
let me have more than three drives connected to the FDC card.

If  your  computer  does not have a Western Digital  floppy  disk
controller then adapting MF will not be easy.

The software is:

     Operating system:	      CP/M Plus
     C compiler:	      Hi-Tech Z80
     Assembler(s):	      Microsoft's M80 and D.R.'s RMAC
			      or SLR System's equivalents

MF was written because there is no commercial (or  public-domain)
package  to  handle  multiple disk formats which works	for  CP/M
Plus.

The  commercial  programs are tailored to a particular	brand  of
computer  because  any multi-format disk handler  needs  intimate
knowlege  of  the BIOS's method for handling double-sided  disks.
MF is no exception but since I wrote the floppy disk drivers  for
my system with MF in mind I was unconstrained by a pre-cast BIOS.

All  the  commercial programs work by overlaying  the  BIOS  disk
parameter tables.   On a banked CP/M Plus system some of the disk
parameter tables live in the system bank and are not visible from
the TPA so that approach will not work.  To overcome that problem
I implemented an extra BIOS routine specifically for use by MF.

Implementing MF requires modifying the BIOS.   If you do not have
the  source  code for your BIOS then that task will be	extremely
difficult.   I don't have a lot of sympathy for you in that case.
In  the  days of CP/M 2.2 we all learned a simple  rule:  If  the
source	code  for  the	BIOS is not available,	DO  NOT  BUY  THE
COMPUTER!

The three most popular brands of computer running CP/M Plus would
probably be Commodore C128, Epson QX-10 and Osborne Executive.	A
BIOS  source for the C128 is in the public domain.   There  is	a
well-organised	user group (FOG) for Osborne users and I  imagine
the  BIOS  source is available via that channel.   I  don't  know
about the Epson box or any of the others but if you know what you
are doing,  you could probably disassemble a banked BIOS in a few
days of concerted effort,  especially if the BIOS follows Digital
Reasearch's published model.   (Warning: Bondwell 14 does not and
I suspect that Morrow MD4/10 doesn't either.)  If you are  really
keen,  you could even implement a BIOS from scratch.   That's not
as silly as it might seem at first; I've done it.

MF can do three basic things:  format a diskette,  read an entire
track from a diskette and display it on the screen,  and assign a
chosen	format to a particular disk drive.   Of these,	only  the
last requires the cooperation of the BIOS;  formatting and  track
examination are independent operations.

Assigning a format to a drive alters the disk parameter tables in
the  BIOS such that the selected format becomes indistinguishable
from  a native format.	 The selected format  remains  associated
with  the drive until the system is re-booted or until MF is used
to  assign  a  different format.   In the  meantime  the  foreign
diskette is useable for all CP/M applications.

I guess the first step in implementing MF would be to tailor  the
screen control strings in MF2.C to match your own terminal.   The
code  is  fairly  well commented so that should present  no  real
challenge.   The only thing to watch out for is that the terminal
control strings are in length-prefix form,  i.e.  the first  byte
holds a count of the characters which follow.

Next adjust the end bit of MFTBL.C, setting the values of loDrive
and  hiDrive to conform with your floppy drive setup.	Note that
MF assumes that the floppy disk drives occupy a contiguous set of
drive identifiers,  e.g. EFG, AB, or KLMN.  Broken ranges such as
AC  or FGJL are not really supported but will probably be  OK  if
you  are careful:  assigning a new format to a hard disk or to	a
non-existent drive is likely to cause undesirable effects.

Just  after  loDrive  and hiDrive is a short array  dkTypes[]  of
drive types.  The current version of MF knows about the following
disk types:-

     EIGHT	    8-inch, 77 cylinder
     FIVE_80	    5.25" (or 3.5"), 80 cylinder
     FIVE_40	    5.25" 40 cylinder

Obviously,  the values in dkTypes[] should match your floppy disk
drive  configuration.	Note  that  the  number  of  elements  in
dkTypes[]  which  are  actually used depends  on  the  difference
between loDrive and hiDrive.   dkTypes[] should at least  contain
one  entry for each floppy disk drive.	 It may be longer but  it
must not be shorter.

Now  it is time to tackle the assembly-language module,  MFIO.AS.
If  you happen to have a WD179X floppy disk controller AND a  Z80
DMA  controller  in  your system then the  changes  required  are
likely	to be minimal.	 If you do not have a DMA controller then
rip  out all the DMA-related code and replace it with  programmed
I/O  loops.

I mentioned earlier that if your computer uses a different floppy
disk  controller chip,	e.g.  an Intel 8272 or the NEC equivalent
uPD765, then your task is much harder.	Although in all cases you
should be able to copy some code from your BIOS,  the  formatting
process  is quite different and you will also need to modify  the
track set up routines in MF3.C.   In particular, the 8272/765 has
a  "format  track" rather than a "write track" command	and  uses
multi-byte  command and status blocks which have to be set up and
then  written or read (completely) and interpreted.

Even  if your computer does have a WD179X floppy disk  controller
you  should still be aware of the differences between the WD179X-
series chips.  For example, side selection is done "on-chip" with
the 1797 but via external logic in the case of the 1793.   On  my
Colex  computer an external register at FDCbase+3 controls 8"  vs
5.25"/3.5" data rate selection,  FM vs MFM recording (i.e. single
vs  double density),  FDC reset,  the "motor on" signal for 5.25"
drives, and actual disk drive selection.

If  by	now you have not given up then it should be  possible  to
recompile  MF  and  use it to  format  diskettes  and/or  examine
individual tracks at the "raw data" level.   The next task is  to
integrate the format selection routines into your BIOS.

I  have  supplied  the source code to several  bios  modules  but
because  of  copyright	considerations	I  have  not  supplied	a
complete  BIOS	source.   The BIOS follows the	Digital  Research
model fairly closely; the only (slight) deviation is that I split
the floppy disk data definitions out from the code module because
I  wanted  to write the code in Zilog mnemonics and I found  that
M80's  internal  arithmetic failed when  evaluating  the  complex
macros	for declaring the disk parameter tables.   The major BIOS
routines  which I have NOT supplied are CHARIO.Z80 and	the  hard
disk driver module, HDISK.ASM.

Some definitions:

     TRACK	    That portion of a diskette which is  accessed
		    by a single, stationary read/write head.

     SURFACE	    All the tracks on one side of a diskette.

     CYLINDER	    For two-sided diskettes, a cylinder comprises
		    the  pair of tracks accessible without moving
		    the read/write heads.  Same as a track in the
		    case of a single-sided diskette.

Beware of terms such as "80-track diskette drive".   That USUALLY
means  80  cylinders (160 tracks) but some writers adopt  a  more
literal usage so that the term could mean a 40-cylinder two-sided
drive or an 80-cylinder single-sided one.

MF  and  the BIOS disk driver support four  methods  of  handling
double-sided diskettes:

     CYLINDER	    Even numbered tracks (counting from zero) are
		    on side 0, odd-numbered tracks are on side 1.

     LONG TRACK     The track on side 1 is concatenated with  the
		    track on side 0 to make a single, long track.

     EXTENDED	    The  diskette  is treated as a large  single-
     SURFACE	    sided disk;  all of side 0 is used before any
		    of side 1.

     SERPENTINE     Similar  to Extended Surface except that  the
		    tracks  on	side 1 are  utilised  in  reverse
		    order.  The diskette is filled from the outer
		    tracks  on side 0 to the inner tracks on side
		    0 then from the inner tracks on side 1 to the
		    outer tracks on side 1.

The BIOS driver uses an extended disk parameter header similar in
structure  to that suggested by Digital Research in the CP/M Plus
System Guide, except that I have extended the XDPH a little more.


			Lo	  Hi
		     ___________________
		    |	      | 	|   Method of handling
     XDPH-12	    | 2S Mthd | Tracks	|   double-sided disks,
		    |_________|_________|   Tracks per disk.
		    |			|   Address of sector
     XDPH-10	    |	    Write	|   write routine for
		    |___________________|   the drive.
		    |			|   Address of sector
     XDPH-8	    |	    Read	|   read routine for
		    |___________________|   the drive.
		    |			|   Address of disk
     XDPH-6	    |	    Login	|   log-in routine
		    |___________________|   for the drive.
		    |			|   Address of first-
     XDPH-4	    |	    Init	|   time initialisation
		    |___________________|   routine for drive.
		    |	      | 	|   Controller-relative
     XDPH-2	    | Rel Drv |  Type	|   drive number, disk
		    |_________|_________|   type & select bits.
		    |			|   Address of physical
     XDPH	    | Translate Table	|   sector translate
		    |___________________|   table (skew table).
		    |			|
     XDPH+2	    |	   scratch	|   Nine bytes of scratch
		    |___________________|   area used by BDOS.
		    |			|
     XDPH+4	    |	   scratch	|
		    |___________________|
		    |			|
     XDPH+6	    |	   scratch	|
		    |___________________|
		    |			|
     XDPH+8	    |	   scratch	|
		    |___________________|
		    |	      | 	|   Removable medium
     XDPH+10	    | scratch |  RMF	|   flag (not used by
		    |_________|_________|   this BIOS).
		    |			|   Address of Disk
     XDPH+12	    |	     DPB	|   Parameter Block
		    |___________________|   for the drive.
		    |			|   Address of scratch
     XDPH+14	    |	     CSV	|   area used to detect
		    |___________________|   changed disks.
		    |			|   Address of allocation
     XDPH+16	    |	     ALV	|   vector.
		    |___________________|
		    |			|   Address of directory
     XDPH+18	    |	   DIRBCB	|   buffer control block
		    |___________________|   list.
		    |			|   Address of data
     XDPH+20	    |	   DTABCB	|   buffer control block
		    |___________________|   list.
		    |			|   Address of directory
     XDPH+22	    |	    HASH	|   hashing table.
		    |___________________|
		    |	      | 	    Number of the bank
     XDPH+24	    | HBANK   | 	    where the directory
		    |_________| 	    hash table resides.


My additions are the two bytes at XDPH-12 and XDPH-11.	The first
tells the BIOS how translate track numbers while the second tells
it how many tracks are on the disk.  For cylinder-mode, extended-
surface  and  serpentine recording the number of tracks is  twice
the number of cylinders.  For long-track and single-sided formats
the  number  of tracks is the same as the  number  of  cylinders.
Both of these values are filled in by MF.COM with the cooperation
of BIOS function 30 (USERF).

The "type" byte at XDPH-1 is a composite thing.

 _______________________________________________________________
|	|	|	|	|	|	|	|	|
| SDENS | RESET |  8/5" |	|  MTR	|  DS2	|  DS1	|  DS0	|
|_______|_______|_______|_______|_______|_______|_______|_______|
    7	    6	    5	    4	    3	    2	    1	    0

SDENS is set to 1 to select FM (single-density) recording instead
of MFM (double-density) recording.  When the RESET bit is low (0)
the WD1797 will supposedly be reset but I have never fiddled with
that signa.   The 8/5" bit is set to 0 for an 8" data rate (2Mhz)
or to 1 for a 5" data rate (1Mhz).  Bit 4 is unused, bit 3 is the
"motor	on"  bit  for  5.25" drives and the rest  are  for  drive
selection.  (An astute observer may notice that DS3 is missing.)

These  bit assignments reflect those of the drive select register
in my computer and I mention them only so that you may understand
what is going on in the BIOS.

Well  short  of  writing  a  full  implementation  manual  and/or
duplicating most of the CP/M Plus System Manual that is about all
there is for me to say.

Good luck, and happy multi-formatting!
