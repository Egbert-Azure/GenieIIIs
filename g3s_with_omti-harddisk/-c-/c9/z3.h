/*
 * include file for Z3 and HI TECH C
 *   the following routines only work if you use the modified CRTCPM.OBJ
 *   in your linking (which includes the definition of envptr) and are
 *   running Z3.  They only return valid results if z3vinit() returns
 *   a non-NULL value.
 */

extern void  *  	z3vinit(void);		/* initialize z3/C Vidlib */
extern unsigned char	cls(void);		/* clear screen */
extern unsigned char	ereol(void);		/* erase to end of line */
extern unsigned char	gotoxy(int, int);	/* move cursor */
extern unsigned char	tinit(void);		/* initialize terminal */
extern unsigned char	dinit(void);		/* deinitialize terminal */
extern unsigned char	stndout(void);		/* enter standout mode */
extern unsigned char	stndend(void);		/* end standout mode */

struct z3env {
	unsigned char	fill0;		/* the JP */
	void *		cbios;		/*    ... to the BIOS */
	unsigned char	id[5];		/* "Z3ENV" */
	unsigned char	type;		/* hi-bit (0x80) -> extended env */
	void *		expath;		/* external path address */
	unsigned char	expaths;	/*    "      "   size */
	void *		rcp;		/* RCP address */
	unsigned char	rcps;		/*     size */
	void *		iop;		/* IOP address */
	unsigned char	iops;		/*     size */
	void *		fcp;		/* FCP address */
	unsigned char	fcps;		/*     size */
	void *		z3ndir;		/* NDR address */
	unsigned char	z3ndirs;	/*     size */
	char *		z3cl;		/* command line */
	unsigned char	z3cls;		/*     size */
	struct z3env *	env;		/* recursion! */
	unsigned char	envs;		/*     size */
	char *		shstk;		/* shell stack */
	unsigned char	shstks;		/*     # of entries in stack */
	unsigned char	shsize;		/*     size of each entry */
	void *		z3msg;		/* z3 message buffer */
	void *		extfcb;		/* external FCB */
	void *		extstk;		/* external stack */
	unsigned char	quiet;		/* Quiet flag */
	unsigned char * wheel;		/* Wheel byte */
	unsigned char   speed;		/* system speed in MHz */
	unsigned char	maxdrv;		/* max drive */
	unsigned char	maxuser;	/* max user */
	unsigned char	duok;
	unsigned char	fill1;		/* was CRT selection */
	unsigned char	printer;	/* printer selection */
	unsigned char	crtwidth;
	unsigned char	crtlines;
	unsigned char	textlines;
	unsigned int	drives;		/* valid drive vector */
	unsigned char	fill2;
	struct {
		unsigned char width;
		unsigned char lines;
		unsigned char text;
		unsigned char ff;
	} print[2];
	void * 		ccp;		/* ZCPR 3.x start */
	unsigned char	ccps;
	void *		dos;
	unsigned char	doss;
	void *		bios;
	char		sysfile[5][11];	/* sysfile[0] is the sh.var file */
};

extern struct z3env * envptr;

#define isquiet()	(envptr->quiet)
#define iswheel()	(*envptr->wheel)
