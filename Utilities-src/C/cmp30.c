
/* 

CMP.C - a text or binary file compare program
Original author: Leor Zolman

Update history:

v 2.7 - 11/23/92 - Lee Bradley - Converted to BDS Z.
v 2.8 - 11/24/92 - Lee Bradley - Program uses tcap if available.
v 2.9 - 11/24/92 - Lee Bradley - Externalized two variables. Get name from 
                                 external fcb.
v 3.0 - 11/25/92 - Lee Bradley - Fixed external fcb code.

*/

#define CTRLC 0x3
#define VERS "3.0"

#include <stdio.h>

int adrs1,adrs2,sadr;                    /* file indexes */
int binary;                              /* for binary compares */
char scode[25],ir[3],extname[9];         /* global arrays */

main(argc,argv) int argc; char *argv[]; {

char *ptr;
FILE *mf,*cf;
int *z3envp,*efcbp,*zwp,zwpval,i;
char *z3env,*efcb;
char *scodew;

adrs1=adrs2=0;
binary=FALSE;

/* set up clear screen and usage display */

z3envp = 0x109;
z3env = *z3envp;
zwp = z3env + 0x1b;
zwpval = *zwp;
if (zwpval == z3env) {                   /* if Z-System found */
  efcbp = *z3envp + 0x24;                /* get external fcb address */
  efcb = *efcbp;
  i=1;
  while ((*(efcb+i)&0x7f)!=' ' && i<9) { /* extract name used to invoke */
    extname[i-1]=*(efcb+i)&0x7f;
    i++;
    }
  extname[i-1]='\0';
  scodew = z3env + 0x97;                 /* get clear screen string from tcap */
  strcpy(scode,scodew);
  strcpy(ir,"ir");                       /* and tune usage string to dir: */
   }  
else {
  strcpy(extname,"CMP");                  /* otherwise, use CMP */
  *ir = '\0';                             /* tune usage string to d: */
  i=24; while(--i) *(scode+i) = '\n';     /* and 24 line feed clear screen */
  *(scode+24) = '\0';
}

if (argc < 3 || argc > 5)
  i_error();
else if ((mf = fopen(argv[1],"r")) == NULL)
  printf("No such file %s\n",argv[1]);
else if ((cf = fopen(argv[2],"r")) == NULL)
  printf("No such file %s\n",argv[2]);
else {
  while (argc > 3) {
    ptr=argv[--argc];
    if (*ptr++ != '-')
      i_error();
    switch (tolower(*ptr++)) {
      case 'b': binary=TRUE;adrs1=hextoi(ptr);
                fclose(mf);fclose(cf);
                mf = fopen(argv[1],"rb");
                cf = fopen(argv[2],"rb"); 
                break;
      default: printf("Unrecognized option. Aborted.\n");
      i_error();
      } /* end switch */
    } /* end while */
  sadr=adrs2=adrs1;
  printf("%s",scode);
  while (TRUE) fcompare(mf,cf,argv);
  } /* end else */
exit();
} /* end main */

kbhit() {
return bdos(11,0xff);
} /* end kbhit */

fcompare(mfile,cfile,argv) FILE *mfile,*cfile; char *argv[]; {

int mc,cc;                 /* 1 char buffers */
char diff,erflg;           /* error flags */

char str1[6],str2[6];      /* temporaries for strings */
char xlate[10];            /* string used in ascii control char translation */
int lp;                    /* lines printed ... used to freeze screen */
char ss[40];               /* holds string for file sync'ing */

diff=lp=erflg=0;

while ((mc=getc(mfile)) != EOF) {
  if ((cc=getc(cfile)) == EOF) {
    printf("\n %s's end reached.\n",argv[2]);
    restart(mfile,cfile,argv);
    return;
    } /* end if */
  else {
   mc &= 0x00ff; cc &= 0x00ff; /* mask out high byte */
   if (mc != cc) {
      if (!erflg) {
        diff=1;
        erflg=1;
        if (!lp) {
          printf("\n          %-14s               %-14s               ",
                              argv[1],            argv[2]);
          printf("\nAddr      Hex   Char         Addr      Hex   Char   \n");
          printf("----      ---   ----         ----      ---   ----\n\n");
          }
        } /* end if */

        strcpy(str1,xl(mc,xlate)); /* Fudge because parameters are */
        strcpy(str2,xl(cc,xlate)); /* evaluated before being passed */

        printf("%4x       %2x    %4s        %4x       %2x    %4s\n"
               ,adrs1    ,mc    ,str1      ,adrs2    ,cc    ,str2);

        if (++lp == 16) {
          printf("\nCMP v%s: press 1..4, ^C, <ret>, / for help -> ",VERS);
          while(!kbhit()) ;
            switch (toupper(getchar())) {
            
            case  CTRLC : exit();

            case '1' : putchar('\b');putchar(' ');
                       printf("\n\n");
  		       ungetc(cc,cfile);adrs2--;
                       lp=15;
                       break;

            case '2' : putchar('\b');putchar(' ');
                       printf("\n\n");
                       ungetc(mc,mfile);adrs1--;
                       lp=15;
                       break;

            case '3' : putchar('\b');putchar(' ');
		       printf("\nEnter string -> ");
                       gets(ss);
                       if (reseq(ss,mfile,cfile,argv)) return;
                       printf("\n");
                       lp=15;
                       erflg=0;
                       break;

            case '4' : restart(mfile,cfile,argv);
                       lp=erflg=0;
                       printf("%s",scode);
                       return;
                       break;

            case '?' :
            case 'H' :
            case '/' : printf("%s",scode);  
            putchar('\b');putchar(' ');
            printf("\n  1 / 2 : advance file 1 / file 2 by a character.\n");
            printf("  3 : resequence based on search string.\n");
            printf("  4 : restart.\n");
            printf("\nPress <ret> ");
            while (!getchar());
            printf("%s",scode);
            lp=erflg=0;
            break;

            case '\n':
            default:  printf("%s",scode);  
                      lp=erflg=0;
            }
          } /* end if */
    } /* end if mc != cc */
    else {
      erflg=0;
    }
  } /* end else */

adrs1++;
adrs2++;

} /* end while */

if ((cc=getc(cfile)) != EOF) {
  printf("\n %s's end reached.\n",argv[1]);
  restart(mfile,cfile,argv);
  return;
  }
else {
  if (!diff)
    printf("\nFiles are identical.\n");
  else
    if (!erflg)
      printf("\nFiles agree after last match.\n");
  exit();
  }
} /* end fcompare */

hextoi(string) char *string; {

int number;
char c;

number=0;
c=tolower(*string++);
while (isalpha(c) || isdigit(c)) {
  if (c > 'f')
    return number;
  number *= 16;
  if (isdigit(c))
    number+=c-'0';
  else
    number+=c-'a'+10;
  c=tolower(*string++);
  }
  return number;
} /* end hextoi */

i_error() {

printf("\nUsage: %s {d%s1:}fn1.tp1 {d%s2:}fn2.tp2 {-bhexnum}",extname,ir,ir);
printf("\nOption -b used for binary files. hexnum is hex load address\n");
exit();

} /* end i_error */

xl(c,xlate) int c; char *xlate; {

if (c > 0x7f || c < 0)
  strcpy(xlate,"<?> ");
else if (c == 0x7f)
  strcpy(xlate,"del ");
else if (c > 0x1f) { /* Then it's printable */
  xlate[0]=c;
  strcpy(xlate+1,"   ");
  }
else
  switch (c) {
    case 0x7: strcpy(xlate,"bel ");
    break;
    case 0x8: strcpy(xlate,"bs  ");
    break;
    case 0x9: strcpy(xlate,"tab ");
    break;
    case 0xa: strcpy(xlate,"lf  ");
    break;
    case 0xc: strcpy(xlate,"ff  ");
    break;
    case 0xd: strcpy(xlate,"cr  ");
    break;
    case 0x1b: strcpy(xlate,"esc ");
    break;
    default:xlate[0]='^'; /* show control chars as  */
      xlate[1]=c+0x40;    /* ^<char> e.g. ^A is     */
      xlate[2]=' ';
      xlate[3]=' ';
      xlate[4]='\0'; /* control A */
    break;
    }
return xlate;
} /* end xl */

/* 

Advance pointers if possible in two files by comparing the string passed to 
each file. The intent of this module is to get out of synch files back in 
synch.

*/

reseq(strng,b1,b2,argv) char *strng; FILE *b1,*b2; char *argv[]; {

int flg1,flg2;
int i;

flg1=flg2=TRUE;

if (!(i=fix(b1,strng))) {
  printf("\nCouldn't find %s in %s.\n",strng,argv[1]);
  }
else {
  adrs1+=i;
  flg1=FALSE;
  }
if (!(i=fix(b2,strng))) { 
  printf("\nCouldn't find %s in %s.\n",strng,argv[2]);
  }
else {
  adrs2+=i;
  flg2=FALSE;
  }
if (flg1 || flg2) restart(b1,b2,argv);
return(flg1 || flg2);

} /* end reseq */

/* 

Look for str in buf and return the number of bytes advanced in file.  
Return 0 if not found.

*/

fix(bf,str) char *str; FILE *bf; {

int wptr;
int c;         
char wstr[20];
int i;

wptr=0;

/* Load up wstr with first strlen(str) bytes from file */

for (i=0; i < strlen(str); i++) {
  *(wstr+i)=(c=getc(bf));
  }

/* Tack on required null for strings */

*(wstr+i)='\0';

wptr=i;

/* 

While you can't find it, advance byte by byte.

This is done by shifting the 2nd thru strlen(str) bytes in wstr and then 
plugging the end with the next byte in the file buffer.

*/

while (strcmp(str,wstr)  && ((c=getc(bf)) != EOF)) {
  for (i=0; i < strlen(str); i++) {
    *(wstr+i)=*(wstr+1+i);
    }

  wptr++;
  *(wstr+i-1)=c;

  } /* end while */

if (c == EOF)
  return (FALSE);
else
  return (wptr);

} /* end fix */

restart(b1,b2,argv) FILE *b1,*b2; char *argv[]; {

fclose(b1); if (binary)
     b1 = fopen(argv[1],"rb");
else b1 = fopen(argv[1],"r");
fclose(b2); if (binary)
     b2 = fopen(argv[2],"rb");
else b2 = fopen(argv[2],"r");
adrs1=adrs2=sadr;
} /* end restart */