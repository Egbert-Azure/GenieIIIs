
 
 ; Ab hier stehen ALIASE, mit denen der HI-TECH-C Compiler, der in C9:HI-TEC
 ; liegt, einfacher erreicht werden kann. Die Sources der C-Programme sollen 
 ; in einem anderen Userbereich zu liegen kommen, n[mlich in C11:CSOUR
 ; Vor der LIBC.LB wird die HOLTE.LIB durchsucht.  

CC    	C9:C -i9:c: $1.c 9:c:holte.lib $-1
CCF	C9:C -i9:c: $1.c 9:c:holte.lib $-1 -lf
