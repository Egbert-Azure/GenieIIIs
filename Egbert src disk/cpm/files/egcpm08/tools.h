/*******************************************************************************
*  T O O L S  *  G R E P 0 0 0	*  T h o m a s	 H o l t e   *	 8 6 0 9 0 9   *
********************************************************************************
*
* Version 1.0 by Thomas Holte
*/

/*------------------------------------------------------------------------------
*		TOOLS.H: Various #defines and typedefs for GREP
*
*		       Copyright (c) 1984 Allen Holub
*	    Copyright (c) 1984 Software Engineering Consultants
*			       P.O. Box 5679
*			    Berkeley, CA, 94705
*
*			    All rights reserved
*
*	This program may be copied for personal,  non-commercial use
*	only, provided that this copyright notice is included in all
*	copies	and  that  this  program is not modified in any way.
*	Copying for any other use without previously  obtaining  the
*	written permission of the author is prohibited.
*
*	Machine readable versions of  this  program may be purchased
*	for $35 from  Software	Engineering  Consultants.  Supported
*	disk formats are CP/M 8" SS/SD and PCDOS (v2.x) 5-1/4" DS/DD.
*
*-----------------------------------------------------------------------------*/

/* #defines for non-printing ASCII characters */
#define NUL    0x00			/* ^@  */
#define SOH    0x01			/* ^A  */
#define STX    0x02			/* ^B  */
#define ETX    0x03			/* ^C  */
#define EOT    0x04			/* ^D  */
#define ENQ    0x05			/* ^E  */
#define ACK    0x06			/* ^F  */
#define BEL    0x07			/* ^G  */
#define BS     0x08			/* ^H  */
#define HT     0x09			/* ^I  */
#define LF     0x0A			/* ^J  */
#define NL     LF
#define VT     0x0B			/* ^K  */
#define FF     0x0C			/* ^L  */
#define CR     0x0D			/* ^M  */
#define SO     0x0E			/* ^N  */
#define SI     0x0F			/* ^O  */
#define DLE    0x10			/* ^P  */
#define DC1    0x11			/* ^Q  */
#define DC2    0x12			/* ^R  */
#define DC3    0x13			/* ^S  */
#define DC4    0x14			/* ^T  */
#define NAK    0x15			/* ^U  */
#define SYN    0x16			/* ^V  */
#define ETB    0x17			/* ^W  */
#define CAN    0x18			/* ^X  */
#define EM     0x19			/* ^Y  */
#define SUB    0x1A			/* ^Z  */
#define CPMEOF SUB
#define ESC    0x1B			/* ^[  */
#define FS     0x1C			/* ^\  */
#define GS     0x1D			/* ^]  */
#define RS     0x1E			/* ^^  */
#define US     0x1F			/* ^_  */
#define DEL    0x7F			/* DEL */

/*	Definitions of meta-characters used in pattern matching routines.
 *	LITCHAR & NCCL are only used as token identifiers; all the others
 *	are also both token identifiers and the actual symbol used in
 *	the regular expression.
 */

#define BOL	'^'
#define EOL	'$'
#define ANY	'.'
#define LITCHAR 'L'
#define ESCAPE	'\\'
#define CCL	'['                     /* Character class [...]           */
#define CCLEND	']'
#define NEGATE	'^'
#define NCCL	'!'                     /* Negative character class [^...] */
#define CLOSURE '*'
#define OR_SYM	'|'

#define CLS_SIZE 128		/* Largest permitted size for an expanded
				 * character class. (I.e. the class [a-z] will
				 * expand into 26 symbols; [a-z0-9] will expand
				 * into 36 symbols.)
				 */

/* Tokens are used to hold pattern templates (see makepat () in tools.c) */
typedef struct token {
		       char   tok;
		       char   lchar;
		       char   *bitmap;
		       struct token *next;
		     } TOKEN;

#define TOKSIZE sizeof(TOKEN)

/* An absolute maximum for strings */
#define MAXSTR 132			/* Maximum number of characters in
					 * a line.
					 */
