/*******************************************************************************
*  G E T A R G S  *  T O O L S 0 0 5  *  T h o m a s   H o l t e * 8 6 0 9 2 1 *
********************************************************************************
*
* Version 1.0 by Thomas Holte
*/

/*	GETARGS.H      Typedefs and defines needed for getargs */

#define INTEGER   0
#define BOOLEAN   1
#define CHARACTER 2
#define STRING	  3
#define PROC	  4

typedef struct {
		 char arg;		/* command line switch	    */
		 char type;		/* variable type	    */
		 char *variable;	/* pointer to variable	    */
		 char *errmsg;		/* pointer to error message */
	       } ARG;
