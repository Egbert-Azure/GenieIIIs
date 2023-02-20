
/* BIOS-Aufruf fuer CP/M Plus ueber die BDOS-Funktion 50 */

/* CP/M-80 BIOS entry points */
#define BOOT    0		/* perform cold start initialization    */
#define WBOOT   1		/* perform warm start initialization    */
#define CONST   2		/* check for console input char ready   */
#define CONIN   3		/* read  console character in	        */
#define CONOUT  4		/* write console character out	        */
#define LIST    5		/* write list    character out	        */
#define AUXOUT  6		/* write auxiliary output character     */
#define AUXIN   7		/* read  auxiliary input  character     */
#define HOME    8		/* move to track 00 on selected disk    */ 
#define SELDSK  9		/* select disk drive		        */
#define SETTRK 10		/* set track  number		        */
#define SETSEC 11		/* set sector number		        */
#define SETDMA 12		/* set DMA address		        */
#define READ   13		/* read  specifidd sector	        */
#define WRITE  14		/* write specified sector	        */
#define LISTST 15		/* return list status		        */
#define SECTRN 16		/* translate logical to physical sector */
#define CONOST 17		/* return output status of console	*/
#define AUXIST 18		/* return input  status of aux. port    */
#define AUXOST 19		/* return output status of aux. port	*/
#define DEVTBL 20		/* return address of char. I/O table	*/
#define DEVINI 21		/* initialize char. I/O devices		*/
#define DRVTBL 22		/* return address of disk drive table	*/
#define MULTIO 23		/* set number of logically consecutive  */
				/* sectors to be read or written	*/
#define FLUSH  24		/* force physical buffer flushing for   */
				/* user-supported deblocking		*/
#define MOVE   25		/* memory to memory move		*/
#define TIME   26		/* time set/get signal			*/
#define SELMEM 27		/* select bank of memory		*/
#define SETBNK 28		/* specify bank for DMA operation	*/
#define XMOVE  29		/* set bank when a buffer is in a bank  */
				/* other than 0 or 1			*/
#define USERF  30		/* reserved for system implementor	*/ 
#define FALSE  0

#ifndef uchar
#define uchar unsigned char
#endif

/* function prototypes */
/*unsigned bios(uchar nummer, ...);*/
unsigned bioshl(uchar nummer, ...);

