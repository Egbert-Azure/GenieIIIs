/******************************************************************************
*  E R R N O  *  C L I B Y Y Y  *  T h o m a s   H o l t e  *   8 6 0 2 1 0   *
*******************************************************************************
*
* Version 1.0 by Thomas Holte
*
* Header file with common definitions for Mi-C file system error numbers.
*
* Note: This file generates no code !
*/

/* global error variable */
extern int errno;

/* error numbers */
#define ENOENT   2 		/* no such file              */
#define EIO	 5		/* I/O error		     */
#define ENXIO    6		/* no such device or address */
#define E2BIG	 7		/* arg list too long	     */
#define ENOEXEC  8		/* exec format error	     */
#define EBADF    9		/* bad file number	     */
#define ENOMEM  12		/* not enough core	     */
#define EACCESS 13		/* permission denied	     */
#define ENOTBLK 15		/* blockdevice required      */
#define EEXIST	17		/* file exists		     */
#define EXDEV   18		/* cross-device link	     */
#define ENODEV  19		/* no such device	     */
#define EINVAL  22		/* invalid argument	     */
#define ENFILE  23		/* file table overflow       */
#define EMFILE  24		/* too many open files       */
#define ENOSPC  28		/* no space left on device   */
#define ESPIPE  29		/* illegal seek		     */
#define EROFS   30		/* read-only file-system     */
#define EDOM    33		/* math argument	     */
#define ERANGE  34		/* result too large	     */
