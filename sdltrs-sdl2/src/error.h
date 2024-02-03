/*
 * Copyright (C) 1992 Clarendon Hill Software.
 *
 * Permission is granted to any individual or institution to use, copy,
 * or redistribute this software, provided this copyright notice is retained.
 *
 * This software is provided "as is" without any expressed or implied
 * warranty.  If this software brings on any sort of damage -- physical,
 * monetary, emotional, or brain -- too bad.  You've got no one to blame
 * but yourself.
 *
 * The software may be modified for your own purposes, but modified versions
 * must retain this notice.
 */

#ifndef _ERROR_H
#define _ERROR_H

void debug(const char *fmt, ...);
void error(const char *fmt, ...);
void fatal(const char *fmt, ...);

void file_error(const char *fmt, ...);

#endif /* _ERROR_H */
