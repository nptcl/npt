#ifndef __INFO_HEADER__
#define __INFO_HEADER__

/*
 *  Print object for debug.
 *    Similary to print-object function but info don't use stream object.
 */
#include "typedef.h"

void info(const char *, ...);
void info_noeol(const char *, ...);
void infobit(addr pos);
void infoprint(addr pos);
void infoprint_noeol(addr pos);
void infoerror(const char *, int, const char *, const char *, ...);
void infosystem(void);

#endif

