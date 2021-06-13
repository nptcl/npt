#ifndef __INFO_HEADER__
#define __INFO_HEADER__

/*
 *  Print object for debug.
 *    Similary to print-object function but info don't use stream object.
 */
#include "define.h"
#include "typedef.h"

#define info _n(info)
#define info_noeol _n(info_noeol)
#define infobit _n(infobit)
#define infoprint _n(infoprint)
#define infoprint_depth _n(infoprint_depth)
#define infoprint_noeol _n(infoprint_noeol)
#define infoprint_once _n(infoprint_once)
#define infoerror _n(infoerror)
#define infosystem _n(infosystem)
#define infotype _n(infotype)

void info(const char *, ...);
void info_noeol(const char *, ...);
void infobit(addr pos);
void infoprint(addr pos);
void infoprint_depth(addr pos, int depth);
void infoprint_noeol(addr pos);
void infoprint_once(addr pos, const char *name);
void infoerror(const char *, int, const char *, const char *, ...);
void infosystem(void);
void infotype(addr pos);

#endif

