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

_g void info(const char *, ...);
_g void info_noeol(const char *, ...);
_g void infobit(addr pos);
_g void infoprint(addr pos);
_g void infoprint_depth(addr pos, int depth);
_g void infoprint_noeol(addr pos);
_g void infoprint_once(addr pos, const char *name);
_g void infoerror(const char *, int, const char *, const char *, ...);
_g void infosystem(void);

#endif

