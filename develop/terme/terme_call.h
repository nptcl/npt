#ifndef __TERME_CALL_HEADER__
#define __TERME_CALL_HEADER__

#include "execute.h"
#include "typedef.h"

#define terme_init _n(terme_init)
#define terme_free _n(terme_free)
#define terme_switch_textmode _n(terme_switch_textmode)
#define terme_switch_rawmode _n(terme_switch_rawmode)

int terme_init(void);
int terme_free(void);
int terme_switch_textmode(int *ret);
int terme_switch_rawmode(int *ret);

#endif

