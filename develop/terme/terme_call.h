#ifndef __TERME_CALL_HEADER__
#define __TERME_CALL_HEADER__

#include "execute.h"
#include "typedef.h"

#define terme_init _n(terme_init)
#define terme_begin _n(terme_begin)
#define terme_end _n(terme_end)
#define terme_switch_textmode _n(terme_switch_textmode)
#define terme_switch_rawmode _n(terme_switch_rawmode)
#define terme_screen_x _n(terme_screen_x)

void terme_init(void);
int terme_begin(void);
int terme_end(void);
int terme_switch_textmode(int *ret);
int terme_switch_rawmode(int *ret);
void terme_screen_x(int *ret);

#endif

