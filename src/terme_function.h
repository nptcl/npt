#ifndef __TERME_FUNCTION_HEADER__
#define __TERME_FUNCTION_HEADER__

#include "typedef.h"

#define terme_call_enable_p _n(terme_call_enable_p)
#define terme_call_input_ _n(terme_call_input_)
#define terme_call_move_ _n(terme_call_move_)
#define terme_call_clear_ _n(terme_call_clear_)
#define terme_call_begin_ _n(terme_call_begin_)
#define terme_call_end_ _n(terme_call_end_)

int terme_call_enable_p(void);
int terme_call_input_(addr args, addr *ret1, addr *ret2);
int terme_call_move_(addr args);
int terme_call_clear_(void);
int terme_call_begin_(addr *ret);
int terme_call_end_(addr pos);

#endif

