#ifndef __TERME_FUNCTION_HEADER__
#define __TERME_FUNCTION_HEADER__

#include "execute.h"
#include "typedef.h"

#define terme_call_enable_p _n(terme_call_enable_p)
#define terme_call_input_ _n(terme_call_input_)
#define terme_call_output_ _n(terme_call_output_)
#define terme_call_move_ _n(terme_call_move_)
#define terme_call_clear_ _n(terme_call_clear_)
#define terme_call_delete_ _n(terme_call_delete_)
#define terme_call_font_ _n(terme_call_font_)
#define terme_call_size_ _n(terme_call_size_)
#define terme_call_scroll_ _n(terme_call_scroll_)
#define terme_call_begin_ _n(terme_call_begin_)
#define terme_call_end_ _n(terme_call_end_)

int terme_call_enable_p(void);
int terme_call_input_(addr args, addr *rtype, addr *rvalue);
int terme_call_output_(addr args);
int terme_call_move_(addr args);
int terme_call_clear_(addr args);
int terme_call_delete_(addr args);
int terme_call_font_(Execute ptr, addr args);
int terme_call_size_(addr *rx, addr *ry);
int terme_call_scroll_(addr args);
int terme_call_begin_(addr *ret);
int terme_call_end_(addr pos);

#endif

