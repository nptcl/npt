#ifndef __TERME_SCREEN_HEADER__
#define __TERME_SCREEN_HEADER__

#include "execute.h"
#include "typedef.h"

#define terme_screen_build _n(terme_screen_build)
#define terme_screen_clear_ _n(terme_screen_clear_)
#define terme_screen_prompt_ _n(terme_screen_prompt_)
#define terme_screen_push_ _n(terme_screen_push_)
#define terme_screen_history_ _n(terme_screen_history_)
#define terme_screen_left_ _n(terme_screen_left_)
#define terme_screen_right_ _n(terme_screen_right_)
#define terme_screen_delete_ _n(terme_screen_delete_)
#define terme_screen_backspace_ _n(terme_screen_backspace_)
#define terme_screen_first_ _n(terme_screen_first_)
#define terme_screen_last_ _n(terme_screen_last_)
#define terme_screen_update_ _n(terme_screen_update_)
#define terme_screen_refresh_ _n(terme_screen_refresh_)
#define terme_screen_rmleft_ _n(terme_screen_rmleft_)
#define terme_screen_rmright_ _n(terme_screen_rmright_)

void terme_screen_build(addr *ret);
int terme_screen_clear_(Execute ptr);
int terme_screen_prompt_(Execute ptr);
int terme_screen_push_(Execute ptr);
int terme_screen_history_(Execute ptr);
int terme_screen_left_(Execute ptr, unsigned width);
int terme_screen_right_(Execute ptr, unsigned width);
int terme_screen_delete_(Execute ptr);
int terme_screen_backspace_(Execute ptr, unsigned width);
int terme_screen_first_(Execute ptr);
int terme_screen_last_(Execute ptr);
int terme_screen_update_(Execute ptr);
int terme_screen_refresh_(Execute ptr);
int terme_screen_rmleft_(Execute ptr);
int terme_screen_rmright_(Execute ptr);

#endif

