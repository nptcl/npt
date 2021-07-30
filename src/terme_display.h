#ifndef __TERME_DISPLAY_HEADER__
#define __TERME_DISPLAY_HEADER__

#include "execute.h"
#include "prompt.h"
#include "typedef.h"

#define terme_display_build _n(terme_display_build)
#define terme_display_clear_ _n(terme_display_clear_)

#define terme_display_write_char_ _n(terme_display_write_char_)
#define terme_display_terpri_ _n(terme_display_terpri_)
#define terme_display_delete_line_right_ _n(terme_display_delete_line_right_)
#define terme_display_left_ _n(terme_display_left_)
#define terme_display_right_ _n(terme_display_right_)
#define terme_display_up_ _n(terme_display_up_)
#define terme_display_down_ _n(terme_display_down_)
#define terme_display_first_up_ _n(terme_display_first_up_)
#define terme_display_first_down_ _n(terme_display_first_down_)
#define terme_display_delete_line_ _n(terme_display_delete_line_)
#define terme_display_getwidth_ _n(terme_display_getwidth_)
#define terme_display_previous_ _n(terme_display_previous_)
#define terme_display_getlast_ _n(terme_display_getlast_)
#define terme_display_delete_page_ _n(terme_display_delete_page_)

void terme_display_build(addr *ret);
int terme_display_clear_(Execute ptr);

int terme_display_write_char_(Execute ptr, unicode c, unsigned width, PromptMode mode);
int terme_display_terpri_(Execute ptr);
int terme_display_delete_line_right_(Execute ptr);
int terme_display_left_(Execute ptr, int n);
int terme_display_right_(Execute ptr, int n);
int terme_display_up_(Execute ptr, int n);
int terme_display_down_(Execute ptr, int n);
int terme_display_first_up_(Execute ptr, int n);
int terme_display_first_down_(Execute ptr, int n);
int terme_display_delete_line_(Execute ptr);
int terme_display_getwidth_(Execute ptr, unsigned *ret);
int terme_display_previous_(Execute ptr, int *ret);
int terme_display_getlast_(Execute ptr, unsigned *ret);
int terme_display_delete_page_(Execute ptr);

#endif

