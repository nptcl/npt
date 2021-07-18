#ifndef __TERME_WRITE_HEADER__
#define __TERME_WRITE_HEADER__

#include "execute.h"
#include "prompt.h"
#include "typedef.h"

#define terme_write_flush_ _n(terme_write_flush_)
#define terme_write_char_ _n(terme_write_char_)
#define terme_write_terpri_ _n(terme_write_terpri_)
#define terme_write_delete_line_right_ _n(terme_write_delete_line_right_)
#define terme_write_left_ _n(terme_write_left_)
#define terme_write_right_ _n(terme_write_right_)
#define terme_write_up_ _n(terme_write_up_)
#define terme_write_down_ _n(terme_write_down_)
#define terme_write_first_up_ _n(terme_write_first_up_)
#define terme_write_first_down_ _n(terme_write_first_down_)
#define terme_write_delete_line_ _n(terme_write_delete_line_)
#define terme_write_delete_page_ _n(terme_write_delete_page_)

int terme_write_flush_(void);
int terme_write_char_(Execute ptr, unicode c, unsigned width, PromptMode mode);
int terme_write_terpri_(Execute ptr);
int terme_write_delete_line_right_(Execute ptr);
int terme_write_left_(Execute ptr, int n);
int terme_write_right_(Execute ptr, int n);
int terme_write_up_(Execute ptr, int n);
int terme_write_down_(Execute ptr, int n);
int terme_write_first_up_(Execute ptr, int n);
int terme_write_first_down_(Execute ptr, int n);
int terme_write_delete_line_(Execute ptr);
int terme_write_delete_page_(Execute ptr);

#endif

