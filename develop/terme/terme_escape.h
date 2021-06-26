#ifndef __TERME_ESCAPE_HEADER__
#define __TERME_ESCAPE_HEADER__

#include "print_font.h"
#include "typedef.h"

#define terme_font _n(terme_font)
#define terme_text_color _n(terme_text_color)
#define terme_back_color _n(terme_back_color)
#define terme_cursor_left _n(terme_cursor_left)
#define terme_cursor_right _n(terme_cursor_right)
#define terme_cursor_move _n(terme_cursor_move)
#define terme_cursor_delete_line_left _n(terme_cursor_delete_line_left)
#define terme_cursor_delete_line_right _n(terme_cursor_delete_line_right)
#define terme_cursor_delete_line _n(terme_cursor_delete_line)
#define terme_cursor_delete_page _n(terme_cursor_delete_page)

int terme_font(PrintFont value);
int terme_text_color(PrintColor value);
int terme_back_color(PrintColor value);
int terme_cursor_left(int n);
int terme_cursor_right(int n);
int terme_cursor_move(int n);
int terme_cursor_delete_line_left(void);
int terme_cursor_delete_line_right(void);
int terme_cursor_delete_line(void);
int terme_cursor_delete_page(void);

#endif

