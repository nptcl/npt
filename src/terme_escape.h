#ifndef __TERME_ESCAPE_HEADER__
#define __TERME_ESCAPE_HEADER__

#include "execute.h"
#include "print_font.h"
#include "typedef.h"

#define terme_font _n(terme_font)
#define terme_text_color _n(terme_text_color)
#define terme_back_color _n(terme_back_color)
#define terme_cursor_left _n(terme_cursor_left)
#define terme_cursor_right _n(terme_cursor_right)
#define terme_cursor_up _n(terme_cursor_up)
#define terme_cursor_down _n(terme_cursor_down)
#define terme_cursor_move_x _n(terme_cursor_move_x)
#define terme_cursor_move _n(terme_cursor_move)
#define terme_cursor_first_up _n(terme_cursor_first_up)
#define terme_cursor_first_down _n(terme_cursor_first_down)
#define terme_cursor_delete_line_left _n(terme_cursor_delete_line_left)
#define terme_cursor_delete_line_right _n(terme_cursor_delete_line_right)
#define terme_cursor_delete_line _n(terme_cursor_delete_line)
#define terme_cursor_delete_page_left _n(terme_cursor_delete_page_left)
#define terme_cursor_delete_page_right _n(terme_cursor_delete_page_right)
#define terme_cursor_delete_page _n(terme_cursor_delete_page)
#define terme_cursor_scroll_up _n(terme_cursor_scroll_up)
#define terme_cursor_scroll_down _n(terme_cursor_scroll_down)
#define terme_color_symbol_ _n(terme_color_symbol_)
#define terme_color_not_symbol_ _n(terme_color_not_symbol_)
#define terme_color_dark_ _n(terme_color_dark_)
#define terme_color_bright_ _n(terme_color_bright_)
#define terme_font_parser_ _n(terme_font_parser_)
#define terme_font_update_ _n(terme_font_update_)

int terme_font(Execute ptr, PrintFont value);
int terme_text_color(Execute ptr, PrintColor value);
int terme_back_color(Execute ptr, PrintColor value);
int terme_cursor_left(int n);
int terme_cursor_right(int n);
int terme_cursor_up(int n);
int terme_cursor_down(int n);
int terme_cursor_move_x(int x);
int terme_cursor_move(int x, int y);
int terme_cursor_first_up(int n);
int terme_cursor_first_down(int n);
int terme_cursor_delete_line_left(void);
int terme_cursor_delete_line_right(void);
int terme_cursor_delete_line(void);
int terme_cursor_delete_page_left(void);
int terme_cursor_delete_page_right(void);
int terme_cursor_delete_page(void);
int terme_cursor_scroll_up(int n);
int terme_cursor_scroll_down(int n);
int terme_color_symbol_(Execute ptr, addr pos, int *ret, int *brightp);
int terme_color_not_symbol_(Execute ptr, addr pos, int *ret, int *brightp);
int terme_color_dark_(addr pos, int *ret);
int terme_color_bright_(addr pos, int *ret);
int terme_font_parser_(addr args);
int terme_font_update_(Execute ptr, addr args);

#endif

