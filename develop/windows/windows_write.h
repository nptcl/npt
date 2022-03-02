#ifndef __WINDOWS_WRITE_HEADER__
#define __WINDOWS_WRITE_HEADER__

#include "typedef.h"

#define windows_write_setleft_lock _n(windows_write_setleft_lock)
#define windows_write_char_lock _n(windows_write_char_lock)
#define windows_write_clear_nolock _n(windows_write_clear_nolock)
#define windows_write_cursor_up_lock _n(windows_write_cursor_up_lock)
#define windows_write_cursor_down_lock _n(windows_write_cursor_down_lock)
#define windows_write_cursor_left_lock _n(windows_write_cursor_left_lock)
#define windows_write_cursor_right_lock _n(windows_write_cursor_right_lock)
#define windows_write_first_down_lock _n(windows_write_first_down_lock)
#define windows_write_first_up_lock _n(windows_write_first_up_lock)
#define windows_write_move_x_lock _n(windows_write_move_x_lock)
#define windows_write_move_xy_lock _n(windows_write_move_xy_lock)
#define windows_write_delete_line_lock _n(windows_write_delete_line_lock)
#define windows_write_delete_page_lock _n(windows_write_delete_page_lock)
#define windows_write_scroll_next_lock _n(windows_write_scroll_next_lock)
#define windows_write_scroll_prev_lock _n(windows_write_scroll_prev_lock)
#define windows_write_font_lock _n(windows_write_font_lock)

int windows_write_setleft_lock(unsigned x);
int windows_write_char_lock(unicode c);
int windows_write_clear_nolock(void);
int windows_write_cursor_up_lock(int16_t s);
int windows_write_cursor_down_lock(int16_t s);
int windows_write_cursor_left_lock(int16_t s);
int windows_write_cursor_right_lock(int16_t s);
int windows_write_first_down_lock(int16_t s);
int windows_write_first_up_lock(int16_t s);
int windows_write_move_x_lock(int16_t s);
int windows_write_move_xy_lock(int16_t y, int16_t x);
int windows_write_delete_line_lock(int16_t s);
int windows_write_delete_page_lock(int16_t s);
int windows_write_scroll_next_lock(int16_t s);
int windows_write_scroll_prev_lock(int16_t s);
int windows_write_font_lock(int16_t *escape, unsigned size);

#endif
