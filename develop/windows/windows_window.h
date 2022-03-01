#ifndef __WINDOWS_WINDOW_HEADER__
#define __WINDOWS_WINDOW_HEADER__

#include <Windows.h>
#include "typedef.h"

#define windows_window_init _n(windows_window_init)
#define windows_window _n(windows_window)
#define windows_window_show_default _n(windows_window_show_default)
#define windows_window_show_show _n(windows_window_show_show)
#define windows_window_show_hide _n(windows_window_show_hide)
#define windows_window_size_update _n(windows_window_size_update)
#define windows_window_error _n(windows_window_error)
#define windows_draw_cursor_on_nolock _n(windows_draw_cursor_on_nolock)
#define windows_draw_cursor_off_nolock _n(windows_draw_cursor_off_nolock)
#define windows_draw_cursor_on_lock _n(windows_draw_cursor_on_lock)
#define windows_draw_cursor_off_lock _n(windows_draw_cursor_off_lock)
#define windows_draw_character_nolock _n(windows_draw_character_nolock)
#define windows_draw_character_lock _n(windows_draw_character_lock)
#define windows_draw_line_feed_nolock _n(windows_draw_line_feed_nolock)
#define windows_draw_line_back_nolock _n(windows_draw_line_back_nolock)
#define windows_draw_delete_nolock _n(windows_draw_delete_nolock)

int windows_window_init(void);
int windows_window(void);
int windows_window_show_default(void);
int windows_window_show_show(void);
int windows_window_show_hide(void);
int windows_window_size_update(unsigned x, unsigned y);
void windows_window_error(const char *str);

void windows_draw_cursor_on_nolock(HDC hDC);
void windows_draw_cursor_off_nolock(HDC hDC);
void windows_draw_cursor_on_lock(HDC hDC);
void windows_draw_cursor_off_lock(HDC hDC);

int windows_draw_character_nolock(HDC hDC, unsigned x, unsigned y, unicode c);
int windows_draw_character_lock(HWND hWnd, unsigned x, unsigned y, unicode c);
int windows_draw_line_feed_nolock(void);
int windows_draw_line_back_nolock(void);
int windows_draw_delete_nolock(HDC hDC, unsigned x1, unsigned y1, unsigned x2, unsigned y2);

#endif
