#ifndef __WINDOWS_DISPLAY_HEADER__
#define __WINDOWS_DISPLAY_HEADER__

#include "typedef.h"
#include <Windows.h>

#define windows_display_init _n(windows_display_init)
#define windows_display_update _n(windows_display_update)
#define windows_display_character _n(windows_display_character)
#define windows_display_line_feed _n(windows_display_line_feed)
#define windows_display_line_back _n(windows_display_line_back)
#define windows_display_paint_nolock _n(windows_display_paint_nolock)
#define windows_display_delete_x1 _n(windows_display_delete_x1)
#define windows_display_delete_x2 _n(windows_display_delete_x2)
#define windows_display_delete_y1 _n(windows_display_delete_y1)
#define windows_display_delete_y2 _n(windows_display_delete_y2)
#define windows_display_delete_all _n(windows_display_delete_all)

void windows_display_init(void);
int windows_display_update(void);
int windows_display_character(unsigned x, unsigned y, unsigned width, unicode c);
int windows_display_line_feed(void);
int windows_display_line_back(void);
void windows_display_paint_nolock(HDC hDC);
int windows_display_delete_x1(unsigned y, unsigned x);
int windows_display_delete_x2(unsigned y, unsigned x);
int windows_display_delete_y1(unsigned y);
int windows_display_delete_y2(unsigned y);
int windows_display_delete_all(void);

#endif

