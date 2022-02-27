#ifndef __WINDOWS_DISPLAY_HEADER__
#define __WINDOWS_DISPLAY_HEADER__

#include "typedef.h"
#include <Windows.h>

#define windows_display_init _n(windows_display_init)
#define windows_display_update _n(windows_display_update)
#define windows_display_character _n(windows_display_character)
#define windows_display_line_feed _n(windows_display_line_feed)
#define windows_display_paint_nolock _n(windows_display_paint_nolock)

void windows_display_init(void);
int windows_display_update(void);
int windows_display_character(unsigned x, unsigned y, unsigned width, unicode c);
int windows_display_line_feed(void);
void windows_display_paint_nolock(HDC hDC);

#endif

