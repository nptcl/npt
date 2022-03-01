#include "eastasian_unicode.h"
#include "windows_display.h"
#include "windows_screen.h"
#include "windows_window.h"
#include "windows_write.h"
#include "terme_arch.h"
#include "typedef.h"

int windows_write_setleft_lock(unsigned x)
{
	windows_screen_enter();
	windows_draw_cursor_off_nolock(NULL);
	Window_CursorX = x;
	windows_draw_cursor_on_nolock(NULL);
	windows_screen_leave();

	return 0;
}

static int windows_write_carriage_return_nolock(void)
{
	/* 0x0D */
	Window_CursorX = 0;
	return 0;
}

static int windows_write_line_feed_nolock(void)
{
	/* 0x0A */
	Window_CursorY++;
	if (Window_CursorY < Window_SizeY)
		return 0;
	if (windows_draw_line_feed_nolock())
		return 1;
	if (windows_display_line_feed())
		return 1;
	Window_CursorY = Window_SizeY - 1;

	return 0;
}

static int windows_write_return_call_nolock(void)
{
	if (windows_write_carriage_return_nolock())
		return 1;
	if (windows_write_line_feed_nolock())
		return 1;

	return 0;
}

int windows_write_return_lock(void)
{
	int check;

	windows_screen_enter();
	windows_draw_cursor_off_nolock(NULL);
	check = windows_write_return_call_nolock();
	windows_draw_cursor_on_nolock(NULL);
	windows_screen_leave();

	return check;
}

static int windows_write_character_nolock(unicode c)
{
	unsigned width, x, y;

	width = eastasian_width(c);
	if (Window_SizeX < Window_CursorX + width) {
		if (windows_write_return_call_nolock())
			return 1;
	}
	x = Window_CursorX;
	y = Window_CursorY;
	if (windows_display_character(x, y, width, c))
		return 1;
	if (windows_draw_character_nolock(NULL, x, y, c))
		return 1;
	Window_CursorX += width;

	return 0;
}

static int windows_write_control_nolock(unicode c)
{
	switch (c) {
	case 0x0A: /* Line Feed */
		return windows_write_line_feed_nolock();

	case 0x0D: /* Carriage Return */
		return windows_write_carriage_return_nolock();

	case 0x08: /* BackSpace */
	case 0x7F: /* Delete */
	default:
		return 0;
	}
}

static int windows_write_char_lock_call(unicode c)
{
	if (c < 0x20 || c == 0x7F)
		return windows_write_control_nolock(c);
	else
		return windows_write_character_nolock(c);
}

int windows_write_char_lock(unicode c)
{
	int check;

	windows_screen_enter();
	windows_draw_cursor_off_nolock(NULL);
	check = windows_write_char_lock_call(c);
	windows_draw_cursor_on_nolock(NULL);
	windows_screen_leave();

	return check;
}

int windows_write_clear_nolock(void)
{
	Window_CursorX = 0;
	Window_CursorY = 0;
	if (terme_arch_size_update())
		return 1;
	if (windows_display_update())
		return 1;

	return 0;
}

static void windows_write_cursor_up_nolock(int16_t s)
{
	unsigned u;

	u = (s < 0) ? 1 : (unsigned)s;
	if (Window_CursorY < u)
		Window_CursorY = 0;
	else
		Window_CursorY -= u;
}

int windows_write_cursor_up_lock(int16_t s)
{
	windows_screen_enter();
	windows_draw_cursor_off_nolock(NULL);
	windows_write_cursor_up_nolock(s);
	windows_draw_cursor_on_nolock(NULL);
	windows_screen_leave();

	return 0;
}

static void windows_write_cursor_down_nolock(int16_t s)
{
	unsigned u;

	u = (s < 0) ? 1 : (unsigned)s;
	u = Window_CursorY + u;
	if (u < Window_SizeY)
		Window_CursorY = u;
	else
		Window_CursorY = Window_SizeY - 1;
}

int windows_write_cursor_down_lock(int16_t s)
{
	windows_screen_enter();
	windows_draw_cursor_off_nolock(NULL);
	windows_write_cursor_down_nolock(s);
	windows_draw_cursor_on_nolock(NULL);
	windows_screen_leave();

	return 0;
}

int windows_write_cursor_left_lock(int16_t s)
{
	unsigned u;

	windows_screen_enter();
	windows_draw_cursor_off_nolock(NULL);
	u = (s < 0) ? 1 : (unsigned)s;
	if (Window_CursorX < u)
		Window_CursorX = 0;
	else
		Window_CursorX -= u;
	windows_draw_cursor_on_nolock(NULL);
	windows_screen_leave();

	return 0;
}

int windows_write_cursor_right_lock(int16_t s)
{
	unsigned u;

	windows_screen_enter();
	windows_draw_cursor_off_nolock(NULL);
	u = (s < 0) ? 1 : (unsigned)s;
	u = Window_CursorX + u;
	if (u <= Window_SizeX)
		Window_CursorX = u;
	else
		Window_CursorX = Window_SizeX;
	windows_draw_cursor_on_nolock(NULL);
	windows_screen_leave();

	return 0;
}

int windows_write_first_down_lock(int16_t s)
{
	windows_screen_enter();
	windows_draw_cursor_off_nolock(NULL);
	windows_write_cursor_down_nolock(s);
	Window_CursorX = 0;
	windows_draw_cursor_on_nolock(NULL);
	windows_screen_leave();

	return 0;
}

int windows_write_first_up_lock(int16_t s)
{
	windows_screen_enter();
	windows_draw_cursor_off_nolock(NULL);
	windows_write_cursor_up_nolock(s);
	Window_CursorX = 0;
	windows_draw_cursor_on_nolock(NULL);
	windows_screen_leave();

	return 0;
}

int windows_write_move_x_lock(int16_t s)
{
	unsigned u;

	windows_screen_enter();
	windows_draw_cursor_off_nolock(NULL);

	/* x: 1 - (size+1) */
	u = (s <= 1) ? 0 : (unsigned)(s - 1);
	if (u <= Window_SizeX)
		Window_CursorX = u;
	else
		Window_CursorX = Window_SizeX;
	windows_draw_cursor_on_nolock(NULL);
	windows_screen_leave();

	return 0;
}

int windows_write_move_xy_lock(int16_t x, int16_t y)
{
	unsigned u;

	windows_screen_enter();
	windows_draw_cursor_off_nolock(NULL);

	/* x: 1 - (size+1) */
	u = (x <= 1) ? 0 : (unsigned)(x - 1);
	if (u <= Window_SizeX)
		Window_CursorX = u;
	else
		Window_CursorX = Window_SizeX;

	/* y: 1 - size */
	u = (y <= 1) ? 0 : (unsigned)(y - 1);
	if (u < Window_SizeY)
		Window_CursorY = u;
	else
		Window_CursorY = Window_SizeY - 1;

	windows_draw_cursor_on_nolock(NULL);
	windows_screen_leave();

	return 0;
}

static void windows_write_delete_line_0(void)
{
	unsigned x, y, x1, y1, x2, y2;

	x = Window_CursorX;
	y = Window_CursorY;
	if (x < Window_SizeX) {
		x1 = x;
		y1 = y;
		x2 = Window_SizeX;
		y2 = y1 + 1;
		windows_draw_delete_nolock(NULL, x1, y1, x2, y2);
		windows_display_delete_x2(y, x1);
	}
}

static void windows_write_delete_line_1(void)
{
	unsigned y1, x2, y2;

	y1 = Window_CursorY;
	x2 = Window_CursorX;
	y2 = Window_CursorY + 1;
	windows_draw_delete_nolock(NULL, 0, y1, x2, y2);
	windows_display_delete_x1(y1, x2);
}

static void windows_write_delete_line_2(void)
{
	unsigned y, y1, x2, y2;

	y = Window_CursorY;
	y1 = y;
	x2 = Window_SizeX;
	y2 = y1 + 1;
	windows_draw_delete_nolock(NULL, 0, y1, x2, y2);
	windows_display_delete_x2(y, 0);
}

int windows_write_delete_line_lock(int16_t s)
{
	windows_screen_enter();
	windows_draw_cursor_off_nolock(NULL);
	if (s <= 0)
		windows_write_delete_line_0();
	else if (s == 1)
		windows_write_delete_line_1();
	else if (s == 2)
		windows_write_delete_line_2();
	windows_draw_cursor_on_nolock(NULL);
	windows_screen_leave();

	return 0;
}

static void windows_write_delete_page_0(void)
{
	unsigned y, x2, y1, y2;

	/* delete y */
	y = Window_CursorY;
	if (y < Window_SizeY) {
		y1 = y + 1;
		x2 = Window_SizeX;
		y2 = Window_SizeY;
		windows_draw_delete_nolock(NULL, 0, y1, x2, y2);
		windows_display_delete_y2(y1);
	}

	/* delete x */
	windows_write_delete_line_0();
}

static void windows_write_delete_page_1(void)
{
	unsigned x2, y2;

	/* delete y */
	x2 = Window_SizeX;
	y2 = Window_CursorY;
	windows_draw_delete_nolock(NULL, 0, 0, x2, y2);
	windows_display_delete_y1(y2);

	/* delete x */
	windows_write_delete_line_1();
}

static void windows_write_delete_page_2(void)
{
	unsigned x2, y2;

	x2 = Window_SizeX;
	y2 = Window_SizeY;
	windows_draw_delete_nolock(NULL, 0, 0, x2, y2);
	windows_display_delete_all();
}

int windows_write_delete_page_lock(int16_t s)
{
	windows_screen_enter();
	windows_draw_cursor_off_nolock(NULL);
	if (s <= 0)
		windows_write_delete_page_0();
	else if (s == 1)
		windows_write_delete_page_1();
	else if (s == 2)
		windows_write_delete_page_2();
	windows_draw_cursor_on_nolock(NULL);
	windows_screen_leave();

	return 0;
}

static int windows_write_scroll_next_nolock(void)
{
	if (windows_draw_line_feed_nolock())
		return 1;
	if (windows_display_line_feed())
		return 1;

	return 0;
}

static int windows_write_scroll_prev_nolock(void)
{
	if (windows_draw_line_back_nolock())
		return 1;
	if (windows_display_line_back())
		return 1;

	return 0;
}

int windows_write_scroll_next_lock(int16_t s)
{
	unsigned u, i;

	windows_screen_enter();
	windows_draw_cursor_off_nolock(NULL);
	u = (s < 0) ? 1 : (unsigned)s;
	for (i = 0; i < u; i++) {
		if (windows_write_scroll_next_nolock())
			return 1;
	}
	windows_draw_cursor_on_nolock(NULL);
	windows_screen_leave();

	return 0;
}

int windows_write_scroll_prev_lock(int16_t s)
{
	unsigned u, i;

	windows_screen_enter();
	windows_draw_cursor_off_nolock(NULL);
	u = (s < 0) ? 1 : (unsigned)s;
	for (i = 0; i < u; i++) {
		if (windows_write_scroll_prev_nolock())
			return 1;
	}
	windows_draw_cursor_on_nolock(NULL);
	windows_screen_leave();

	return 0;
}


/*
 *  font
 */
static COLORREF Window_RGB[256] = {
	RGB(0x00,0x00,0x00), RGB(0xCD,0x00,0x00), RGB(0x00,0xCD,0x00), RGB(0xCD,0xCD,0x00),
	RGB(0x00,0x00,0xEE), RGB(0xCD,0x00,0xCD), RGB(0x00,0xCD,0xCD), RGB(0xE5,0xE5,0xE5),
	RGB(0x7F,0x7F,0x7F), RGB(0xFF,0x00,0x00), RGB(0x00,0xFF,0x00), RGB(0xFF,0xFF,0x00),
	RGB(0x5C,0x5C,0xFF), RGB(0xFF,0x00,0xFF), RGB(0x00,0xFF,0xFF), RGB(0xFF,0xFF,0xFF),
	RGB(0x00,0x00,0x00), RGB(0x00,0x00,0x33), RGB(0x00,0x00,0x66), RGB(0x00,0x00,0x99),
	RGB(0x00,0x00,0xCC), RGB(0x00,0x00,0xFF), RGB(0x00,0x33,0x00), RGB(0x00,0x33,0x33),
	RGB(0x00,0x33,0x66), RGB(0x00,0x33,0x99), RGB(0x00,0x33,0xCC), RGB(0x00,0x33,0xFF),
	RGB(0x00,0x66,0x00), RGB(0x00,0x66,0x33), RGB(0x00,0x66,0x66), RGB(0x00,0x66,0x99),
	RGB(0x00,0x66,0xCC), RGB(0x00,0x66,0xFF), RGB(0x00,0x99,0x00), RGB(0x00,0x99,0x33),
	RGB(0x00,0x99,0x66), RGB(0x00,0x99,0x99), RGB(0x00,0x99,0xCC), RGB(0x00,0x99,0xFF),
	RGB(0x00,0xCC,0x00), RGB(0x00,0xCC,0x33), RGB(0x00,0xCC,0x66), RGB(0x00,0xCC,0x99),
	RGB(0x00,0xCC,0xCC), RGB(0x00,0xCC,0xFF), RGB(0x00,0xFF,0x00), RGB(0x00,0xFF,0x33),
	RGB(0x00,0xFF,0x66), RGB(0x00,0xFF,0x99), RGB(0x00,0xFF,0xCC), RGB(0x00,0xFF,0xFF),
	RGB(0x33,0x00,0x00), RGB(0x33,0x00,0x33), RGB(0x33,0x00,0x66), RGB(0x33,0x00,0x99),
	RGB(0x33,0x00,0xCC), RGB(0x33,0x00,0xFF), RGB(0x33,0x33,0x00), RGB(0x33,0x33,0x33),
	RGB(0x33,0x33,0x66), RGB(0x33,0x33,0x99), RGB(0x33,0x33,0xCC), RGB(0x33,0x33,0xFF),
	RGB(0x33,0x66,0x00), RGB(0x33,0x66,0x33), RGB(0x33,0x66,0x66), RGB(0x33,0x66,0x99),
	RGB(0x33,0x66,0xCC), RGB(0x33,0x66,0xFF), RGB(0x33,0x99,0x00), RGB(0x33,0x99,0x33),
	RGB(0x33,0x99,0x66), RGB(0x33,0x99,0x99), RGB(0x33,0x99,0xCC), RGB(0x33,0x99,0xFF),
	RGB(0x33,0xCC,0x00), RGB(0x33,0xCC,0x33), RGB(0x33,0xCC,0x66), RGB(0x33,0xCC,0x99),
	RGB(0x33,0xCC,0xCC), RGB(0x33,0xCC,0xFF), RGB(0x33,0xFF,0x00), RGB(0x33,0xFF,0x33),
	RGB(0x33,0xFF,0x66), RGB(0x33,0xFF,0x99), RGB(0x33,0xFF,0xCC), RGB(0x33,0xFF,0xFF),
	RGB(0x66,0x00,0x00), RGB(0x66,0x00,0x33), RGB(0x66,0x00,0x66), RGB(0x66,0x00,0x99),
	RGB(0x66,0x00,0xCC), RGB(0x66,0x00,0xFF), RGB(0x66,0x33,0x00), RGB(0x66,0x33,0x33),
	RGB(0x66,0x33,0x66), RGB(0x66,0x33,0x99), RGB(0x66,0x33,0xCC), RGB(0x66,0x33,0xFF),
	RGB(0x66,0x66,0x00), RGB(0x66,0x66,0x33), RGB(0x66,0x66,0x66), RGB(0x66,0x66,0x99),
	RGB(0x66,0x66,0xCC), RGB(0x66,0x66,0xFF), RGB(0x66,0x99,0x00), RGB(0x66,0x99,0x33),
	RGB(0x66,0x99,0x66), RGB(0x66,0x99,0x99), RGB(0x66,0x99,0xCC), RGB(0x66,0x99,0xFF),
	RGB(0x66,0xCC,0x00), RGB(0x66,0xCC,0x33), RGB(0x66,0xCC,0x66), RGB(0x66,0xCC,0x99),
	RGB(0x66,0xCC,0xCC), RGB(0x66,0xCC,0xFF), RGB(0x66,0xFF,0x00), RGB(0x66,0xFF,0x33),
	RGB(0x66,0xFF,0x66), RGB(0x66,0xFF,0x99), RGB(0x66,0xFF,0xCC), RGB(0x66,0xFF,0xFF),
	RGB(0x99,0x00,0x00), RGB(0x99,0x00,0x33), RGB(0x99,0x00,0x66), RGB(0x99,0x00,0x99),
	RGB(0x99,0x00,0xCC), RGB(0x99,0x00,0xFF), RGB(0x99,0x33,0x00), RGB(0x99,0x33,0x33),
	RGB(0x99,0x33,0x66), RGB(0x99,0x33,0x99), RGB(0x99,0x33,0xCC), RGB(0x99,0x33,0xFF),
	RGB(0x99,0x66,0x00), RGB(0x99,0x66,0x33), RGB(0x99,0x66,0x66), RGB(0x99,0x66,0x99),
	RGB(0x99,0x66,0xCC), RGB(0x99,0x66,0xFF), RGB(0x99,0x99,0x00), RGB(0x99,0x99,0x33),
	RGB(0x99,0x99,0x66), RGB(0x99,0x99,0x99), RGB(0x99,0x99,0xCC), RGB(0x99,0x99,0xFF),
	RGB(0x99,0xCC,0x00), RGB(0x99,0xCC,0x33), RGB(0x99,0xCC,0x66), RGB(0x99,0xCC,0x99),
	RGB(0x99,0xCC,0xCC), RGB(0x99,0xCC,0xFF), RGB(0x99,0xFF,0x00), RGB(0x99,0xFF,0x33),
	RGB(0x99,0xFF,0x66), RGB(0x99,0xFF,0x99), RGB(0x99,0xFF,0xCC), RGB(0x99,0xFF,0xFF),
	RGB(0xCC,0x00,0x00), RGB(0xCC,0x00,0x33), RGB(0xCC,0x00,0x66), RGB(0xCC,0x00,0x99),
	RGB(0xCC,0x00,0xCC), RGB(0xCC,0x00,0xFF), RGB(0xCC,0x33,0x00), RGB(0xCC,0x33,0x33),
	RGB(0xCC,0x33,0x66), RGB(0xCC,0x33,0x99), RGB(0xCC,0x33,0xCC), RGB(0xCC,0x33,0xFF),
	RGB(0xCC,0x66,0x00), RGB(0xCC,0x66,0x33), RGB(0xCC,0x66,0x66), RGB(0xCC,0x66,0x99),
	RGB(0xCC,0x66,0xCC), RGB(0xCC,0x66,0xFF), RGB(0xCC,0x99,0x00), RGB(0xCC,0x99,0x33),
	RGB(0xCC,0x99,0x66), RGB(0xCC,0x99,0x99), RGB(0xCC,0x99,0xCC), RGB(0xCC,0x99,0xFF),
	RGB(0xCC,0xCC,0x00), RGB(0xCC,0xCC,0x33), RGB(0xCC,0xCC,0x66), RGB(0xCC,0xCC,0x99),
	RGB(0xCC,0xCC,0xCC), RGB(0xCC,0xCC,0xFF), RGB(0xCC,0xFF,0x00), RGB(0xCC,0xFF,0x33),
	RGB(0xCC,0xFF,0x66), RGB(0xCC,0xFF,0x99), RGB(0xCC,0xFF,0xCC), RGB(0xCC,0xFF,0xFF),
	RGB(0xFF,0x00,0x00), RGB(0xFF,0x00,0x33), RGB(0xFF,0x00,0x66), RGB(0xFF,0x00,0x99),
	RGB(0xFF,0x00,0xCC), RGB(0xFF,0x00,0xFF), RGB(0xFF,0x33,0x00), RGB(0xFF,0x33,0x33),
	RGB(0xFF,0x33,0x66), RGB(0xFF,0x33,0x99), RGB(0xFF,0x33,0xCC), RGB(0xFF,0x33,0xFF),
	RGB(0xFF,0x66,0x00), RGB(0xFF,0x66,0x33), RGB(0xFF,0x66,0x66), RGB(0xFF,0x66,0x99),
	RGB(0xFF,0x66,0xCC), RGB(0xFF,0x66,0xFF), RGB(0xFF,0x99,0x00), RGB(0xFF,0x99,0x33),
	RGB(0xFF,0x99,0x66), RGB(0xFF,0x99,0x99), RGB(0xFF,0x99,0xCC), RGB(0xFF,0x99,0xFF),
	RGB(0xFF,0xCC,0x00), RGB(0xFF,0xCC,0x33), RGB(0xFF,0xCC,0x66), RGB(0xFF,0xCC,0x99),
	RGB(0xFF,0xCC,0xCC), RGB(0xFF,0xCC,0xFF), RGB(0xFF,0xFF,0x00), RGB(0xFF,0xFF,0x33),
	RGB(0xFF,0xFF,0x66), RGB(0xFF,0xFF,0x99), RGB(0xFF,0xFF,0xCC), RGB(0xFF,0xFF,0xFF),
	RGB(0x00,0x00,0x00), RGB(0x0B,0x0B,0x0B), RGB(0x16,0x16,0x16), RGB(0x21,0x21,0x21),
	RGB(0x2C,0x2C,0x2C), RGB(0x37,0x37,0x37), RGB(0x42,0x42,0x42), RGB(0x4D,0x4D,0x4D),
	RGB(0x58,0x58,0x58), RGB(0x63,0x63,0x63), RGB(0x6F,0x6F,0x6F), RGB(0x7A,0x7A,0x7A),
	RGB(0x85,0x85,0x85), RGB(0x90,0x90,0x90), RGB(0x9B,0x9B,0x9B), RGB(0xA6,0xA6,0xA6),
	RGB(0xB1,0xB1,0xB1), RGB(0xBC,0xBC,0xBC), RGB(0xC7,0xC7,0xC7), RGB(0xD3,0xD3,0xD3),
	RGB(0xDE,0xDE,0xDE), RGB(0xE9,0xE9,0xE9), RGB(0xF4,0xF4,0xF4), RGB(0xFF,0xFF,0xFF)
};

struct windows_write_font {
	int16_t *escape;
	size_t size, index;
};

static int window_write_getfont(struct windows_write_font *str, int16_t *ret)
{
	if (str->index < str->size) {
		*ret = str->escape[str->index];
		str->index++;
		return 0;
	}
	else {
		*ret = -1;
		return 1;
	}
}

static void windows_write_font_index(int16_t m, int a, int b, COLORREF *ret)
{
	m = m - a;
	*ret = Window_RGB[m + b];
}

static void windows_write_font_mode(struct windows_write_font *str, COLORREF *ret)
{
	int16_t m, r, g, b;

	if (window_write_getfont(str, &m))
		return;

	/* Color 256 */
	if (m == 2) {
		if (window_write_getfont(str, &m))
			m = 0;
		if (m < 0 || 0xFF < m)
			m = 0;
		*ret = Window_RGB[m];
		return;
	}

	/* Color RGB */
	if (m == 5) {
		if (window_write_getfont(str, &r))
			r = 0;
		if (r < 0 || 0xFF < r)
			r = 0;
		if (window_write_getfont(str, &g))
			g = 0;
		if (g < 0 || 0xFF < g)
			g = 0;
		if (window_write_getfont(str, &b))
			b = 0;
		if (b < 0 || 0xFF < b)
			b = 0;
		*ret = RGB(r, g, b);
		return;
	}
}

static void windows_write_font_color(int16_t *escape, unsigned size,
	COLORREF *c1, COLORREF *c2)
{
	struct windows_write_font str;
	int16_t m;

	str.escape = escape;
	str.size = size;
	str.index = 0;

	while (! window_write_getfont(&str, &m)) {
		/* Color1 */
		if (30 <= m && m <= 37) {
			windows_write_font_index(m, 30, 0, c1);
			continue;
		}
		if (m == 38) {
			windows_write_font_mode(&str, c1);
			continue;
		}
		if (m == 39) {
			*c1 = Window_Color1_Default;
			continue;
		}
		if (90 <= m && m <= 97) {
			windows_write_font_index(m, 90, 8, c1);
			continue;
		}

		/* Color2 */
		if (40 <= m && m <= 47) {
			windows_write_font_index(m, 40, 0, c2);
			continue;
		}
		if (m == 48) {
			windows_write_font_mode(&str, c2);
			continue;
		}
		if (m == 49) {
			*c2 = Window_Color2_Default;
			continue;
		}
		if (100 <= m && m <= 107) {
			windows_write_font_index(m, 40, 8, c2);
			continue;
		}
	}
}

int windows_write_font_lock(int16_t *escape, unsigned size)
{
	COLORREF color1, color2;

	windows_screen_enter();
	color1 = Window_Color1_Default;
	color2 = Window_Color2_Default;
	windows_write_font_color(escape, size, &color1, &color2);
	Window_Color1 = color1;
	Window_Color2 = color2;
	windows_screen_leave();

	return 0;
}
