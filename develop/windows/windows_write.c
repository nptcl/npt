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

static void windows_write_delete_page_0(void)
{
}

static void windows_write_delete_page_1(void)
{
}

static void windows_write_delete_page_2(void)
{
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

static void windows_write_delete_line_0(void)
{
}

static void windows_write_delete_line_1(void)
{
}

static void windows_write_delete_line_2(void)
{
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

int windows_write_scroll_down_lock(int16_t s)
{
	unsigned u;

	windows_screen_enter();
	windows_draw_cursor_off_nolock(NULL);
	u = (s < 0) ? 0 : (unsigned)s;
	windows_draw_cursor_on_nolock(NULL);
	windows_screen_leave();

	return 0;
}

int windows_write_scroll_up_lock(int16_t s)
{
	unsigned u;

	windows_screen_enter();
	windows_draw_cursor_off_nolock(NULL);
	u = (s < 0) ? 0 : (unsigned)s;
	windows_draw_cursor_on_nolock(NULL);
	windows_screen_leave();

	return 0;
}
