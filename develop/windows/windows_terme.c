#include "object.h"
#include "typedef.h"
#include "windows_display.h"
#include "windows_input.h"
#include "windows_output.h"
#include "windows_screen.h"
#include "windows_stream.h"
#include "windows_terme.h"
#include "windows_window.h"

int terme_windows_init(void)
{
	windows_display_init();
	windows_input_init();
	windows_output_init();
	windows_screen_init();
	windows_stream_init();
	if (windows_window_init())
		return 1;

	return 0;
}

int terme_windows_begin(void)
{
#ifndef LISP_TERME_HIDE
	if (windows_window_show_default())
		return 1;
#endif

	return 0;
}

int terme_windows_end(void)
{
	return 0;
}

int terme_windows_textmode(void)
{
	return 0;
}

int terme_windows_rawmode(void)
{
	return 0;
}

void terme_windows_build(void)
{
	windows_stream_build();
}

int terme_windows_select(int *ret)
{
	return windows_input_select(ret);
}

int terme_windows_wait_integer(int *ret, int value)
{
	return windows_input_wait_integer(ret, value);
}

int terme_windows_wait_float(int *ret, double value)
{
	return windows_input_wait_float(ret, value);
}

int terme_windows_read(void *data, size_t size, size_t *ret)
{
	return windows_input_read(data, size, ret);
}

int terme_windows_write(const void *data, size_t size, size_t *ret)
{
	return windows_output_write(data, size, ret);
}

int terme_windows_escape_begin(void)
{
	return 0;
}

int terme_windows_escape_end(int *ret)
{
	*ret = 0;
	return 0;
}

int terme_windows_begin_default_(addr *ret)
{
	*ret = Nil;
	return 0;
}

int terme_windows_begin_rawmode_(addr *ret)
{
	*ret = Nil;
	return 0;
}

int terme_windows_restore_(addr pos)
{
	return 0;
}
