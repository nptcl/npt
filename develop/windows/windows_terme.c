#include "condition.h"
#include "constant.h"
#include "object.h"
#include "package_import.h"
#include "paper.h"
#include "symbol.h"
#include "terme_output.h"
#include "typedef.h"
#include "windows_display.h"
#include "windows_input.h"
#include "windows_output.h"
#include "windows_screen.h"
#include "windows_stream.h"
#include "windows_terme.h"
#include "windows_window.h"
#include <Windows.h>

#define TERME_WINDOWS_ESCAPE       98

static int terme_windows_escape_p;
static LARGE_INTEGER terme_windows_escape_timeval;

int terme_windows_init(void)
{
	terme_windows_escape_p = 0;
	cleartype(terme_windows_escape_timeval);

	windows_display_init();
	windows_input_init();
	windows_output_init();
	windows_screen_init();
	windows_stream_init();
	return windows_window_init();
}

int terme_windows_signal_p_(int *ret)
{
	if (Window_Exit) {
		abort_execute();
		return Result(ret, 0);
	}

	if (Window_Update) {
		Window_Update = 0;
		return Result(ret, 0);  /*1*/
	}

	return Result(ret, 0);
}

int terme_windows_begin(void)
{
	windows_screen_begin();
	return 0;
}

int terme_windows_end(void)
{
	windows_screen_end();
	return 0;
}

int terme_windows_textmode(void)
{
	windows_screen_textmode();
	return 0;
}

int terme_windows_rawmode(void)
{
	windows_screen_rawmode();
	return 0;
}

void terme_windows_build(void)
{
	addr package, symbol;

	/* (import 'terme) */
	GetConst(SPECIAL_PACKAGE, &package);
	GetValueSymbol(package, &package);
	GetConst(SYSTEM_TERME, &symbol);
	import_package_(package, symbol);

	/* Stream */
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
	BOOL check;

	if (terme_windows_escape_p)
		return 0;
	check = QueryPerformanceCounter(&terme_windows_escape_timeval);
	if (check == 0)
		return 1;
	terme_windows_escape_p = 1;

	return 0;
}

int terme_windows_escape_end(int *ret)
{
	BOOL check;
	LONGLONG a, b, c, diff;
	LARGE_INTEGER now, hz;

	if (terme_windows_escape_p == 0)
		goto error;
	check = QueryPerformanceCounter(&now);
	if (check == 0)
		goto error;
	check = QueryPerformanceFrequency(&hz);
	if (check == 0)
		goto error;
	c = hz.QuadPart;
	a = now.QuadPart * 10000 / c;
	b = terme_windows_escape_timeval.QuadPart * 10000 / c;

	/* minus */
	if (a < b)
		goto normal;

	/* diff */
	diff = a - b;
	if (diff < TERME_WINDOWS_ESCAPE)
		goto normal;

	/* escape */
	terme_windows_escape_p = 0;
	*ret = 1;
	return 0;

normal:
	terme_windows_escape_p = 0;
	*ret = 0;
	return 0;

error:
	terme_windows_escape_p = 0;
	*ret = 0;
	return 1;
}

static int terme_windows_begin_update_(int mode, addr *ret)
{
	addr pos;
	size_t size;

	/* flush */
	if (terme_finish_output()) {
		*ret = Nil;
		return fmte_("terme_finish_output error.", NULL);
	}

	/* paper */
	paper_body_heap(&pos, sizeoft(mode));
	paper_set_memory(pos, 0, sizeoft(mode), (const void *)&mode, &size);
	Check(size != sizeoft(mode), "size error");
	return Result(ret, pos);
}

int terme_windows_begin_default_(addr *ret)
{
	int mode;

	mode = windows_screen_getmode();
	Return(terme_windows_begin_update_(mode, ret));
	windows_screen_textmode();

	return 0;
}


int terme_windows_begin_rawmode_(addr *ret)
{
	int mode;

	mode = windows_screen_getmode();
	Return(terme_windows_begin_update_(mode, ret));
	windows_screen_rawmode();

	return 0;
}

int terme_windows_restore_(addr pos)
{
	int mode;
	size_t size;

	paper_get_memory(pos, 0, sizeoft(mode), (void *)&mode, &size);
	Check(size != sizeoft(mode), "size error");
	windows_screen_setmode(mode);

	return 0;
}
