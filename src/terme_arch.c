#include "arch.h"
#include "condition.h"
#include "define.h"
#include "paper.h"
#include "terme_arch.h"
#include "terme_escape.h"
#include "terme_output.h"
#include "typedef.h"

static unsigned terme_arch_x;
static unsigned terme_arch_y;
static int terme_arch_textmode_p;
static int terme_arch_enable_p;
static int terme_arch_signal_value;

#if defined(LISP_TERME_UNIX)
#include "terme_unix.h"
#elif defined(LISP_TERME_WINDOWS)
#include "windows_terme.h"
#endif


/*
 *  terme-init
 */
int terme_arch_init(void)
{
	terme_arch_x = 0;
	terme_arch_y = 0;
	terme_arch_textmode_p = 0;
	terme_arch_enable_p = 0;
	terme_arch_signal_value = 0;

#if defined(LISP_TERME_UNIX)
	return terme_unix_init();
#elif defined(LISP_TERME_WINDOWS)
	return terme_windows_init();
#else
	return 0;
#endif
}


/*
 *  window size
 */
int terme_arch_size_update(void)
{
#if defined(LISP_TERME_UNIX)
	return getwidth_arch(&terme_arch_x, &terme_arch_y);
#elif defined(LISP_TERME_WINDOWS)
	return getwidth_arch(&terme_arch_x, &terme_arch_y);
#else
	terme_arch_x = 0;
	terme_arch_y = 0;
	return 0;
#endif
}

void terme_arch_size_get(unsigned *ret_x, unsigned *ret_y)
{
	if (ret_x)
		*ret_x = terme_arch_x;
	if (ret_y)
		*ret_y = terme_arch_y;
}


/*
 *  terme-signal
 */
int terme_arch_signal_p(void)
{
	return terme_arch_signal_value;
}

void terme_arch_signal_clear(void)
{
	terme_arch_signal_value = 0;
}


/*
 *  terme-begin
 */
int terme_arch_begin(void)
{
#if defined(LISP_TERME_UNIX)
	terme_arch_textmode_p = 1;
	terme_arch_enable_p = 1;
	return terme_unix_begin();
#elif defined(LISP_TERME_WINDOWS)
	terme_arch_textmode_p = 1;
	terme_arch_enable_p = 1;
	return terme_windows_begin();
#elif defined(LISP_TERME)
	terme_arch_textmode_p = 1;
	terme_arch_enable_p = 0;
	return 0;
#else
	terme_arch_enable_p = 0;
	return 0;
#endif
}


/*
 *  terme-end
 */
int terme_arch_end(void)
{
	if (! terme_arch_enable_p)
		return 0;
#if defined(LISP_TERME_UNIX)
	return terme_unix_end();
#elif defined(LISP_TERME_WINDOWS)
	return terme_windows_end();
#else
	return 0;
#endif
}


/*
 *  terme-switch
 */
int terme_arch_textmode(int *ret)
{
	int check;

	if (ret == NULL)
		ret = &check;
	if (! terme_arch_enable_p) {
		*ret = 0;
		return 0;
	}
	if (terme_arch_textmode_p) {
		*ret = 0;
		return 0;
	}
	terme_arch_textmode_p = 1;
#if defined(LISP_TERME_UNIX)
	if (terme_unix_textmode()) {
		*ret = 0;
		return 0;
	}
	*ret = 1;
#elif defined(LISP_TERME_WINDOWS)
	if (terme_windows_textmode()) {
		*ret = 0;
		return 0;
	}
	*ret = 1;
#else
	*ret = 0;
#endif

	return 0;
}

int terme_arch_rawmode(int *ret)
{
	int check;

	if (ret == NULL)
		ret = &check;
	if (! terme_arch_enable_p) {
		*ret = 0;
		return 0;
	}
	if (! terme_arch_textmode_p) {
		*ret = 0;
		return 0;
	}
	terme_arch_textmode_p = 0;
#if defined(LISP_TERME_UNIX)
	if (terme_unix_rawmode()) {
		*ret = 0;
		return 0;
	}
	*ret = 1;
#elif defined(LISP_TERME_WINDOWS)
	if (terme_windows_rawmode()) {
		*ret = 0;
		return 0;
	}
	*ret = 1;
#else
	*ret = 0;
#endif

	return 0;
}


/*
 *  terme-build
 */
void terme_arch_build(void)
{
#ifdef LISP_TERME_WINDOWS
	terme_windows_build();
#endif
}


/*
 *  wait
 */
int terme_arch_select(int *ret)
{
	if (! terme_arch_enable_p) {
		*ret = 0;
		return 0;
	}
#if defined(LISP_TERME_UNIX)
	return terme_unix_select(ret);
#elif defined(LISP_TERME_WINDOWS)
	return terme_windows_select(ret);
#else
	*ret = 0;
	return 0;
#endif
}

int terme_arch_wait_integer(int *ret, int value)
{
	if (! terme_arch_enable_p) {
		*ret = 0;
		return 0;
	}
#if defined(LISP_TERME_UNIX)
	return terme_unix_wait_integer(ret, value);
#elif defined(LISP_TERME_WINDOWS)
	return terme_windows_wait_integer(ret, value);
#else
	*ret = 0;
	return 0;
#endif
}

int terme_arch_wait_float(int *ret, double value)
{
	if (! terme_arch_enable_p) {
		*ret = 0;
		return 0;
	}
#if defined(LISP_TERME_UNIX)
	return terme_unix_wait_float(ret, value);
#elif defined(LISP_TERME_WINDOWS)
	return terme_windows_wait_float(ret, value);
#else
	*ret = 0;
	return 0;
#endif
}

int terme_arch_read(void *data, size_t size, size_t *ret)
{
	if (! terme_arch_enable_p) {
		*ret = 0;
		return 1;
	}
#if defined(LISP_TERME_UNIX)
	return terme_unix_read(data, size, ret);
#elif defined(LISP_TERME_WINDOWS)
	return terme_windows_read(data, size, ret);
#else
	*ret = 0;
	return 1;
#endif
}

int terme_arch_write(const void *data, size_t size, size_t *ret)
{
	if (! terme_arch_enable_p) {
		*ret = 0;
		return 1;
	}
#if defined(LISP_TERME_UNIX)
	return terme_unix_write(data, size, ret);
#elif defined(LISP_TERME_WINDOWS)
	return terme_windows_write(data, size, ret);
#else
	*ret = 0;
	return 1;
#endif
}


/*
 *  Ctrl + C
 */
int terme_arch_terminal_sigint_(void)
{
	if (! terme_arch_enable_p)
		return 0;
#ifdef LISP_TERME_UNIX
	return terme_unix_terminal_sigint_();
#else
	return 0;
#endif
}


/*
 *  Ctrl + Z
 */
int terme_arch_terminal_stop_(void)
{
	if (! terme_arch_enable_p)
		return 0;
#ifdef LISP_TERME_UNIX
	return terme_unix_terminal_stop_();
#else
	return 0;
#endif
}


/*
 *  enable
 */
int terme_arch_enable(void)
{
	return terme_arch_enable_p;
}


/*
 *  escape
 */
int terme_arch_escape_begin(void)
{
#if defined(LISP_TERME_UNIX)
	return terme_unix_escape_begin();
#elif defined(LISP_TERME_WINDOWS)
	return terme_windows_escape_begin();
#else
	return 0;
#endif
}

int terme_arch_escape_end(int *ret)
{
#if defined(LISP_TERME_UNIX)
	return terme_unix_escape_end(ret);
#elif defined(LISP_TERME_WINDOWS)
	return terme_windows_escape_end(ret);
#else
	*ret = 0;
	return 0;
#endif
}


/*
 *  font
 */
int font_arch_terme(Execute ptr, PrintFont value)
{
	if (! terme_arch_enable_p)
		return 0;

	return terme_font(ptr, value)
		|| terme_finish_output();
}

int text_color_arch_terme(Execute ptr, PrintColor value)
{
	if (! terme_arch_enable_p)
		return 0;

	return terme_text_color(ptr, value)
		|| terme_finish_output();
}

int back_color_arch_terme(Execute ptr, PrintColor value)
{
	if (! terme_arch_enable_p)
		return 0;

	return terme_back_color(ptr, value)
		|| terme_finish_output();
}


/*
 *  call begin-default
 */
int terme_arch_begin_default_(addr *ret)
{
#if defined(LISP_TERME_UNIX)
	return terme_unix_begin_default_(ret);
#elif defined(LISP_TERME_WINDOWS)
	return terme_windows_begin_default_(ret);
#else
	*ret = Nil;
	return fmte_("TERME is not enabled.", NULL);
#endif
}


/*
 *  call begin-rawmode
 */
int terme_arch_begin_rawmode_(addr *ret)
{
#if defined(LISP_TERME_UNIX)
	return terme_unix_begin_rawmode_(ret);
#elif defined(LISP_TERME_WINDOWS)
	return terme_windows_begin_rawmode_(ret);
#else
	*ret = Nil;
	return fmte_("TERME is not enabled.", NULL);
#endif
}


/*
 *  call begin-restore
 */
int terme_arch_restore_(addr pos)
{
#if defined(LISP_TERME_UNIX)
	return terme_unix_restore_(pos);
#elif defined(LISP_TERME_WINDOWS)
	return terme_windows_restore_(pos);
#else
	return fmte_("TERME is not enabled.", NULL);
#endif
}

