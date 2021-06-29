#include <sys/types.h>
#include <signal.h>
#include <unistd.h>
#include "condition.h"
#include "copy.h"
#include "eastasian_unicode.h"
#include "print_font.h"
#include "stream.h"
#include "stream_common.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "terme_call.h"
#include "terme_data.h"
#include "terme_escape.h"
#include "terme_input.h"
#include "terme_output.h"
#include "terme_prompt.h"
#include "terme_screen.h"
#include "terme_value.h"

static int terme_prompt_size;
static int terme_prompt_width;
static int terme_value_now;
static int terme_value_width;
static int terme_history_now;

/*
 *  error
 */
static int terme_fmte_va_(addr format, addr args)
{
	int mode, check;

	if (terme_switch_textmode(&mode)) {
		Abort("terme_switch_textmode error.");
		return 0;
	}
	check = call_simple_error_(NULL, format, args);
	if (mode && terme_switch_rawmode(NULL)) {
		Abort("terme_switch_rawmode error.");
		return 0;
	}

	return check;
}

static int terme_fmte_(const char *str, ...)
{
	addr format, args;
	va_list va;

	if (terme_fresh_line()) {
		Abort("terme_fresh_line error.");
		return 0;
	}
	if (terme_finish_output()) {
		Abort("terme_finish_output error.");
		return 0;
	}
	strvect_char_heap(&format, str);
	va_start(va, str);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);
	return terme_fmte_va_(format, args);
}


/*
 *  prompt
 */
int terme_prompt_(Execute ptr, addr pos, enum prompt_mode mode)
{
	Check(! stringp(pos), "type error");
	return terme_set_prompt_(ptr, pos, mode);
}

static int terme_prompt_string_(addr pos)
{
	int width, check;
	unicode c;
	size_t size, i;

	string_length(pos, &size);
	width = 0;
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		check = (int)eastasian_width(c);
		if (terme_write_char(c, check))
			return terme_fmte_("terme_write_char error.", NULL);
		width += check;
	}
	terme_prompt_width = width;

	return 0;
}

static PrintColor terme_prompt_color_bright(Execute ptr, enum prompt_mode mode)
{
	switch (mode) {
		case prompt_for:
			return print_color_bright_yellow;

		case prompt_debugger:
		case prompt_inspect:
		case prompt_step:
			return print_color_bright_blue;

		case prompt_eval:
		default:
			return print_color_bright_green;
	}
}

static PrintColor terme_prompt_color_dark(Execute ptr, enum prompt_mode mode)
{
	switch (mode) {
		case prompt_for:
			return print_color_yellow;

		case prompt_debugger:
		case prompt_inspect:
		case prompt_step:
			return print_color_blue;

		case prompt_eval:
		default:
			return print_color_green;
	}
}

static PrintColor terme_prompt_color(Execute ptr, enum prompt_mode mode)
{
	addr pos;

	GetConst(SYSTEM_PROMPT_BRIGHT, &pos);
	getspecial_local(ptr, pos, &pos);
	if (pos == Nil)
		return terme_prompt_color_dark(ptr, mode);

	/* unbound or (not nil) */
	return terme_prompt_color_bright(ptr, mode);
}

static int terme_prompt_output_(Execute ptr)
{
	int check;
	enum prompt_mode mode;
	PrintColor color;
	addr pos, io;
	size_t size;

	/* special */
	Return(terme_get_prompt_(ptr, &pos, &mode));
	if (pos == Nil)
		return 0;

	/* prompt */
	string_length(pos, &size);
	terme_prompt_size = (int)size;

	/* fresh-line */
	Return(terminal_io_stream_(ptr, &io));
	Return(fresh_line_stream_(io, &check));
	if (terme_font(ptr, print_font_reset))
		goto error;
	color = terme_prompt_color(ptr, mode);
	if (terme_text_color(ptr, color))
		goto error;
	Return(terme_prompt_string_(pos));
	if (terme_font(ptr, print_font_reset))
		goto error;
	if (terme_finish_output())
		goto error;
	return 0;

error:
	return terme_fmte_("terme output error.", NULL);
}


/*
 *  readline
 */
static int terme_readline_loop_(Execute ptr, TermeKeyboard *, addr *, int *);

static int terme_readline_ctrl_z_(TermeKeyboard *str)
{
	int mode;
	pid_t pid;

	if (terme_switch_textmode(&mode))
		return terme_fmte_("terme_switch_textmode error.", NULL);

	pid = getpid();
	if (kill(pid, SIGTSTP))
		return terme_fmte_("kill error.", NULL);

	if (mode && terme_switch_rawmode(NULL))
		return terme_fmte_("terme_switch_rawmode error.", NULL);

	/* ignore */
	str->type = terme_escape_error;
	return 0;
}

static int terme_readline_control_(Execute ptr,
		TermeKeyboard *str, addr *value, int *ret)
{
	switch (str->c) {
		case 0x03:	/* C */
			return terme_fmte_("Ctrl + C", NULL);

		case 0x04:  /* D, delete */
			str->type = terme_escape_delete;
			break;

		case 0x0A:  /* J */
		case 0x0D:  /* Return, Enter, Ctrl+M */
			str->type = terme_escape_return;
			break;

		case 0x10:  /* P, up */
			str->type = terme_escape_up;
			break;

		case 0x0E:  /* N, down */
			str->type = terme_escape_down;
			break;

		case 0x06:  /* F, left */
			str->type = terme_escape_right;
			break;

		case 0x02:  /* B, right */
			str->type = terme_escape_left;
			break;

		case 0x01:  /* A */
			str->type = terme_escape_first;
			break;

		case 0x05:  /* E */
			str->type = terme_escape_last;
			break;

		case 0x0C:  /* L */
			str->type = terme_escape_update;
			break;

		case 0x08:  /* H, backspace */
			str->type = terme_escape_backspace;
			break;

		case 0x15:  /* U, rmleft */
			str->type = terme_escape_rmleft;
			break;

		case 0x0B:  /* K, rmright */
			str->type = terme_escape_rmright;
			break;

		case 0x09:  /* I, tab */
			str->type = terme_escape_tab;
			break;

		case 0x1A:  /* Z */
			Return(terme_readline_ctrl_z_(str));
			break;

		default:
			str->type = terme_escape_error;
			break;
	}

	return terme_readline_loop_(ptr, str, value, ret);
}

static int terme_readline_write_line_(Execute ptr, int first)
{
	int i, check;
	unicode c;

	for (i = first; ; i++) {
		Return(terme_data_get_(ptr, i, &c, &check));
		if (check < 0)
			break;
		if (terme_write_char(c, check))
			return terme_fmte_("terme_write_char error.", NULL);
	}

	return 0;
}

static int terme_readline_code_(Execute ptr, TermeKeyboard *str, addr *value, int *ret)
{
	int check;
	unicode c;

	c = str->c;
	if (c < 0x20)
		return terme_readline_control_(ptr, str, value, ret);

	/* value */
	Return(terme_data_push_(ptr, terme_value_now, c, &check));
	if (check < 0)  /* buffer overflow */
		return Result(ret, 0);
	terme_value_width += check;

	/* output */
	if (terme_readline_write_line_(ptr, terme_value_now))
		return terme_fmte_("terme_write_char error.", NULL);
	if (terme_cursor_move(terme_prompt_width + terme_value_width))
		return terme_fmte_("terme_cursor_move error.", NULL);
	terme_value_now++;

	return Result(ret, 0);
}

static int terme_readline_up_down_(Execute ptr, int diff)
{
	int index, check;

	if (terme_history_now == 0) {
		Return(terme_history_save_(ptr));
	}
	index = terme_history_now + diff;
	Return(terme_history_update_(ptr, index, &check));
	if (! check)
		return 0;
	terme_history_now = index;

	/* output */
	Return(terme_data_size_width_(ptr, &terme_value_now, &terme_value_width));
	if (terme_cursor_delete_line())
		return terme_fmte_("terme_cursor_delete_line error.", NULL);
	if (terme_cursor_move(0))
		return terme_fmte_("terme_cursor_move error.", NULL);
	Return(terme_prompt_output_(ptr));
	Return(terme_readline_write_line_(ptr, 0));
	if (terme_finish_output())
		return terme_fmte_("terme_finish_output error.", NULL);

	return 0;
}

static int terme_readline_up_(Execute ptr)
{
	return terme_readline_up_down_(ptr, 1);
}

static int terme_readline_down_(Execute ptr)
{
	return terme_readline_up_down_(ptr, -1);
}

static int terme_readline_left_(Execute ptr)
{
	int check;

	if (terme_value_now == 0)
		return 0;
	Return(terme_data_get_width_(ptr, terme_value_now - 1UL, &check));
	if (check < 0)
		return terme_fmte_("terme_data_get_width_ error.", NULL);
	check = (check? 2: 1);
	if (terme_cursor_left(check))
		return terme_fmte_("terme_cursor_left error.", NULL);
	terme_value_now--;
	terme_value_width -= check;

	return 0;
}

static int terme_readline_right_(Execute ptr)
{
	int check;

	Return(terme_data_size_(ptr, &check));
	if (check <= terme_value_now)
		return 0;
	Return(terme_data_get_width_(ptr, terme_value_now, &check));
	if (check < 0)
		return terme_fmte_("terme_data_get_width_ error.", NULL);
	check = (check? 2: 1);
	if (terme_cursor_right(check))
		return terme_fmte_("terme_cursor_right error.", NULL);
	terme_value_now++;
	terme_value_width += check;

	return 0;
}

static int terme_readline_return(Execute ptr, addr *value, int *ret)
{
	terme_history_now = 0;
	Return(terme_data_make_(ptr, value));
	return Result(ret, 1);
}

static int terme_readline_backspace_(Execute ptr)
{
	int width, check;

	if (terme_value_now <= 0)
		return 0;
	Return(terme_data_get_width_(ptr, terme_value_now - 1UL, &width));
	if (width < 0)
		return terme_fmte_("terme_data_get_width_ error.", NULL);
	width = (width? 2: 1);

	/* backspace */
	Return(terme_data_delete_(ptr, terme_value_now - 1UL, &check));
	if (! check)
		return 0;
	terme_value_now--;
	terme_value_width -= width;

	/* output */
	if (terme_cursor_left(width))
		return terme_fmte_("terme_cursor_left error.", NULL);
	if (terme_cursor_delete_line_right())
		return terme_fmte_("terme_cursor_delete_line_right error.", NULL);
	Return(terme_readline_write_line_(ptr, terme_value_now));
	if (terme_cursor_move(terme_prompt_width + terme_value_width))
		return terme_fmte_("terme_cursor_move error.", NULL);

	return 0;
}

static int terme_readline_first_(Execute ptr)
{
	terme_value_now = 0;
	terme_value_width = 0;
	if (terme_cursor_move(terme_prompt_width))
		return terme_fmte_("terme_cursor_move error.", NULL);

	return 0;
}

static int terme_readline_last_(Execute ptr)
{
	int now, width;

	Return(terme_data_size_(ptr, &now));
	Return(terme_data_allwidth_(ptr, &width));
	terme_value_now = now;
	terme_value_width = width;
	if (terme_cursor_move(terme_prompt_width + width))
		return terme_fmte_("terme_cursor_move error.", NULL);

	return 0;
}

static int terme_readline_update_(Execute ptr)
{
	int base;

	/* cursor position */
	base = terme_prompt_width + terme_value_width;

	/* all clean */
	if (terme_cursor_delete_page())
		return terme_fmte_("terme_cursor_delete_page error.", NULL);

	/* output prompt */
	Return(terme_prompt_output_(ptr));

	/* output text */
	Return(terme_readline_write_line_(ptr, 0));
	if (terme_cursor_move(base))
		return terme_fmte_("terme_cursor_move error.", NULL);

	return 0;
}

static int terme_readline_delete_(Execute ptr, addr *value, int *ret)
{
	int check;

	/* exit */
	Return(terme_data_size_(ptr, &check));
	if (check == 0) {
		*value = Nil;
		return Result(ret, 1);
	}

	/* delete */
	Return(terme_data_delete_(ptr, terme_value_now, &check));
	if (! check)
		return 0;
	if (terme_cursor_delete_line_right())
		return terme_fmte_("terme_cursor_delete_line_right error.", NULL);
	Return(terme_readline_write_line_(ptr, terme_value_now));
	if (terme_cursor_move(terme_prompt_width + terme_value_width))
		return terme_fmte_("terme_cursor_move error.", NULL);

	return 0;
}

static int terme_readline_rmleft_(Execute ptr)
{
	int check;

	/* data */
	Return(terme_data_delete_left_(ptr, terme_value_now, &check));
	if (! check)
		return 0;
	terme_value_now = 0;
	terme_value_width = 0;

	/* cursor */
	if (terme_cursor_move(terme_prompt_width))
		return terme_fmte_("terme_cursor_move error.", NULL);
	if (terme_cursor_delete_line_right())
		return terme_fmte_("terme_cursor_delete_line_right error.", NULL);
	Return(terme_readline_write_line_(ptr, 0));
	if (terme_cursor_move(terme_prompt_width))
		return terme_fmte_("terme_cursor_move error.", NULL);

	return 0;
}

static int terme_readline_rmright_(Execute ptr)
{
	int check;

	Return(terme_data_delete_right_(ptr, terme_value_now, &check));
	if (! check)
		return 0;
	if (terme_cursor_delete_line_right())
		return terme_fmte_("terme_cursor_delete_line_right error.", NULL);

	return 0;
}

static int terme_readline_loop_(Execute ptr, TermeKeyboard *str, addr *value, int *ret)
{
	*ret = 0;
	switch (str->type) {
		case terme_escape_error:
			break;

		case terme_escape_code: /* TODO */
			return terme_readline_code_(ptr, str, value, ret);

		case terme_escape_up: /* TODO */
			return terme_readline_up_(ptr);

		case terme_escape_down: /* TODO */
			return terme_readline_down_(ptr);

		case terme_escape_left: /* TODO */
			return terme_readline_left_(ptr);

		case terme_escape_right: /* TODO */
			return terme_readline_right_(ptr);

		case terme_escape_return: /* TODO */
			return terme_readline_return(ptr, value, ret);

		case terme_escape_backspace: /* TODO */
			return terme_readline_backspace_(ptr);

		case terme_escape_first: /* TODO */
			return terme_readline_first_(ptr);

		case terme_escape_last: /* TODO */
			return terme_readline_last_(ptr);

		case terme_escape_update: /* TODO */
			return terme_readline_update_(ptr);

		case terme_escape_delete: /* TODO */
			return terme_readline_delete_(ptr, value, ret);

		case terme_escape_rmleft: /* TODO */
			return terme_readline_rmleft_(ptr);

		case terme_escape_rmright: /* TODO */
			return terme_readline_rmright_(ptr);

		case terme_escape_function:
		case terme_escape_tab:
			break; /* ignore */

		default:
			return terme_fmte_("terme_readline error.", NULL);
	}

	return 0;
}

static int terme_readline_call_(Execute ptr, addr *ret)
{
	int check;
	addr pos;
	TermeKeyboard str;

	/* loop */
	Return(terme_prompt_output_(ptr));
	pos = Nil;
	for (;;) {
		if (terme_read_keyboard(&str))
			continue;
		Return(terme_readline_loop_(ptr, &str, &pos, &check));
		if (check)
			break;
	}

	/* finish */
	if (pos != Nil) {
		if (terme_fresh_line())
			goto error;
	}
	if (terme_finish_output())
		goto error;
	return Result(ret, pos);

error:
	*ret = Nil;
	return terme_fmte_("terme output error.", NULL);
}

int terme_readline_(Execute ptr, addr *ret)
{
	int mode, check;

	/* initialize */
	terme_prompt_size = 0;
	terme_prompt_width = 0;
	terme_value_now = 0;
	terme_value_width = 0;
	terme_history_now = 0;
	Return(terme_data_init_(ptr));

	/* readline */
	if (terme_switch_rawmode(&mode)) {
		*ret = Nil;
		return terme_fmte_("terme_switch_rawmode error.", NULL);
	}
	check = terme_readline_call_(ptr, ret);
	if (mode && terme_switch_textmode(NULL)) {
		*ret = Nil;
		return terme_fmte_("terme_switch_textmode error.", NULL);
	}

	return check;
}

