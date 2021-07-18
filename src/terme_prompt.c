#include "condition.h"
#include "execute.h"
#include "print_write.h"
#include "strvect.h"
#include "stream_common.h"
#include "stream_function.h"
#include "terme_arch.h"
#include "terme_display.h"
#include "terme_data.h"
#include "terme_error.h"
#include "terme_history.h"
#include "terme_input.h"
#include "terme_object.h"
#include "terme_output.h"
#include "terme_prompt.h"
#include "terme_screen.h"

/*
 *  default
 */
static int readline_default_terme_newline_(addr pos, addr *ret)
{
	unicode c;
	addr value;
	size_t size, i;

	strvect_length(pos, &size);
	strvect_heap(&value, size + 1UL);
	for (i = 0; i < size; i++) {
		strvect_getc(pos, i, &c);
		Return(strvect_setc_(value, i, c));
	}
	Return(strvect_setc_(value, i, 0x0A));

	return Result(ret, value);
}

static int readline_default_terme_(Execute ptr, addr *ret)
{
	int check;
	addr input, output, prompt, pos;

	GetConst(STREAM_STDIN, &input);
	GetConst(STREAM_STDOUT, &output);
	Return(terme_prompt_get_(ptr, &prompt, NULL));
	Return(fresh_line_stream_(output, NULL));
	if (prompt != Nil) {
		Return(princ_print(ptr, output, prompt));
	}
	Return(finish_output_stream_(output));
	Return(clear_input_stream_(input));
	Return(read_line_stream_(ptr, &pos, &check, input, 0, Nil, 0));
	if (pos == Nil)
		return Result(ret, Nil);
	Return(readline_default_terme_newline_(pos, &pos));
	return Result(ret, pos);
}


/*
 *  readline
 */
static int terme_readline_loop_(Execute ptr, TermeKeyboard *, addr *, int *);

static int terme_readline_ctrl_z_(TermeKeyboard *str)
{
	int mode;

	if (terme_arch_textmode(&mode))
		return terme_fmte_("terme_arch_textmode error.", NULL);
	if (terme_arch_terminal_stop_())
		return terme_fmte_("kill error.", NULL);
	if (mode && terme_arch_rawmode(NULL))
		return terme_fmte_("terme_arch_rawmode error.", NULL);

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

		case 0x09:  /* I, tabular */
			str->type = terme_escape_tabular;
			break;

		case 0x12:  /* R, search */
			str->type = terme_escape_search;
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

static int terme_readline_character_(Execute ptr, unicode c, addr *value, int *ret)
{
	int check;
	unsigned width;

	/* value */
	Return(terme_data_insert_(ptr, c, &width, &check));
	if (check)  /* buffer overflow */
		return Result(ret, 0);

	/* screen */
	Return(terme_screen_push_(ptr));

	/* next */
	return terme_data_next_(ptr);
}

static int terme_readline_code_(Execute ptr, TermeKeyboard *str, addr *value, int *ret)
{
	if (str->c < 0x20)
		return terme_readline_control_(ptr, str, value, ret);
	else
		return terme_readline_character_(ptr, str->c, value, ret);
}

static int terme_readline_up_down_(Execute ptr, int diff)
{
	int check;

	/* select */
	Return(terme_history_select_(ptr, diff, &check));
	if (! check)
		return 0;

	/* screen */
	Return(terme_screen_history_(ptr));
	/* data */
	Return(terme_data_last_(ptr));

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
	unsigned width;

	Return(terme_data_left_(ptr, &width));
	if (width == 0)
		return 0;

	return terme_screen_left_(ptr);
}

static int terme_readline_right_(Execute ptr)
{
	unsigned width;

	Return(terme_data_right_(ptr, &width));
	if (width == 0)
		return 0;

	return terme_screen_right_(ptr);
}

static int terme_readline_return_(Execute ptr, addr *value, int *ret)
{
	/* history */
	Return(terme_history_return_(ptr));
	/* result */
	Return(terme_data_make_(ptr, value, 1));
	return Result(ret, 1);
}

static int terme_readline_delete_(Execute ptr, addr *value, int *ret)
{
	unsigned size;
	int check;

	/* exit */
	Return(terme_data_size_(ptr, &size));
	if (size == 0) {
		*value = Nil;
		return Result(ret, 1);
	}
	*value = Nil;
	*ret = 0;

	/* delete */
	Return(terme_data_delete_(ptr, &check));
	if (! check)
		return 0;

	return terme_screen_delete_(ptr);
}

static int terme_readline_backspace_(Execute ptr)
{
	int check;

	Return(terme_data_backspace_(ptr, &check));
	if (! check)
		return 0;

	return terme_screen_backspace_(ptr);
}

static int terme_readline_first_(Execute ptr)
{
	Return(terme_data_first_(ptr));
	return terme_screen_first_(ptr);
}

static int terme_readline_last_(Execute ptr)
{
	Return(terme_screen_last_(ptr));
	return terme_data_last_(ptr);
}

static int terme_readline_update_(Execute ptr)
{
	return terme_screen_update_(ptr);
}

static int terme_readline_rmleft_(Execute ptr)
{
	int check;

	Return(terme_data_rmleft_(ptr, &check));
	if (! check)
		return 0;

	return terme_screen_rmleft_(ptr);
}

static int terme_readline_rmright_(Execute ptr)
{
	int check;

	Return(terme_data_rmright_(ptr, &check));
	if (! check)
		return 0;

	return terme_screen_rmright_(ptr);
}

static int terme_readline_loop_(Execute ptr, TermeKeyboard *str, addr *value, int *ret)
{
	*ret = 0;
	switch (str->type) {
		case terme_escape_error:
			break;

		case terme_escape_code:
			return terme_readline_code_(ptr, str, value, ret);

		case terme_escape_up:
			return terme_readline_up_(ptr);

		case terme_escape_down:
			return terme_readline_down_(ptr);

		case terme_escape_left:
			return terme_readline_left_(ptr);

		case terme_escape_right:
			return terme_readline_right_(ptr);

		case terme_escape_return:
			return terme_readline_return_(ptr, value, ret);

		case terme_escape_delete:
			return terme_readline_delete_(ptr, value, ret);

		case terme_escape_backspace:
			return terme_readline_backspace_(ptr);

		case terme_escape_first:
			return terme_readline_first_(ptr);

		case terme_escape_last:
			return terme_readline_last_(ptr);

		case terme_escape_update:
			return terme_readline_update_(ptr);

		case terme_escape_rmleft:
			return terme_readline_rmleft_(ptr);

		case terme_escape_rmright:
			return terme_readline_rmright_(ptr);

		case terme_escape_function:
		case terme_escape_tabular:
		case terme_escape_search:
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

	/* begin */
	Return(terme_screen_prompt_(ptr));

	/* loop */
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

static int terme_readline_module_(Execute ptr, addr *ret)
{
	int check, mode;

	Return(terme_data_clear_(ptr));
	Return(terme_screen_clear_(ptr));
	Return(terme_display_clear_(ptr));
	Return(terme_history_clear_(ptr));

	/* readline */
	if (terme_arch_rawmode(&mode)) {
		*ret = Nil;
		return terme_fmte_("terme_arch_rawmode error.", NULL);
	}
	check = terme_readline_call_(ptr, ret);
	if (mode && terme_arch_textmode(NULL)) {
		*ret = Nil;
		return terme_fmte_("terme_arch_textmode error.", NULL);
	}

	return check;
}

int terme_readline_(Execute ptr, addr *ret)
{
	int check;

	Return(terme_root_enable_(ptr, &check));
	if (check)
		return terme_readline_module_(ptr, ret);
	else
		return readline_default_terme_(ptr, ret);
}

