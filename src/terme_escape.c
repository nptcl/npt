#include <stdio.h>
#include "bignum.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "integer.h"
#include "strtype.h"
#include "symbol.h"
#include "terme_escape.h"
#include "terme_output.h"
#include "typedef.h"

static int terme_color_enable(Execute ptr)
{
	addr symbol, pos;

	GetConst(SYSTEM_PROMPT_COLOR, &symbol);
	if (ptr == NULL) {
		GetValueSymbol(symbol, &pos);
	}
	else {
		getspecial_local(ptr, symbol, &pos);
	}

	return pos == Unbound || pos != Nil;
}

static int terme_escape_operator(const char *str)
{
	byte c;
	int i;

	for (i = 0; ; i++) {
		c = (byte)str[i];
		if (c == 0)
			break;
		if (terme_write_byte(c))
			return 1;
	}

	return 0;
}

static int terme_escape_operator_(const char *str)
{
	if (terme_escape_operator(str))
		return fmte_("terme_escape_operator error.", NULL);
	return 0;
}

int terme_font(Execute ptr, PrintFont value)
{
	const char *str;

	if (! terme_color_enable(ptr))
		return 0;
	switch (value) {
		case print_font_reset:      str = "\x1B[0m"; break;
		case print_font_bold:       str = "\x1B[1m"; break;
		case print_font_faint:      str = "\x1B[2m"; break;
		case print_font_italic:     str = "\x1B[3m"; break;
		case print_font_underline:  str = "\x1B[4m"; break;
		case print_font_blink1:     str = "\x1B[5m"; break;
		case print_font_blink2:     str = "\x1B[6m"; break;
		case print_font_reverse:    str = "\x1B[7m"; break;
		case print_font_hide_in:    str = "\x1B[8m"; break;
		case print_font_hide_out:   str = "\x1B[9m"; break;
		default: return 1;
	}

	return terme_escape_operator(str);
}

int terme_text_color(Execute ptr, PrintColor value)
{
	const char *str;

	if (! terme_color_enable(ptr))
		return 0;
	switch (value) {
		case print_color_reset:           str = "\x1B[0m"; break;
		case print_color_black:           str = "\x1B[30m"; break;
		case print_color_red:             str = "\x1B[31m"; break;
		case print_color_green:           str = "\x1B[32m"; break;
		case print_color_yellow:          str = "\x1B[33m"; break;
		case print_color_blue:            str = "\x1B[34m"; break;
		case print_color_magenta:         str = "\x1B[35m"; break;
		case print_color_cyan:            str = "\x1B[36m"; break;
		case print_color_white:           str = "\x1B[37m"; break;
		case print_color_bright_black:    str = "\x1B[90m"; break;
		case print_color_bright_red:      str = "\x1B[91m"; break;
		case print_color_bright_green:    str = "\x1B[92m"; break;
		case print_color_bright_yellow:   str = "\x1B[93m"; break;
		case print_color_bright_blue:     str = "\x1B[94m"; break;
		case print_color_bright_magenta:  str = "\x1B[95m"; break;
		case print_color_bright_cyan:     str = "\x1B[96m"; break;
		case print_color_bright_white:    str = "\x1B[97m"; break;
		default: return 1;
	}

	return terme_escape_operator(str);
}

int terme_back_color(Execute ptr, PrintColor value)
{
	const char *str;

	if (! terme_color_enable(ptr))
		return 0;
	switch (value) {
		case print_color_reset:           str = "\x1B[0m"; break;
		case print_color_black:           str = "\x1B[40m"; break;
		case print_color_red:             str = "\x1B[41m"; break;
		case print_color_green:           str = "\x1B[42m"; break;
		case print_color_yellow:          str = "\x1B[43m"; break;
		case print_color_blue:            str = "\x1B[44m"; break;
		case print_color_magenta:         str = "\x1B[45m"; break;
		case print_color_cyan:            str = "\x1B[46m"; break;
		case print_color_white:           str = "\x1B[47m"; break;
		case print_color_bright_black:    str = "\x1B[100m"; break;
		case print_color_bright_red:      str = "\x1B[101m"; break;
		case print_color_bright_green:    str = "\x1B[102m"; break;
		case print_color_bright_yellow:   str = "\x1B[103m"; break;
		case print_color_bright_blue:     str = "\x1B[104m"; break;
		case print_color_bright_magenta:  str = "\x1B[105m"; break;
		case print_color_bright_cyan:     str = "\x1B[106m"; break;
		case print_color_bright_white:    str = "\x1B[107m"; break;
		default: return 1;
	}

	return terme_escape_operator(str);
}

static int terme_cursor_move_character(int n, byte c)
{
	char data[64];

	if (n <= 0)
		return 0;
	if (n == 1) {
		if (terme_escape_operator("\x1B["))
			return 1;
	}
	else {
		snprintf(data, 64, "\x1B[%d", n);
		if (terme_escape_operator(data))
			return 1;
	}

	return terme_write_byte(c);
}

int terme_cursor_left(int n)
{
	return terme_cursor_move_character(n, 'D');
}

int terme_cursor_right(int n)
{
	return terme_cursor_move_character(n, 'C');
}

int terme_cursor_up(int n)
{
	return terme_cursor_move_character(n, 'A');
}

int terme_cursor_down(int n)
{
	return terme_cursor_move_character(n, 'B');
}

int terme_cursor_move_x(int x)
{
	char data[64];
	snprintf(data, 64, "\x1B[%dG", x + 1);
	return terme_escape_operator(data);
}

int terme_cursor_move(int x, int y)
{
	char data[64];
	snprintf(data, 64, "\x1B[%d;%dH", y + 1, x + 1);
	return terme_escape_operator(data);
}

int terme_cursor_first_up(int n)
{
	return terme_cursor_move_character(n, 'F');
}

int terme_cursor_first_down(int n)
{
	return terme_cursor_move_character(n, 'E');
}

int terme_cursor_delete_line_left(void)
{
	return terme_escape_operator("\x1B[1K");
}

int terme_cursor_delete_line_right(void)
{
	return terme_escape_operator("\x1B[K");
}

int terme_cursor_delete_line(void)
{
	return terme_escape_operator("\x1B[2K");
}

int terme_cursor_delete_page_left(void)
{
	return terme_escape_operator("\x1B[1J")
		|| terme_escape_operator("\x1B[1;1H");
}

int terme_cursor_delete_page_right(void)
{
	return terme_escape_operator("\x1B[J");
}

int terme_cursor_delete_page(void)
{
	return terme_escape_operator("\x1B[2J")
		|| terme_escape_operator("\x1B[1;1H");
}

int terme_cursor_scroll_up(int n)
{
	return terme_cursor_move_character(n, 'T');
}

int terme_cursor_scroll_down(int n)
{
	return terme_cursor_move_character(n, 'S');
}


/*
 *  font
 */
struct terme_font_struct {
	const char *name;
	int value;
};

static struct terme_font_struct terme_struct_code[] = {
	{ "RESET",           0 },
	{ "BOLD",            1 },
	{ "FAINT",           2 },
	{ "ITALIC",          3 },
	{ "UNDERLINE",       4 },
	{ "SLOW-BLINK",      5 },
	{ "RAPID-BLINK",     6 },
	{ "REVERSE",         7 },
	{ "HIDE",            8 },
	{ "STRIKE",          9 },
	{ NULL,              0 }
};

static struct terme_font_struct terme_struct_color[] = {
	{ "BLACK",           0 },
	{ "RED",             1 },
	{ "GREEN",           2 },
	{ "YELLOW",          3 },
	{ "BLUE",            4 },
	{ "MAGENTA",         5 },
	{ "CYAN",            6 },
	{ "WHITE",           7 },
	{ "DEFAULT",         9 },
	{ NULL,              0 }
};

static struct terme_font_struct terme_struct_dark[] = {
	{ "DARK-BLACK",      0 },
	{ "DARK-RED",        1 },
	{ "DARK-GREEN",      2 },
	{ "DARK-YELLOW",     3 },
	{ "DARK-BLUE",       4 },
	{ "DARK-MAGENTA",    5 },
	{ "DARK-CYAN",       6 },
	{ "DARK-WHITE",      7 },
	{ NULL,              0 }
};

static struct terme_font_struct terme_struct_bright[] = {
	{ "BRIGHT-BLACK",    0 },
	{ "BRIGHT-RED",      1 },
	{ "BRIGHT-GREEN",    2 },
	{ "BRIGHT-YELLOW",   3 },
	{ "BRIGHT-BLUE",     4 },
	{ "BRIGHT-MAGENTA",  5 },
	{ "BRIGHT-CYAN",     6 },
	{ "BRIGHT-WHITE",    7 },
	{ NULL,              0 }
};

static int terme_struct_find_(struct terme_font_struct *array,
		addr pos, int *ret, int *value)
{
	int i, check;
	struct terme_font_struct *str;

	if (symbolp(pos))
		GetNameSymbol(pos, &pos);
	if (! stringp(pos))
		goto error;
	for (i = 0; ; i++) {
		str = array + i;
		if (str->name == NULL)
			break;
		Return(string_equal_char_(pos, str->name, &check));
		if (check) {
			if (value)
				*value = str->value;
			return Result(ret, 1);
		}
	}

error:
	return Result(ret, 0);
}

static int terme_font_parser_list_code_(addr list, addr *ret)
{
	int check;
	addr pos;

	Return_getcons(list, &pos, ret);

	/* integer */
	if (integerp(pos)) {
		Return(getint_unsigned_(pos, &check));
		if (0xFF < check)
			return fmte_("Too large integer check, ~S.", pos, NULL);
		return 0;
	}

	/* name */
	Return(terme_struct_find_(terme_struct_code, pos, &check, NULL));
	if (check)
		return 0;

	/* error */
	return fmte_("Invalid value, ~S.", pos, NULL);
}

static int terme_font_parser_list_color_(addr list, addr *ret)
{
	int check;
	addr pos;

	Return_getcons(list, &pos, ret);

	/* color */
	Return(terme_struct_find_(terme_struct_color, pos, &check, NULL));
	if (check)
		return 0;

	/* dark */
	Return(terme_struct_find_(terme_struct_dark, pos, &check, NULL));
	if (check)
		return 0;

	/* bright */
	Return(terme_struct_find_(terme_struct_bright, pos, &check, NULL));
	if (check)
		return 0;

	/* error */
	return fmte_("Invalid value, ~S.", pos, NULL);
}

static int terme_font_parser_list_palette_(addr list, addr *ret, int *value)
{
	int check;
	addr pos;

	Return_getcons(list, &pos, ret);
	/* integer */
	if (! integerp(pos))
		return fmte_("Invalid value, ~S.", pos, NULL);
	Return(getint_unsigned_(pos, &check));
	if (0xFF < check)
		return fmte_("Too large integer value, ~S.", pos, NULL);
	if (value)
		*value = check;

	return 0;
}

static int terme_font_parser_list_rgb_(addr list, addr *ret)
{
	Return(terme_font_parser_list_palette_(list, &list, NULL));
	Return(terme_font_parser_list_palette_(list, &list, NULL));
	Return(terme_font_parser_list_palette_(list, &list, NULL));
	return Result(ret, list);
}

static int terme_font_parser_list_(addr list, addr pos, addr *ret)
{
	int check;

	if (symbolp(pos))
		GetNameSymbol(pos, &pos);
	if (! stringp(pos))
		goto error;

	/* code */
	Return(string_equal_char_(pos, "CODE", &check));
	if (check)
		return terme_font_parser_list_code_(list, ret);

	/* fore */
	Return(string_equal_char_(pos, "FORE", &check));
	if (check)
		return terme_font_parser_list_color_(list, ret);

	/* back */
	Return(string_equal_char_(pos, "BACK", &check));
	if (check)
		return terme_font_parser_list_color_(list, ret);

	/* palfore */
	Return(string_equal_char_(pos, "PALFORE", &check));
	if (check)
		return terme_font_parser_list_palette_(list, ret, NULL);

	/* palback */
	Return(string_equal_char_(pos, "PALBACK", &check));
	if (check)
		return terme_font_parser_list_palette_(list, ret, NULL);

	/* rgbfore */
	Return(string_equal_char_(pos, "RGBFORE", &check));
	if (check)
		return terme_font_parser_list_rgb_(list, ret);

	/* rgbback */
	Return(string_equal_char_(pos, "RGBBACK", &check));
	if (check)
		return terme_font_parser_list_rgb_(list, ret);

	/* error */
error:
	return fmte_("Invalid operator, ~S.", pos, NULL);
}

int terme_font_parser_(addr args)
{
	addr list, pos;

	list = args;
	if (list == Nil)
		return 0; /* reset */
	Return_getcons(list, &pos, &list);
	if (pos == Nil) {
		if (list != Nil)
			return fmte_("Invalid font format, ~S.", args, NULL);
		/* reset */
		return 0;
	}

	/* loop */
	list = args;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(terme_font_parser_list_(list, pos, &list));
	}

	return 0;
}


/* update */
static int terme_font_update_single_(int value)
{
	char data[64];
	snprintf(data, 64, "%d", value);
	return terme_escape_operator_(data);
}

static int terme_font_update_list_code_(addr list, addr *ret)
{
	int check, value;
	addr pos;

	Return_getcons(list, &pos, ret);

	/* integer */
	if (integerp(pos)) {
		Return(getint_unsigned_(pos, &check));
		if (0xFF < check)
			return fmte_("Too large integer check, ~S.", pos, NULL);
		return terme_font_update_single_(check);
	}

	/* name */
	Return(terme_struct_find_(terme_struct_code, pos, &check, &value));
	if (check)
		return terme_font_update_single_(value);

	/* error */
	return fmte_("Invalid value, ~S.", pos, NULL);
}

static int terme_font_bright_mode(Execute ptr)
{
	addr pos;

	GetConst(SYSTEM_PROMPT_BRIGHT, &pos);
	getspecial_local(ptr, pos, &pos);
	if (pos == Unbound)
		pos = Nil;

	return pos != Nil;
}

static int terme_font_update_list_color_mode_(Execute ptr,
		addr list, addr *ret, int dark, int bright)
{
	int check, value;
	addr pos;

	Return_getcons(list, &pos, ret);

	/* color */
	Return(terme_struct_find_(terme_struct_color, pos, &check, &value));
	if (check) {
		if (terme_font_bright_mode(ptr))
			return terme_font_update_single_(value + bright);
		else
			return terme_font_update_single_(value + dark);
	}

	/* dark */
	Return(terme_struct_find_(terme_struct_dark, pos, &check, &value));
	if (check)
		return terme_font_update_single_(value + dark);

	/* bright */
	Return(terme_struct_find_(terme_struct_bright, pos, &check, &value));
	if (check)
		return terme_font_update_single_(value + bright);

	/* error */
	return fmte_("Invalid value, ~S.", pos, NULL);
}

static int terme_font_update_list_fore_(Execute ptr, addr list, addr *ret)
{
	return terme_font_update_list_color_mode_(ptr, list, ret, 30, 90);
}

static int terme_font_update_list_back_(Execute ptr, addr list, addr *ret)
{
	return terme_font_update_list_color_mode_(ptr, list, ret, 40, 100);
}

static int terme_font_update_list_palette_(addr list, addr *ret, int id)
{
	int value;

	Return(terme_font_parser_list_palette_(list, ret, &value));
	Return(terme_font_update_single_(id));
	Return(terme_escape_operator_(";5;"));
	Return(terme_font_update_single_(value));

	return 0;
}

static int terme_font_update_list_palfore_(addr list, addr *ret)
{
	return terme_font_update_list_palette_(list, ret, 38);
}

static int terme_font_update_list_palback_(addr list, addr *ret)
{
	return terme_font_update_list_palette_(list, ret, 48);
}

static int terme_font_update_list_rgb_(addr list, addr *ret, int id)
{
	int r, g, b;

	Return(terme_font_parser_list_palette_(list, &list, &r));
	Return(terme_font_parser_list_palette_(list, &list, &g));
	Return(terme_font_parser_list_palette_(list, &list, &b));
	Return(terme_font_update_single_(id));
	Return(terme_escape_operator_(";2;"));
	Return(terme_font_update_single_(r));
	Return(terme_escape_operator_(";"));
	Return(terme_font_update_single_(g));
	Return(terme_escape_operator_(";"));
	Return(terme_font_update_single_(b));

	return Result(ret, list);
}

static int terme_font_update_list_rgbfore_(addr list, addr *ret)
{
	return terme_font_update_list_rgb_(list, ret, 38);
}

static int terme_font_update_list_rgbback_(addr list, addr *ret)
{
	return terme_font_update_list_rgb_(list, ret, 48);
}

static int terme_font_update_list_(Execute ptr, addr list, addr pos, addr *ret)
{
	int check;

	if (symbolp(pos))
		GetNameSymbol(pos, &pos);
	if (! stringp(pos))
		goto error;

	/* code */
	Return(string_equal_char_(pos, "CODE", &check));
	if (check)
		return terme_font_update_list_code_(list, ret);

	/* fore */
	Return(string_equal_char_(pos, "FORE", &check));
	if (check)
		return terme_font_update_list_fore_(ptr, list, ret);

	/* back */
	Return(string_equal_char_(pos, "BACK", &check));
	if (check)
		return terme_font_update_list_back_(ptr, list, ret);

	/* palfore */
	Return(string_equal_char_(pos, "PALFORE", &check));
	if (check)
		return terme_font_update_list_palfore_(list, ret);

	/* palback */
	Return(string_equal_char_(pos, "PALBACK", &check));
	if (check)
		return terme_font_update_list_palback_(list, ret);

	/* rgbfore */
	Return(string_equal_char_(pos, "RGBFORE", &check));
	if (check)
		return terme_font_update_list_rgbfore_(list, ret);

	/* rgbback */
	Return(string_equal_char_(pos, "RGBBACK", &check));
	if (check)
		return terme_font_update_list_rgbback_(list, ret);

	/* error */
error:
	return fmte_("Invalid operator, ~S.", pos, NULL);
}

int terme_font_update_(Execute ptr, addr args)
{
	int semi;
	addr list, pos;

	list = args;
	if (list == Nil)
		return terme_escape_operator_("\x1B[0m");
	Return_getcons(list, &pos, &list);
	if (pos == Nil)
		return terme_escape_operator_("\x1B[0m");

	/* loop */
	Return(terme_escape_operator_("\x1B["));
	list = args;
	semi = 0;
	while (list != Nil) {
		if (semi) {
			Return(terme_escape_operator_(";"));
		}
		Return_getcons(list, &pos, &list);
		Return(terme_font_update_list_(ptr, list, pos, &list));
		semi = 1;
	}
	Return(terme_escape_operator_("m"));

	return 0;
}

