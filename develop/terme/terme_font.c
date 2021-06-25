#include <stdio.h>
#include "terme_font.h"
#include "terme_output.h"
#include "typedef.h"

static int terme_font_operator(const char *str)
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
	return terme_finish_output();
}

int terme_font(PrintFont value)
{
	const char *str;

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

	return terme_font_operator(str);
}

int terme_text_color(PrintColor value)
{
	const char *str;

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

	return terme_font_operator(str);
}

int terme_back_color(PrintColor value)
{
	const char *str;

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

	return terme_font_operator(str);
}

int terme_cursor_left(void)
{
	return terme_font_operator("\x1B[D");
}

int terme_cursor_right(void)
{
	return terme_font_operator("\x1B[C");
}

int terme_cursor_move(int n)
{
	char data[64];
	snprintf(data, 64, "\x1B[%dG", n + 1);
	return terme_font_operator(data);
}

int terme_cursor_delete_line_left(void)
{
	return terme_font_operator("\x1B[1K");
}

int terme_cursor_delete_line_right(void)
{
	return terme_font_operator("\x1B[K");
}

int terme_cursor_delete_line(void)
{
	return terme_font_operator("\x1B[2K");
}

int terme_cursor_delete_page(void)
{
	return terme_font_operator("\x1B[2J")
		|| terme_font_operator("\x1B[1;1H");
}

