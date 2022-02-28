#include "character_check.h"
#include "typedef.h"
#include "windows_display.h"
#include "windows_error.h"
#include "windows_output.h"
#include "windows_window.h"
#include "windows_write.h"
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#define WINDOWS_OUTPUT_BUFFER	1024
#define WINDOWS_OUTPUT_ESCAPE	32
static byte Windows_Output_Data[WINDOWS_OUTPUT_BUFFER];
static unsigned Windows_Output_size;

struct windows_output_data {
	int16_t escape[WINDOWS_OUTPUT_ESCAPE];
	byte prev[WINDOWS_OUTPUT_BUFFER];
	byte next[WINDOWS_OUTPUT_BUFFER];
	const byte *ptr;
	unsigned prev_now, prev_size;
	unsigned escape_size;
	unsigned next_size;
	size_t size, now;
};
typedef struct windows_output_data WindowsOutputData;

void windows_output_init(void)
{
	Windows_Output_size = 0;
}

static int windows_output_getc(WindowsOutputData *str, byte *ret)
{
	byte c;

	/* Buffer overflow */
	if (WINDOWS_OUTPUT_BUFFER <= str->next_size) {
		str->next_size = 0;
		*ret = 0;
		return windows_error("WindowsOutputData buffer error.");
	}

	/* Previous buffer */
	if (str->prev_size) {
		c = str->prev[str->prev_now++];
		if (str->prev_size <= str->prev_now) {
			str->prev_size = 0;
			str->prev_now = 0;
		}
		str->next[str->next_size++] = c;
		*ret = c;
		return 0;
	}

	/* Argument buffer */
	if (str->size) {
		c = str->ptr[str->now++];
		if (str->size <= str->now) {
			str->size = 0;
			str->now = 0;
		}
		str->next[str->next_size++] = c;
		*ret = c;
		return 0;
	}

	/* EOF */
	*ret = 0;
	return -1;
}

static int windows_output_escape_up(WindowsOutputData *str)
{
	int16_t v = str->escape[0];
	return windows_write_cursor_up_lock(v);
}

static int windows_output_escape_down(WindowsOutputData *str)
{
	int16_t v = str->escape[0];
	return windows_write_cursor_down_lock(v);
}

static int windows_output_escape_left(WindowsOutputData *str)
{
	int16_t v = str->escape[0];
	return windows_write_cursor_left_lock(v);
}

static int windows_output_escape_right(WindowsOutputData *str)
{
	int16_t v = str->escape[0];
	return windows_write_cursor_right_lock(v);
}

static int windows_output_escape_first_down(WindowsOutputData *str)
{
	int16_t v = str->escape[0];
	return windows_write_first_down_lock(v);
}

static int windows_output_escape_first_up(WindowsOutputData *str)
{
	int16_t v = str->escape[0];
	return windows_write_first_up_lock(v);
}

static int windows_output_escape_move_x(WindowsOutputData *str)
{
	int16_t v = str->escape[0];
	return windows_write_move_x_lock(v);
}

static int windows_output_escape_move_xy(WindowsOutputData *str)
{
	int16_t x, y;
	x = str->escape[0];
	y = str->escape[1];
	return windows_write_move_xy_lock(x, y);
}

static int windows_output_escape_delete_page(WindowsOutputData *str)
{
	int16_t v = str->escape[0];
	return windows_write_delete_page_lock(v);
}

static int windows_output_escape_delete_line(WindowsOutputData *str)
{
	int16_t v = str->escape[0];
	return windows_write_delete_line_lock(v);
}

static int windows_output_escape_scroll_next(WindowsOutputData *str)
{
	int16_t v = str->escape[0];
	return windows_write_scroll_next_lock(v);
}

static int windows_output_escape_scroll_prev(WindowsOutputData *str)
{
	int16_t v = str->escape[0];
	return windows_write_scroll_prev_lock(v);
}

static int windows_output_escape_font(WindowsOutputData *str)
{
	int16_t *ptr;
	unsigned size;

	ptr = str->escape;
	size = str->escape_size;
	return windows_write_font_lock(ptr, size);
}

static int windows_output_escape_home(void)
{
	return 0;
}

static int windows_output_escape_end(void)
{
	return 0;
}

static int windows_output_escape_insert(void)
{
	return 0;
}

static int windows_output_escape_page_up(void)
{
	return 0;
}

static int windows_output_escape_page_down(void)
{
	return 0;
}

static int windows_output_escape_function(int index)
{
	return 0;
}

static int windows_output_character(unicode c)
{
	return windows_write_char_lock(c);
}

static int windows_output_escape(WindowsOutputData *str, int *ret)
{
	byte c;
	char data[16];
	int16_t type;
	int check;
	unsigned datai;

	*ret = 0;
	datai = 0;
	str->escape_size = 0;

	/* first */
	check = windows_output_getc(str, &c);
	if (check < 0)
		goto eof;
	if (c == 0x4F) /* O */
		goto third_4F;
	if (c == 0x5B) /* [ */
		goto third_5B;
	goto invalid;

third_4F: /* O */
	check = windows_output_getc(str, &c);
	if (check < 0)
		goto eof;
	if (0x50 <= c && c <= 0x53) /* PF1 - PF4 */
		goto program;
	goto invalid;

third_5B: /* [ */
	/* parse error */
	if (8 <= datai)
		goto parse_error;
	if (WINDOWS_OUTPUT_ESCAPE <= str->escape_size)
		goto parse_error;

	/* read char */
	check = windows_output_getc(str, &c);
	if (check < 0)
		goto eof;

	/* digit */
	if (isdigit(c)) {
		data[datai++] = c;
		goto third_5B;
	}

	/* not digit */
	if (datai == 0) {
		check = -1;
	}
	else {
		data[datai++] = 0;
		check = atoi(data);
		datai = 0;
		if (check < 0 || 0xFF <= check)
			goto parse_error;
	}
	str->escape[str->escape_size++] = (int16_t)check;

	/* separator */
	if (c == ';' || c == ':')
		goto third_5B;

	/* operator */
	if (c == 0x7E) /* ~ */
		goto forth_7E;
	if (c == 0x41) /* A */
		goto escape_up;
	if (c == 0x42) /* B */
		goto escape_down;
	if (c == 0x43) /* C */
		goto escape_right;
	if (c == 0x44) /* D */
		goto escape_left;
	if (c == 0x45) /* E */
		goto escape_first_down;
	if (c == 0x46) /* F */
		goto escape_first_up;
	if (c == 0x47) /* G */
		goto escape_move_x;
	if (c == 0x48) /* H */
		goto escape_move_xy;
	if (c == 0x4A) /* J */
		goto escape_delete_page;
	if (c == 0x4B) /* K */
		goto escape_delete_line;
	if (c == 0x53) /* S */
		goto escape_scroll_next;
	if (c == 0x54) /* T */
		goto escape_scroll_prev;
	if (c == 0x66) /* f */
		goto escape_move_xy;
	if (c == 0x6D) /* m */
		goto escape_font;
	goto invalid;

forth_7E:
	type = str->escape[0];
	if (type < 0)
		type = 0;
	if (type == 1)
		goto escape_home;
	if (type == 2)
		goto escape_insert;
	if (type == 4)
		goto escape_end;
	if (type == 5)
		goto escape_page_up;
	if (type == 6)
		goto escape_page_down;
	if (11 <= type && type <= 19)
		goto function1;
	goto invalid;

escape_up:
	str->next_size = 0;
	return windows_output_escape_up(str);

escape_down:
	str->next_size = 0;
	return windows_output_escape_down(str);

escape_right:
	str->next_size = 0;
	return windows_output_escape_right(str);

escape_left:
	str->next_size = 0;
	return windows_output_escape_left(str);

escape_first_down:
	str->next_size = 0;
	return windows_output_escape_first_down(str);

escape_first_up:
	str->next_size = 0;
	return windows_output_escape_first_up(str);

escape_move_x:
	str->next_size = 0;
	return windows_output_escape_move_x(str);

escape_move_xy:
	str->next_size = 0;
	return windows_output_escape_move_xy(str);

escape_delete_page:
	str->next_size = 0;
	return windows_output_escape_delete_page(str);

escape_delete_line:
	str->next_size = 0;
	return windows_output_escape_delete_line(str);

escape_scroll_next:
	str->next_size = 0;
	return windows_output_escape_scroll_next(str);

escape_scroll_prev:
	str->next_size = 0;
	return windows_output_escape_scroll_prev(str);

escape_font:
	str->next_size = 0;
	return windows_output_escape_font(str);

escape_home:
	str->next_size = 0;
	return windows_output_escape_home();

escape_insert:
	str->next_size = 0;
	return windows_output_escape_insert();

escape_end:
	str->next_size = 0;
	return windows_output_escape_end();

escape_page_up:
	str->next_size = 0;
	return windows_output_escape_page_up();

escape_page_down:
	str->next_size = 0;
	return windows_output_escape_page_down();

program:
	str->next_size = 0;
	return windows_output_escape_function((c - 0x50) + 1);

function1:
	str->next_size = 0;
	return windows_output_escape_function(type - 10);

eof:
	*ret = -1;
	return 0;

invalid:
	str->next_size = 0;
	return 0;

parse_error:
	str->next_size = 0;
	return 0;
}

static int windows_output_unicode(WindowsOutputData *str, int *ret)
{
	byte c;
	int check;
	unicode value;

	check = windows_output_getc(str, &c);
	if (check < 0)
		goto eof;
	if (c == 0x1B)
		goto escape;
	if (0x00 <= c && c <= 0x7F)
		goto sequence1;
	if (0xC2 <= c && c <= 0xDF)
		goto sequence2;
	if (0xE0 <= c && c <= 0xEF)
		goto sequence3;
	if (0xF0 <= c && c <= 0xF7)
		goto sequence4;
	goto invalid;

sequence1:
	value = (unicode)c;
	goto normal;

sequence2:
	value = (0x1F & c) << 6;
	check = windows_output_getc(str, &c);
	if (check < 0)
		goto eof;
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	value |= 0x3F & c;
	if (value < 0x80)
		goto invalid;
	goto normal;

sequence3:
	value = (0x0F & c) << 12;
	check = windows_output_getc(str, &c);
	if (check < 0)
		goto eof;
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	value |= (0x3F & c) << 6;
	check = windows_output_getc(str, &c);
	if (check < 0)
		goto eof;
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	value |= 0x3F & c;
	if (value < 0x0800)
		goto invalid;
	if (isSurrogatePair(value))
		goto invalid;
	goto normal;

sequence4:
	value = (0x07 & c) << 18;
	check = windows_output_getc(str, &c);
	if (check < 0)
		goto eof;
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	value |= (0x3F & c) << 12;
	check = windows_output_getc(str, &c);
	if (check < 0)
		goto eof;
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	value |= (0x3F & c) << 6;
	check = windows_output_getc(str, &c);
	if (check < 0)
		goto eof;
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	value |= 0x3F & c;
	if (value < 0x010000)
		goto invalid;
	if (UnicodeCount <= value)
		goto size_error;
	goto normal;

escape:
	str->next[0] = c;
	str->next_size = 1;
	*ret = 0;
	return 0;

normal:
	str->next_size = 0;
	*ret = 1;
	return windows_output_character(value);

invalid:
	str->next[0] = c;
	str->next_size = 1;
	*ret = 1;
	return 0; /* ignore */

size_error:
	str->next_size = 0;
	*ret = 1;
	return 0; /* ignore */

eof:
	*ret = -1;
	return 0;
}

static int windows_output_table(WindowsOutputData *str)
{
	int check;

	for (;;) {
		/* UTF-8 */
		if (windows_output_unicode(str, &check))
			return 1;
		if (check < 0) /* eof */
			break;
		if (check) /* read */
			continue;

		/* Escape Sequence */
		if (windows_output_escape(str, &check))
			return 1;
		if (check < 0) /* eof */
			break;
	}

	return 0;
}

int windows_output_write(const void *data, size_t size, size_t *ret)
{
	struct windows_output_data str;

	/* prev */
	str.prev_now = 0;
	str.prev_size = Windows_Output_size;
	if (Windows_Output_size)
		memcpy(str.prev, Windows_Output_Data, Windows_Output_size);

	/* argument */
	str.ptr = (const byte *)data;
	str.size = size;
	str.now = 0;
	str.next_size = 0;
	str.escape_size = 0;

	/* result */
	if (windows_output_table(&str)) {
		*ret = 0;
		return 1;
	}
	if (str.next_size)
		memcpy(Windows_Output_Data, str.next, str.next_size);
	Windows_Output_size = str.next_size;
	*ret = size;
	return 0;
}

void windows_output_clear(void)
{
	Windows_Output_size = 0;
}
