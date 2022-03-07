#include "constant.h"
#include "eastasian_unicode.h"
#include "execute.h"
#include "heap.h"
#include "stream.h"
#include "stream_common.h"
#include "strtype.h"
#include "symbol.h"
#include "terme_arch.h"
#include "terme_display.h"
#include "terme_data.h"
#include "terme_error.h"
#include "terme_escape.h"
#include "terme_object.h"
#include "terme_output.h"
#include "terme_screen.h"
#include "terme_write.h"
#include "typedef.h"

/*
 *  object
 */
struct terme_screen_struct {
	unsigned window_x, window_y;
	unsigned prompt_x, prompt_y;
	unsigned now_x, now_y, last_y;
};

static struct terme_screen_struct *struct_terme_screen(addr pos)
{
	Check(! terme_screen_p(pos), "type error");
	return (struct terme_screen_struct *)terme_pointer(pos);
}

void terme_screen_build(addr *ret)
{
	addr pos;
	struct terme_screen_struct *str;

	heap_body(&pos, LISPSYSTEM_TERME, sizeoft(struct terme_screen_struct));
	terme_set_type(pos, terme_type_screen);
	str = struct_terme_screen(pos);
	str->window_x = 0;
	str->window_y = 0;
	str->prompt_x = 0;
	str->prompt_y = 0;
	str->now_x = 0;
	str->now_y = 0;
	str->last_y = 0;
	*ret = pos;
}


/*
 *  clear
 */
int terme_screen_clear_(Execute ptr)
{
	unsigned screen_x, screen_y;
	addr pos;
	struct terme_screen_struct *str;

	Return(terme_root_screen_(ptr, &pos));
	terme_arch_size_get(&screen_x, &screen_y);
	str = struct_terme_screen(pos);
	str->window_x = screen_x;
	str->window_y = screen_y;
	str->prompt_x = 0;
	str->prompt_y = 0;
	str->now_x = 0;
	str->now_y = 0;
	str->last_y = 0;

	return 0;
}


/*
 *  prompt
 */
static int terme_screen_write_char_(Execute ptr,
		unicode c, unsigned width, PromptMode mode)
{
	int output_p;
	unsigned next;
	addr pos;
	struct terme_screen_struct *str;

	Return(terme_root_screen_(ptr, &pos));
	str = struct_terme_screen(pos);
	output_p = (str->now_y < str->window_y);

	/* eol */
	next = str->now_x + width;
	if (str->window_x < next) {
		if (output_p) {
			Return(terme_write_terpri_(ptr));
			Return(terme_write_delete_line_right_(ptr));
		}
		str->now_x = 0;
		str->now_y++;
	}

	/* output */
	if (output_p) {
		Return(terme_write_char_(ptr, c, width, mode));
	}
	str->now_x++;

	/* width */
	if (1 < width) {
		if (output_p) {
			Return(terme_write_right_(ptr, mode));
		}
		str->now_x++;
	}

	return 0;
}

static int terme_screen_prompt_string_(Execute ptr, addr pos, PromptMode mode)
{
	unsigned width;
	unicode c;
	size_t size, i;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		width = eastasian_width(c);
		Return(terme_screen_write_char_(ptr, c, width, mode));
	}

	return 0;
}

static PrintColor terme_prompt_color_bright(PromptMode mode)
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

static PrintColor terme_prompt_color_dark(PromptMode mode)
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

static PrintColor terme_prompt_color(Execute ptr, PromptMode mode)
{
	addr pos;

	GetConst(SYSTEM_PROMPT_BRIGHT, &pos);
	getspecial_local(ptr, pos, &pos);
	if (pos == Nil)
		return terme_prompt_color_dark(mode);

	/* unbound or (not nil) */
	return terme_prompt_color_bright(mode);
}

static int terme_screen_prompt_output_(Execute ptr)
{
	PromptMode mode;
	PrintColor color;
	addr pos;

	/* prompt */
	Return(terme_prompt_get_(ptr, &pos, &mode));
	if (pos == Nil)
		return 0;

	/* color */
	if (terme_font(ptr, print_font_reset))
		return terme_fmte_("terme_font error.", NULL);
	color = terme_prompt_color(ptr, mode);
	if (terme_text_color(ptr, color))
		return terme_fmte_("terme_text_color error.", NULL);

	/* output */
	Return(terme_screen_prompt_string_(ptr, pos, mode));

	/* reset */
	if (terme_font(ptr, print_font_reset))
		return terme_fmte_("terme_font error.", NULL);

	return 0;
}

static int terme_screen_prompt_position_(Execute ptr)
{
	addr pos;
	struct terme_screen_struct *str;

	Return(terme_root_screen_(ptr, &pos));
	str = struct_terme_screen(pos);
	str->prompt_x = str->now_x;
	str->prompt_y = str->now_y;

	return 0;
}

int terme_screen_prompt_(Execute ptr)
{
	addr pos;

	/* fresh-line */
	Return(terminal_io_stream_(ptr, &pos));
	Return(fresh_line_stream_(pos, NULL));

	/* screen */
	Return(terme_screen_prompt_output_(ptr));
	Return(terme_screen_prompt_position_(ptr));
	return terme_write_flush_();
}


/*
 *  push
 */
static int terme_screen_push_next_(addr data, addr screen, unsigned *rx, unsigned *ry)
{
	unsigned now, width, next, x, y;
	struct terme_screen_struct *str;

	/* width */
	terme_data_get_value(data, &now, NULL);
	Return(terme_data_get_character_(data, now, NULL, &width));

	/* screen */
	str = struct_terme_screen(screen);
	x = str->now_x;
	y = str->now_y;
	next = x + width;
	if (str->window_x < next) {
		x = 0;
		y++;
	}
	x += width;

	/* result */
	*rx = x;
	*ry = y;
	return 0;
}

static int terme_screen_push_first_(Execute ptr, addr screen)
{
	struct terme_screen_struct *str;

	str = struct_terme_screen(screen);
	if (str->now_x < str->window_x) {
		Return(terme_write_delete_line_right_(ptr));
	}

	return 0;
}

static int terme_screen_output_(Execute ptr, addr data, addr screen, PromptMode mode)
{
	unsigned now, size, width;
	unicode c;
	struct terme_screen_struct *str;

	str = struct_terme_screen(screen);
	terme_data_get_value(data, &now, &size);
	for (; now < size; now++) {
		Return(terme_data_get_character_(data, now, &c, &width));
		Return(terme_screen_write_char_(ptr, c, width, mode));
	}
	if (str->last_y < str->now_y)
		str->last_y = str->now_y;

	return 0;
}

static int terme_screen_move_(Execute ptr, unsigned x, unsigned y)
{
	addr screen;
	unsigned now_x, now_y;
	struct terme_screen_struct *str;

	Return(terme_root_screen_(ptr, &screen));
	str = struct_terme_screen(screen);
	now_x = str->now_x;
	now_y = str->now_y;

	/* up */
	if (y < now_y) {
		Return(terme_write_up_(ptr, now_y - y));
	}

	/* down */
	if (y > now_y) {
		Return(terme_write_down_(ptr, y - now_y));
	}

	/* left */
	if (x < now_x) {
		Return(terme_write_left_(ptr, now_x - x));
	}

	/* right */
	if (x > now_x) {
		Return(terme_write_right_(ptr, x - now_x));
	}

	/* result */
	str->now_x = x;
	str->now_y = y;
	return 0;
}

int terme_screen_push_(Execute ptr)
{
	unsigned x, y;
	addr data, screen;

	Return(terme_root_data_(ptr, &data));
	Return(terme_root_screen_(ptr, &screen));

	/* next position */
	x = y = 0;
	Return(terme_screen_push_next_(data, screen, &x, &y));
	/* line delete */
	Return(terme_screen_push_first_(ptr, screen));
	/* output */
	Return(terme_screen_output_(ptr, data, screen, prompt_input));
	/* move cursor */
	Return(terme_screen_move_(ptr, x, y));

	return terme_write_flush_();
}


/*
 *  history
 */
static int terme_screen_history_delete_(Execute ptr, addr screen)
{
	unsigned y, now_x, now_y;
	struct terme_screen_struct *str;

	str = struct_terme_screen(screen);
	if (str->last_y <= str->now_y)
		return 0;
	now_x = str->now_x;
	now_y = str->now_y;
	str->now_x = 0;
	for (y = str->now_y; y < str->last_y; y++) {
		Return(terme_write_first_down_(ptr, 1));
		Return(terme_write_delete_line_(ptr));
		str->now_y++;
	}

	return terme_screen_move_(ptr, now_x, now_y);
}

int terme_screen_history_(Execute ptr)
{
	addr data, screen;

	Return(terme_root_data_(ptr, &data));
	Return(terme_root_screen_(ptr, &screen));
	/* move (0, 0) */
	Return(terme_screen_move_(ptr, 0, 0));
	/* output prompt */
	Return(terme_write_delete_line_(ptr));
	Return(terme_screen_prompt_output_(ptr));
	/* output data */
	Return(terme_screen_output_(ptr, data, screen, prompt_input));
	/* delete last line */
	Return(terme_screen_history_delete_(ptr, screen));
	/* flush */
	return terme_write_flush_();
}


/*
 *  left, right
 */
static int terme_screen_left_line_(Execute ptr, unsigned width)
{
	addr screen;
	struct terme_screen_struct *str;

	Return(terme_root_screen_(ptr, &screen));
	str = struct_terme_screen(screen);
	Check(str->now_x < width, "width error.");
	Return(terme_write_left_(ptr, width));
	str->now_x -= width;

	return 0;
}

static int terme_screen_left_previous_(Execute ptr, unsigned width)
{
	unsigned move;
	addr screen;
	struct terme_screen_struct *str;

	Return(terme_root_screen_(ptr, &screen));
	str = struct_terme_screen(screen);
	if (str->now_y == 0)
		return 0;
	/* first up */
	Return(terme_write_first_up_(ptr, 1));
	str->now_x = 0;
	str->now_y--;
	/* right */
	Return(terme_display_getlast_(ptr, &move));
	if (width < move) {
		move -= width;
		Return(terme_write_right_(ptr, move));
		str->now_x = move;
	}
	/* left */
	return terme_write_flush_();
}

static int terme_screen_left_output_(Execute ptr, unsigned width, int *ret)
{
	int check;

	Return(terme_display_previous_(ptr, &check));
	if (check < 0)
		return Result(ret, 0);
	if (check) {
		Return(terme_screen_left_line_(ptr, width));
	}
	else {
		Return(terme_screen_left_previous_(ptr, width));
	}

	return Result(ret, 1);
}

int terme_screen_left_(Execute ptr, unsigned width)
{
	int check;

	Return(terme_screen_left_output_(ptr, width, &check));
	if (! check)
		return 0;

	return terme_write_flush_();
}

static int terme_screen_right_next_(Execute ptr)
{
	unsigned now, size, width, next;
	addr data, screen;
	struct terme_screen_struct *str;

	/* data */
	Return(terme_root_data_(ptr, &data));
	terme_data_get_value(data, &now, &size);
	if (size <= now)
		return terme_write_flush_();
	Return(terme_data_get_character_(data, now, NULL, &width));

	Return(terme_root_screen_(ptr, &screen));
	str = struct_terme_screen(screen);
	next = str->now_x + width;
	if (str->window_x < next) {
		Return(terme_write_first_down_(ptr, 1));
		str->now_x = 0;
		str->now_y++;
	}

	return terme_write_flush_();
}

int terme_screen_right_(Execute ptr, unsigned width)
{
	unsigned next;
	addr screen;
	struct terme_screen_struct *str;

	Return(terme_root_screen_(ptr, &screen));
	str = struct_terme_screen(screen);
	next = str->now_x + width;
	if (str->window_x <= next) {
		Return(terme_write_first_down_(ptr, 1));
		str->now_x = 0;
		str->now_y++;
	}
	else {
		Return(terme_write_right_(ptr, width));
		str->now_x += width;
	}

	return terme_screen_right_next_(ptr);
}

static int terme_screen_delete_last_(Execute ptr, addr screen)
{
	unsigned y, now_y, last_y;
	struct terme_screen_struct *str;

	str = struct_terme_screen(screen);
	now_y = str->now_y;
	last_y = str->last_y;
	for (y = now_y; y < last_y; y++) {
		Return(terme_write_first_down_(ptr, 1));
		str->now_x = 0;
		str->now_y++;
		Return(terme_write_delete_line_right_(ptr));
	}

	return 0;
}

int terme_screen_delete_(Execute ptr)
{
	unsigned now_x, now_y;
	addr data, screen;
	struct terme_screen_struct *str;

	Return(terme_root_data_(ptr, &data));
	Return(terme_root_screen_(ptr, &screen));
	str = struct_terme_screen(screen);
	now_x = str->now_x;
	now_y = str->now_y;

	/* line delete */
	Return(terme_screen_push_first_(ptr, screen));
	/* output */
	Return(terme_screen_output_(ptr, data, screen, prompt_input));
	/* delete last line */
	Return(terme_screen_delete_last_(ptr, screen));
	/* move */
	Return(terme_screen_move_(ptr, now_x, now_y));
	/* flush */
	return terme_write_flush_();
}

int terme_screen_backspace_(Execute ptr, unsigned width)
{
	int check;

	/* left */
	Return(terme_screen_left_output_(ptr, width, &check));
	if (! check)
		return 0;

	/* delete */
	return terme_screen_delete_(ptr);
}

int terme_screen_first_(Execute ptr)
{
	addr screen;
	struct terme_screen_struct *str;

	Return(terme_root_screen_(ptr, &screen));
	str = struct_terme_screen(screen);
	Return(terme_screen_move_(ptr, str->prompt_x, str->prompt_y));
	return terme_write_flush_();
}

static int terme_screen_move_char_(addr screen,
		unsigned width, unsigned *rx, unsigned *ry)
{
	unsigned next;
	struct terme_screen_struct *str;

	str = struct_terme_screen(screen);
	if (str->window_y <= *ry)
		return 0;

	/* eol */
	next = *rx + width;
	if (str->window_x < next) {
		*rx = 0;
		(*ry)++;
	}

	/* move */
	*rx += width;

	return 0;
}

static int terme_screen_last_position_(Execute ptr, unsigned *rx, unsigned *ry)
{
	unsigned now, size, width, x, y;
	addr screen, data;
	struct terme_screen_struct *str;

	Return(terme_root_screen_(ptr, &screen));
	Return(terme_root_data_(ptr, &data));
	str = struct_terme_screen(screen);
	terme_data_get_value(data, &now, &size);
	x = str->now_x;
	y = str->now_y;
	for (; now < size; now++) {
		Return(terme_data_get_character_(data, now, NULL, &width));
		Return(terme_screen_move_char_(screen, width, &x, &y));
	}
	*rx = x;
	*ry = y;

	return 0;
}

int terme_screen_last_(Execute ptr)
{
	unsigned x, y;

	x = y = 0;
	Return(terme_screen_last_position_(ptr, &x, &y));
	Return(terme_screen_move_(ptr, x, y));
	return terme_write_flush_();
}

static int terme_screen_update_output_(Execute ptr, addr screen, PromptMode mode)
{
	unsigned now, size, width;
	unicode c;
	addr data;
	struct terme_screen_struct *str;

	Return(terme_root_data_(ptr, &data));
	str = struct_terme_screen(screen);
	terme_data_get_value(data, NULL, &size);
	for (now = 0; now < size; now++) {
		Return(terme_data_get_character_(data, now, &c, &width));
		Return(terme_screen_write_char_(ptr, c, width, mode));
	}
	if (str->last_y < str->now_y)
		str->last_y = str->now_y;

	return 0;
}

int terme_screen_update_(Execute ptr)
{
	unsigned x, y;
	addr screen;
	struct terme_screen_struct *str;

	Return(terme_root_screen_(ptr, &screen));
	str = struct_terme_screen(screen);
	x = str->now_x;
	y = str->now_y;
	str->now_x = 0;
	str->now_y = 0;
	Return(terme_write_delete_page_(ptr));
	Return(terme_screen_prompt_output_(ptr));
	Return(terme_screen_update_output_(ptr, screen, prompt_input));
	Return(terme_screen_move_(ptr, x, y));
	return terme_write_flush_();
}

int terme_screen_refresh_(Execute ptr)
{
	unsigned x, y;
	addr screen;
	struct terme_screen_struct *str;

	Return(terme_root_screen_(ptr, &screen));
	str = struct_terme_screen(screen);
	x = str->now_x;
	y = str->now_y;
	str->now_x = 0;
	str->now_y = 0;
	Return(terme_write_delete_page_(ptr));
	Return(terme_display_restore_(ptr, &x, &y));
	Return(terme_screen_move_(ptr, x, y));
	return terme_write_flush_();
}

int terme_screen_rmleft_(Execute ptr)
{
	addr data, screen;

	Return(terme_root_data_(ptr, &data));
	Return(terme_root_screen_(ptr, &screen));
	/* move (0, 0) */
	Return(terme_screen_move_(ptr, 0, 0));
	/* output prompt */
	Return(terme_write_delete_line_(ptr));
	Return(terme_screen_prompt_output_(ptr));
	/* output data */
	Return(terme_screen_output_(ptr, data, screen, prompt_input));
	/* delete last line */
	Return(terme_screen_history_delete_(ptr, screen));
	/* flush */
	return terme_screen_first_(ptr);
}

int terme_screen_rmright_(Execute ptr)
{
	unsigned x, y;
	addr screen;
	struct terme_screen_struct *str;

	Return(terme_root_screen_(ptr, &screen));
	str = struct_terme_screen(screen);
	x = str->now_x;
	y = str->now_y;
	Return(terme_write_delete_line_right_(ptr));
	Return(terme_screen_history_delete_(ptr, screen));
	Return(terme_screen_move_(ptr, x, y));
	return terme_write_flush_();
}

