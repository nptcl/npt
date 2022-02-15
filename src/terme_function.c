#include "bignum.h"
#include "character.h"
#include "condition.h"
#include "cons.h"
#include "define.h"
#include "eastasian_unicode.h"
#include "integer.h"
#include "paper.h"
#include "real.h"
#include "strtype.h"
#include "terme_arch.h"
#include "terme_escape.h"
#include "terme_function.h"
#include "terme_input.h"
#include "terme_output.h"
#include "typedef.h"

#if defined(LISP_TERME_UNIX)
#include <sys/ioctl.h>
#include <sys/select.h>
#include <sys/types.h>
#include <termios.h>
#include <unistd.h>
#elif defined(LISP_TERME_WINDOWS)
#include <windows.h>
#endif

/*
 *  enable
 */
#if defined(LISP_TERME_UNIX)
int terme_call_enable_p(void)
{
	return 1;
}
#else
int terme_call_enable_p(void)
{
	return 0;
}
#endif


/*
 *  input
 */
#if defined(LISP_TERME_UNIX)
int terme_call_input_(addr args, addr *rtype, addr *rvalue)
{
	/* (terme 'terme-input &optional (blocking t)) */
	int int_value;
	double_float float_value;
	addr pos;

	if (args == Nil) {
		terme_input_infinite(rtype, rvalue);
		return 0;
	}
	Return_getcons(args, &pos, &args);
	if (args != Nil) {
		*rtype = *rvalue = Nil;
		return fmte_("Invalid arguments, ~S.", args, NULL);
	}
	if (pos == T) {
		terme_input_infinite(rtype, rvalue);
		return 0;
	}
	if (pos == Nil) {
		terme_input_integer(0, rtype, rvalue);
		return 0;
	}
	if (integerp(pos)) {
		Return(getint_unsigned_(pos, &int_value));
		terme_input_integer(int_value, rtype, rvalue);
		return 0;
	}
	if (floatp(pos)) {
		Return(cast_double_float_unsafe_(pos, &float_value));
		terme_input_float((double)float_value, rtype, rvalue);
		return 0;
	}

	*rtype = *rvalue = Nil;
	return fmte_("Invalid blocking type, ~S.", pos, NULL);
}
#else
int terme_call_input_(addr args, addr *rtype, addr *rvalue)
{
	*rtype = *rvalue = Nil;
	return fmte_("TERME is not enabled.", NULL);
}
#endif


/*
 *  output
 */
#if defined(LISP_TERME_UNIX)
static int terme_call_output_character_(unicode c)
{
	if (terme_output_char(c))
		return fmte_("terme_output_char error.", NULL);

	return 0;
}

static int terme_call_output_string_(addr x)
{
	unicode c;
	size_t size, i;

	string_length(x, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(x, i, &c));
		Return(terme_call_output_character_(c));
	}

	return 0;
}

int terme_call_output_(addr args)
{
	/* (terme 'terme-output &optional x) */
	addr x;
	unicode c;
	fixnum intvalue;

	/* &optional */
	if (args == Nil) {
		x = Nil;
	}
	else {
		Return_getcons(args, &x, &args);
		if (args != Nil)
			return fmte_("Invalid arguments, ~S.", args, NULL);
	}

	/* flush */
	if (x == Nil) {
		if (terme_finish_output())
			return fmte_("terme_finish_output error.", NULL);
		return 0;
	}

	/* output */
	if (characterp(x)) {
		GetCharacter(x, &c);
		return terme_call_output_character_(c);
	}
	if (stringp(x)) {
		return terme_call_output_string_(x);
	}
	if (integerp(x)) {
		Return(getfixnum_unsigned_(x, &intvalue));
		return terme_call_output_character_((unicode)intvalue);
	}

	return fmte_("Invalid output value, ~S.", x, NULL);
}
#else
int terme_call_output_(addr args)
{
	return fmte_("TERME is not enabled.", NULL);
}
#endif


/*
 *  move
 */
#if defined(LISP_TERME_UNIX)
static int terme_call_unsigned_(addr pos, int *ret)
{
	if (pos == Nil)
		return Result(ret, 0);
	else
		return getint_unsigned_(pos, ret);
}

static int terme_call_move_absolute_(addr pos_x, addr pos_y)
{
	int x, y, check;

	if (pos_x == Nil)
		return fmte_("Invalid x-position, ~S.", pos_x, NULL);
	Return(terme_call_unsigned_(pos_x, &x));
	if (pos_y == Nil) {
		check = terme_cursor_move_x(x);
	}
	else {
		Return(terme_call_unsigned_(pos_y, &y));
		check = terme_cursor_move(x, y);
	}
	if (check)
		return fmte_("terme_cursor error.", NULL);

	return 0;
}

static int terme_call_signed_(addr pos, int *ret)
{
	if (pos == Nil)
		return Result(ret, 0);
	else
		return getint_signed_(pos, ret);
}

static int terme_call_move_relative_(addr pos_x, addr pos_y)
{
	int check, x, y;

	Return(terme_call_signed_(pos_x, &x));
	Return(terme_call_signed_(pos_y, &y));

	/* x */
	check = 0;
	if (x) {
		if (x < 0)
			check = terme_cursor_left(-x);
		if (x > 0)
			check = terme_cursor_right(x);
	}
	if (check)
		return fmte_("terme_cursor x error.", NULL);

	/* y */
	check = 0;
	if (y) {
		if (x < 0)
			check = terme_cursor_up(-x);
		if (x > 0)
			check = terme_cursor_down(x);
	}
	if (check)
		return fmte_("terme_cursor y error.", NULL);

	return 0;
}

int terme_call_move_(addr args)
{
	addr x, y, pos, check;

	/* x, y */
	Return_getcons(args, &x, &args);
	Return_getcons(args, &y, &args);
	Return_getcar(args, &pos);
	/* relative */
	GetConst(KEYWORD_RELATIVE, &check);
	if (pos == check)
		return terme_call_move_relative_(x, y);
	/* absolute */
	GetConst(KEYWORD_ABSOLUTE, &check);
	if (pos == check)
		return terme_call_move_absolute_(x, y);

	/* error */
	return fmte_("Value ~S must be a (member :relative :absolute).", pos, NULL);
}
#else
int terme_call_move_(addr args)
{
	return fmte_("TERME is not enabled.", NULL);
}
#endif


/*
 *  clear
 */
#if defined(LISP_TERME_UNIX)
static int terme_call_clear_all_(void)
{
	if (terme_cursor_delete_page())
		return fmte_("terme_cursor_delete_page error.", NULL);
	return 0;
}

static int terme_call_clear_before_(void)
{
	if (terme_cursor_delete_page_left())
		return fmte_("terme_cursor_delete_page_left error.", NULL);
	return 0;
}

static int terme_call_clear_after_(void)
{
	if (terme_cursor_delete_page_right())
		return fmte_("terme_cursor_delete_page_right error.", NULL);
	return 0;
}

int terme_call_clear_(addr args)
{
	/* (terme 'terme-clear &optional x)
	 *   x  (member :before :after nil)
	 */
	addr pos, check;

	/* all */
	if (args == Nil)
		return terme_call_clear_all_();
	Return_getcons(args, &pos, &args);
	if (args != Nil)
		return fmte_("Invalid arguments, ~S.", args, NULL);
	if (pos == Nil)
		return terme_call_clear_all_();

	/* :before */
	GetConst(KEYWORD_BEFORE, &check);
	if (pos == check)
		return terme_call_clear_before_();

	/* :after */
	GetConst(KEYWORD_AFTER, &check);
	if (pos == check)
		return terme_call_clear_after_();

	return fmte_("Invalid operator, ~S.", pos, NULL);
}
#else
int terme_call_clear_(addr args)
{
	return fmte_("TERME is not enabled.", NULL);
}
#endif


/*
 *  delete
 */
#if defined(LISP_TERME_UNIX)
static int terme_call_delete_all_(void)
{
	if (terme_cursor_delete_line())
		return fmte_("terme_cursor_delete_line error.", NULL);
	return 0;
}

static int terme_call_delete_before_(void)
{
	if (terme_cursor_delete_line_left())
		return fmte_("terme_cursor_delete_line_left error.", NULL);
	return 0;
}

static int terme_call_delete_after_(void)
{
	if (terme_cursor_delete_line_right())
		return fmte_("terme_cursor_delete_line_right error.", NULL);
	return 0;
}

int terme_call_delete_(addr args)
{
	/* (terme 'terme-delete &optional x)
	 *   x  (member :before :after nil)
	 */
	addr pos, check;

	/* all */
	if (args == Nil)
		return terme_call_delete_all_();
	Return_getcar(args, &pos);
	if (pos == Nil)
		return terme_call_delete_all_();

	/* :before */
	GetConst(KEYWORD_BEFORE, &check);
	if (pos == check)
		return terme_call_delete_before_();

	/* :after */
	GetConst(KEYWORD_AFTER, &check);
	if (pos == check)
		return terme_call_delete_after_();

	return fmte_("Invalid operator, ~S.", pos, NULL);
}
#else
int terme_call_delete_(addr args)
{
	return fmte_("TERME is not enabled.", NULL);
}
#endif


/*
 *  font
 *    (terme 'terme-font nil)
 *    (terme 'terme-font 'code 3)
 *    (terme 'terme-font 'code 'italic)
 *    (terme 'terme-font 'fore 'red)
 *    (terme 'terme-font 'back 'red)
 *    (terme 'terme-font 'code 'italic 'fore 'red 'back 'black)
 *    (terme 'terme-font 'palfore 10 'palback 20)
 *    (terme 'terme-font 'rgbfore 30 40 50'rgbback 60 70 80)
 */
#if defined(LISP_TERME_UNIX)
int terme_call_font_(Execute ptr, addr args)
{
	Return(terme_font_parser_(args));
	Return(terme_font_update_(ptr, args));
	return 0;
}
#else
int terme_call_font_(Execute ptr, addr args)
{
	return fmte_("TERME is not enabled.", NULL);
}
#endif


/*
 *  size
 */
#if defined(LISP_TERME_UNIX)
int terme_call_size_(addr *rx, addr *ry)
{
	unsigned x, y;

	if (terme_arch_size_update())
		return fmte_("terme_arch_size_update error.", NULL);
	terme_arch_size_get(&x, &y);
	fixnum_heap(rx, (fixnum)x);
	fixnum_heap(ry, (fixnum)y);

	return 0;
}
#else
int terme_call_size_(addr *rx, addr *ry)
{
	*rx = *ry = Nil;
	return fmte_("TERME is not enabled.", NULL);
}
#endif


/*
 *  scroll
 */
#if defined(LISP_TERME_UNIX)
static int terme_call_scroll_up_(int value)
{
	if (terme_cursor_scroll_up(value))
		return fmte_("terme_cursor_scroll_up error.", NULL);
	return 0;
}

static int terme_call_scroll_down_(int value)
{
	if (terme_cursor_scroll_down(value))
		return fmte_("terme_cursor_scroll_down error.", NULL);
	return 0;
}

int terme_call_scroll_(addr args)
{
	int value;
	addr pos;

	Return_getcons(args, &pos, &args);
	if (args != Nil)
		return fmte_("Invalid arguments, ~S.", args, NULL);
	Return(getint_signed_(pos, &value));
	if (value < 0)
		return terme_call_scroll_up_(-value);
	if (value > 0)
		return terme_call_scroll_down_(value);

	return 0;
}
#else
int terme_call_scroll_(addr args)
{
	return fmte_("TERME is not enabled.", NULL);
}
#endif


/*
 *  begin
 */
#if defined(LISP_TERME_UNIX)
int terme_call_begin_(addr *ret)
{
	addr pos;
	size_t size;
	struct termios v;

	/* flush */
	if (terme_finish_output()) {
		*ret = Nil;
		return fmte_("terme_finish_output error.", NULL);
	}

	/* backup */
	if (tcgetattr(STDIN_FILENO, &v)) {
		*ret = Nil;
		return fmte_("tcgetattr error.", NULL);
	}
	paper_body_heap(&pos, sizeoft(v));
	paper_set_memory(pos, 0, sizeoft(v), (const void *)&v, &size);
	Check(size != sizeoft(v), "size error");

	/* set terminal */
	v.c_iflag &= ~(PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON);
	v.c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
	v.c_oflag &= ~OPOST;
	v.c_cflag &= ~(CSIZE | PARENB);
	v.c_cflag |= CS8;
	v.c_cc[VMIN] = 1;
	v.c_cc[VTIME] = 0;
	if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &v)) {
		*ret = Nil;
		return fmte_("tcsetattr error.", NULL);
	}

	return Result(ret, pos);
}
#else
int terme_call_begin_(addr *ret)
{
	*ret = Nil;
	return fmte_("TERME is not enabled.", NULL);
}
#endif


/*
 *  end
 */
#if defined(LISP_TERME_UNIX)
int terme_call_end_(addr pos)
{
	addr type;
	size_t size;
	struct termios v;

	/* argument */
	if (! paperp(pos)) {
		GetConst(SYSTEM_PAPER, &type);
		return call_type_error_va_(NULL, pos, type,
				"Object ~S must be a PAPER type.", pos, NULL);
	}

	/* flush */
	if (terme_finish_output())
		return fmte_("terme_finish_output error.", NULL);

	/* rollback */
	paper_get_memory(pos, 0, sizeoft(v), (void *)&v, &size);
	Check(size != sizeoft(v), "size error");
	if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &v))
		return fmte_("tcsetattr error.", NULL);

	return 0;
}
#else
int terme_call_end_(addr pos)
{
	return fmte_("TERME is not enabled.", NULL);
}
#endif

