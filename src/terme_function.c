#include "array_access.h"
#include "array_inplace.h"
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
 *  init
 */
#if defined(LISP_TERME_UNIX)
static struct termios terme_call_default_v;

int terme_call_init(void)
{
	if (tcgetattr(STDIN_FILENO, &terme_call_default_v)) {
		Debug("tcgetattr error.");
		return 1;
	}

	return 0;
}
#else
int terme_call_init(void)
{
	return 0;
}
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
	addr pos;

	if (! isBaseType(c)) {
		fixnum_heap(&pos, (fixnum)c);
		return fmte_("Invalid code, ~S.", NULL);
	}
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

static int terme_call_output_object_(addr x)
{
	unicode c;
	fixnum v;

	switch (GetType(x)) {
		case LISPTYPE_CHARACTER:
			GetCharacter(x, &c);
			return terme_call_output_character_(c);

		case LISPTYPE_FIXNUM:
			GetFixnum(x, &v);
			if (v < 0)
				return fmte_("Invalid value, ~S.", x, NULL);
			return terme_call_output_character_((unicode)v);

		default:
			return fmte_("Invalid value, ~S.", x, NULL);
	}
}

static int terme_call_output_array_t_(addr x, size_t front)
{
	addr value;
	size_t i;

	for (i = 0; i < front; i++) {
		Return(array_get_t_(x, i, &value));
		Return(terme_call_output_object_(value));
	}

	return 0;
}

static int terme_call_output_array_character_(addr x, size_t front)
{
	unicode c;
	size_t i;

	for (i = 0; i < front; i++) {
		Return(array_get_unicode_(x, i, &c));
		Return(terme_call_output_character_(c));
	}

	return 0;
}

static int terme_call_output_array_signed_(const struct array_value *str, int *ret)
{
	int8_t i8;
	int16_t i16;
	int32_t i32;
#ifdef LISP_64BIT
	int64_t i64;
#endif

	switch (str->size) {
		case 8:
			i8 = str->value.signed8;
			if (i8 < 0)
				goto error;
			Return(terme_call_output_character_((unicode)i8));
			break;

		case 16:
			i16 = str->value.signed16;
			if (i16 < 0)
				goto error;
			Return(terme_call_output_character_((unicode)i16));
			break;

		case 32:
			i32 = str->value.signed32;
			if (i32 < 0 || isBaseType(i32))
				goto error;
			Return(terme_call_output_character_((unicode)i32));
			break;

#ifdef LISP_64BIT
		case 64:
			i64 = str->value.signed64;
			if (i64 < 0 || isBaseType(i64))
				goto error;
			Return(terme_call_output_character_((unicode)i64));
			break;
#endif
		default:
			goto error;
	}
	return Result(ret, 0);

error:
	return Result(ret, 1);
}

static int terme_call_output_array_unsigned_(const struct array_value *str, int *ret)
{
	uint8_t u8;
	uint16_t u16;
	uint32_t u32;
#ifdef LISP_64BIT
	uint64_t u64;
#endif

	switch (str->size) {
		case 8:
			u8 = str->value.unsigned8;
			Return(terme_call_output_character_((unicode)u8));
			break;

		case 16:
			u16 = str->value.unsigned16;
			Return(terme_call_output_character_((unicode)u16));
			break;

		case 32:
			u32 = str->value.unsigned32;
			if (isBaseType(u32))
				goto error;
			Return(terme_call_output_character_((unicode)u32));
			break;

#ifdef LISP_64BIT
		case 64:
			u64 = str->value.unsigned64;
			if (isBaseType(u64))
				goto error;
			Return(terme_call_output_character_((unicode)u64));
			break;
#endif
		default:
			goto error;
	}
	return Result(ret, 0);

error:
	return Result(ret, 1);
}

static int terme_call_output_array_value_(addr x, size_t front)
{
	int check;
	size_t i;
	struct array_value str;

	for (i = 0; i < front; i++) {
		Return(arrayinplace_get_(x, i, &str));
		check = 1;
		switch (str.type) {
			case ARRAY_TYPE_SIGNED:
				Return(terme_call_output_array_signed_(&str, &check));
				break;

			case ARRAY_TYPE_UNSIGNED:
				Return(terme_call_output_array_unsigned_(&str, &check));
				break;

			default:
				break;
		}
		if (check) {
			Return(array_get_(NULL, x, i, &x));
			return fmte_("Invalid value, ~S.", x, NULL);
		}
	}

	return 0;
}

static int terme_call_output_array_(addr x)
{
	struct array_struct *str;
	size_t front;

	str = ArrayInfoStruct(x);
	if (str->dimension != 1)
		return fmte_("Array ~S dimension must be a 1.", x, NULL);

	front = str->front;
	switch (str->type) {
		case ARRAY_TYPE_T:
			return terme_call_output_array_t_(x, front);

		case ARRAY_TYPE_CHARACTER:
			return terme_call_output_array_character_(x, front);

		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			return terme_call_output_array_value_(x, front);

		default:
			return fmte_("Invalid array type, ~S.", x, NULL);
	}
}

static int terme_call_output_vector_(addr x)
{
	addr value;
	size_t i, size;

	lenarray(x, &size);
	for (i = 0; i < size; i++) {
		getarray(x, i, &value);
		Return(terme_call_output_object_(value));
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
	if (arrayp(x)) {
		return terme_call_output_array_(x);
	}
	if (GetType(x) == LISPTYPE_VECTOR) {
		return terme_call_output_vector_(x);
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
 *    (terme 'terme-font 'rgbfore 30 40 50 'rgbback 60 70 80)
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
static int terme_call_begin_update_(addr *ret, void (*call)(struct termios *))
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
	(*call)(&v);
	if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &v)) {
		*ret = Nil;
		return fmte_("tcsetattr error.", NULL);
	}

	return Result(ret, pos);
}

static void terme_call_begin_raw_call(struct termios *ptr)
{
	ptr->c_iflag &= ~(PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON);
	ptr->c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
	ptr->c_oflag &= ~OPOST;
	ptr->c_cflag &= ~(CSIZE | PARENB);
	ptr->c_cflag |= CS8;
	ptr->c_cc[VMIN] = 1;
	ptr->c_cc[VTIME] = 0;
}

static int terme_call_begin_raw_(addr *ret)
{
	return terme_call_begin_update_(ret, terme_call_begin_raw_call);
}

static void terme_call_begin_default_call(struct termios *ptr)
{
	*ptr = terme_call_default_v;
}

static int terme_call_begin_default_(addr *ret)
{
	return terme_call_begin_update_(ret, terme_call_begin_default_call);
}

int terme_call_begin_(addr args, addr *ret)
{
	addr pos, check;

	if (args == Nil)
		goto raw_mode;
	Return_getcons(args, &pos, &args);
	if (args != Nil)
		goto error;
	if (pos == Nil)
		goto raw_mode;
	GetConst(KEYWORD_DEFAULT, &check);
	if (pos == check)
		goto default_mode;
error:
	*ret = Nil;
	return fmte_("Invalid arguments, ~S.", args, NULL);

default_mode:
	return terme_call_begin_default_(ret);
raw_mode:
	return terme_call_begin_raw_(ret);
}

#else
int terme_call_begin_(addr args, addr *ret)
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


/*
 *  signal
 */
#if defined(LISP_TERME_UNIX)
static int terme_call_signal_sigint_(void)
{
	if (terme_arch_terminal_sigint_())
		return fmte_("kill error.", NULL);

	return 0;
}

static int terme_call_signal_stop_(void)
{
	if (terme_arch_terminal_stop_())
		return fmte_("kill error.", NULL);

	return 0;
}

int terme_call_signal_(addr args)
{
	int check;
	addr pos;

	/* arguments */
	if (args == Nil)
		return fmte_("Invalid arguments.", NULL);
	Return_getcons(args, &pos, &args);
	if (args != Nil)
		return fmte_("Invalid arguments, ~S.", args, NULL);

	/* sigint */
	Return(string_designer_equalp_char_(pos, "SIGINT", &check));
	if (check)
		return terme_call_signal_sigint_();

	/* stop */
	Return(string_designer_equalp_char_(pos, "STOP", &check));
	if (check)
		return terme_call_signal_stop_();

	/* error */
	return fmte_("Invalid arguments, ~S.", pos, NULL);
}
#else
int terme_call_signal_(addr args)
{
	return fmte_("TERME is not enabled.", NULL);
}
#endif

