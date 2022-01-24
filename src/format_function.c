#include <math.h>
#include "array_vector.h"
#include "bignum.h"
#include "bignum_object.h"
#include "character.h"
#include "character_name.h"
#include "character_queue.h"
#include "code_object.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "copy.h"
#include "eastasian.h"
#include "equal.h"
#include "format.h"
#include "format_float.h"
#include "format_function.h"
#include "format_parse.h"
#include "format_print.h"
#include "format_radix.h"
#include "format_typedef.h"
#include "function.h"
#include "heap.h"
#include "hold.h"
#include "local.h"
#include "integer.h"
#include "integer_common.h"
#include "object.h"
#include "package_intern.h"
#include "pointer.h"
#include "print_pretty.h"
#include "print_write.h"
#include "ratio.h"
#include "stream.h"
#include "stream_common.h"
#include "stream_function.h"
#include "stream_pretty.h"
#include "stream_string.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"

typedef int (*fmtcalltype)(fmtprint, struct format_operator *str);
static fmtcalltype FormatCallTable[FormatType_size];

static struct format_argument *format_getargs(
		struct format_operator *str, size_t index)
{
	Check(str->size <= index, "index error");
	return (struct format_argument *)
		(((byte *)str)
		 + sizeoft(struct format_operator)
		 + sizeoft(struct format_argument) * index);
}


/*
 *  reader
 */
static int fmtint_count_(fmtprint print, fixnum *ret)
{
	size_t value;

	Return(length_list_safe_(print->rest->front, &value));
	Check(value < 0, "cast error");

	return Result(ret, (fixnum)value);
}

static int fmtint_argument_(fmtprint print, struct format_operator *str,
		fixnum *ret, int *check)
{
	addr pos;

	Return(fmtprint_pop_(print, str, &pos));
	if (pos == Nil) {
		*ret = 0;
		*check = 1;
		return 0;
	}
	if (bignump(pos)) {
		*ret = 0;
		*check = 1;
		return fmtprop_abort_(print, str,
				"Too large the format argument ~S.", pos, NULL);
	}
	if (! fixnump(pos)) {
		*ret = 0;
		*check = 1;
		return fmtprop_abort_(print, str,
				"The format argument ~S must be an integer.", pos, NULL);
	}
	GetFixnum(pos, ret);
	*check = 0;
	return 0;
}

static int fmtint_nilp_(fmtprint print, struct format_operator *str,
		unsigned index, fixnum *ret, int *check)
{
	struct format_argument *arg;

	Check(str->args_size <= index, "index error");
	arg = format_getargs(str, index);
	switch (arg->type) {
		case fmtargs_nil:
			*ret = 0;
			*check = 1;
			return 0;

		case fmtargs_integer:
			*ret = arg->u.value;
			*check = 0;
			return 0;

		case fmtargs_count:
			Return(fmtint_count_(print, ret));
			*check = 0;
			return 0;

		case fmtargs_argument:
			return fmtint_argument_(print, str, ret, check);

		default:
			*ret = 0;
			*check = 1;
			return fmtprop_abort_(print, str,
					"The format parameter must be an integer.", NULL);
	}
}

static int fmtchar_argument_(fmtprint print,
		struct format_operator *str, unicode *value, int *ret)
{
	addr pos;

	Return(fmtprint_pop_(print, str, &pos));
	if (pos == Nil)
		return Result(ret, 1);
	if (! characterp(pos)) {
		*value = 0;
		return fmtprop_abort_(print, str,
				"The format argument ~S must be a character.", pos, NULL);
	}
	GetCharacter(pos, value);
	return Result(ret, 0);
}

static int fmtchar_nilp_(fmtprint print,
		struct format_operator *str, unsigned index, unicode *value, int *ret)
{
	struct format_argument *arg;

	Check(str->args_size <= index, "index error");
	arg = format_getargs(str, index);
	switch (arg->type) {
		case fmtargs_nil:
			return Result(ret, 1);

		case fmtargs_character:
			*value = arg->u.character;
			return Result(ret, 0);

		case fmtargs_argument:
			return fmtchar_argument_(print, str, value, ret);

		case fmtargs_count:
		default:
			*value = 0;
			*ret = 0;
			return fmtprop_abort_(print, str,
					"The format argument must be a character.", NULL);
	}
}

static int fmtint_default_(fmtprint print, struct format_operator *str,
		unsigned index, fixnum *ret, fixnum defvar)
{
	int check;

	Return(fmtint_nilp_(print, str, index, ret, &check));
	if (check)
		*ret = defvar;

	return 0;
}

static int fmtchar_default_(fmtprint print, struct format_operator *str,
		unsigned index, unicode *ret, unicode defvar)
{
	int check;

	Return(fmtchar_nilp_(print, str, index, ret, &check));
	if (check)
		*ret = defvar;

	return 0;
}

static int fmtargs_abort_(fmtprint print,
		struct format_operator *fmt, unsigned index, const char *str, ...)
{
	struct format_argument *arg;
	va_list args;

	arg = format_getargs(fmt, index);
	va_start(args, str);
	Return(format_abort_(print->format, arg->position, str, args));
	va_end(args);

	return 0;
}


/*
 *  Error
 */
static int format_call_Error(fmtprint print, struct format_operator *str)
{
	return fmtprop_abort_(print, str,
			"Cannot execute format operator [Error].", NULL);
}


/*
 *  End
 */
static int format_call_End(fmtprint print, struct format_operator *str)
{
	return fmtprop_abort_(print, str,
			"Cannot execute format operator [End].", NULL);
}


/*
 *  Format
 */
static int format_call_Format(fmtprint print, struct format_operator *str)
{
	return fmtprop_abort_(print, str,
			"Cannot execute format operator [Format].", NULL);
}


/*
 *  Output
 */
static int format_call_Output(fmtprint print, struct format_operator *str)
{
	int delete_space;
	struct format_argument *arg;
	unicode u, *body;
	size_t size, i;

	/* argument */
	Check(3 < str->args_size, "size error");
	arg = format_getargs(str, 0);
	Check(arg->type != fmtargs_index, "type error");
	size = arg->u.index;

	/* body */
	delete_space = print->delete_space;
	body = (unicode *)format_getargs(str, 3);
	for (i = 0; i < size; i++) {
		u = body[i];
		if ((! delete_space) || (! isSpaceUnicode(u))) {
			Return(fmtprint_putc_(print, u));
			delete_space = 0;
		}
	}
	print->delete_space = delete_space;

	return 0;
}


/*
 *  Aesthetic
 */
static size_t format_colinc_division(size_t size, size_t colinc)
{
	if (size % colinc)
		size = (size / colinc) + 1;
	else
		size = size / colinc;

	return size * colinc;
}

static int format_write_margin_(fmtprint print, addr string,
		int atsign, fixnum mc, fixnum cl, fixnum mp, unicode padchar)
{
	size_t mincol, colinc, minpad, size, space, s;

	Check(mc < 0, "mincol error");
	Check(cl < 1, "colinc error");
	Check(mp < 0, "minpad error");
	mincol = (size_t)mc;
	colinc = (size_t)cl;
	minpad = (size_t)mp;
	Return(eastasian_length_(string, &size, NULL));

	/* output margin */
	space = minpad;
	s = size + space;
	if (s < mincol)
		space += format_colinc_division(mincol - s, colinc);

	if (atsign) {
		/* insert space in left */
		Return(fmtprint_putc_times_(print, padchar, space));
		Return(fmtprint_string_(print, string));
	}
	else {
		/* insert space in right */
		Return(fmtprint_string_(print, string));
		Return(fmtprint_putc_times_(print, padchar, space));
	}

	return 0;
}

static int format_call_print(fmtprint print, addr pos, int colon, int atsign,
		fixnum mincol, fixnum colinc, fixnum minpad, unicode padchar,
		int prin1p)
{
	int check;
	addr stream;

	if (pos == Nil && colon) {
		strvect_char_local(print->local, &pos, "()");
	}
	else {
		Return(fmtprint_stream_(print, &stream));
		if (prin1p)
			check = prin1_print(print->ptr, stream, pos);
		else
			check = princ_print(print->ptr, stream, pos);
		if (check)
			return 1;
		Return(string_stream_local_(print->local, stream, &pos));
		clear_output_string_stream(stream);
	}

	return format_write_margin_(print, pos, atsign, mincol, colinc, minpad, padchar);
}

static int format_call_Aesthetic(fmtprint print, struct format_operator *str)
{
	addr pos;
	fixnum mincol, colinc, minpad;
	unicode padchar;

	Check(4 < str->args_size, "size error");
	Return(fmtint_default_(print, str, 0, &mincol, 0));
	Return(fmtint_default_(print, str, 1, &colinc, 1));
	Return(fmtint_default_(print, str, 2, &minpad, 0));
	Return(fmtchar_default_(print, str, 3, &padchar, ' '));
	if (mincol < 0) {
		return fmtargs_abort_(print, str, 0,
				"The parameter must be a positive integer.", NULL);
	}
	if (colinc < 1) {
		return fmtargs_abort_(print, str, 1,
				"The parameter must be greater than 1.", NULL);
	}
	if (minpad < 0) {
		return fmtargs_abort_(print, str, 2,
				"The parameter must be a positive integer.", NULL);
	}

	/* output */
	Return(fmtprint_pop_(print, str, &pos));
	return format_call_print(print, pos, str->colon, str->atsign,
			mincol, colinc, minpad, padchar, 0);
}

static int format_call_Standard(fmtprint print, struct format_operator *str)
{
	addr pos;
	fixnum mincol, colinc, minpad;
	unicode padchar;

	Check(4 < str->args_size, "size error");
	Return(fmtint_default_(print, str, 0, &mincol, 0));
	Return(fmtint_default_(print, str, 1, &colinc, 1));
	Return(fmtint_default_(print, str, 2, &minpad, 0));
	Return(fmtchar_default_(print, str, 3, &padchar, ' '));
	if (mincol < 0) {
		return fmtargs_abort_(print, str, 0,
				"The parameter must be a positive integer.", NULL);
	}
	if (colinc < 1) {
		return fmtargs_abort_(print, str, 1,
				"The parameter must be greater than 1.", NULL);
	}
	if (minpad < 0) {
		return fmtargs_abort_(print, str, 2,
				"The parameter must be a positive integer.", NULL);
	}

	/* output */
	Return(fmtprint_pop_(print, str, &pos));
	return format_call_print(print, pos, str->colon, str->atsign,
			mincol, colinc, minpad, padchar, 1);
}


/*
 *  Binary, Octal, Decimal, Hexadecimal
 */
static int format_radix_parameter(fmtprint print, struct format_operator *str,
		unsigned radix, fixnum mincol, unicode padchar, fixnum range, unicode comma)
{
	int minusp;
	addr pos, stream;
	LocalRoot local;
	size_t size;

	local = print->local;
	size = (size_t)range;
	Check(size < 0, "cast error");

	Return(fmtprint_pop_(print, str, &pos));
	if (! integerp(pos))
		return format_call_print(print, pos, 0, 1, mincol, 1, 0, padchar, 0);

	/* integer */
	Return(fmtprint_stream_(print, &stream));
	/* sign */
	Return(minusp_integer_(pos, &minusp));
	if (str->atsign || minusp) {
		Return(write_char_stream_(stream, minusp? '-': '+'));
	}
	/* body */
	if (str->colon) {
		Return(output_nosign_comma_integer_(local, stream, pos, radix, 1, size, comma));
	}
	else {
		Return(output_nosign_integer_(local, stream, pos, radix, 1));
	}
	/* output */
	Return(string_stream_local_(print->local, stream, &pos));
	clear_output_string_stream(stream);

	return format_write_margin_(print, pos, 1, mincol, 1, 0, padchar);
}

static int format_radix_integer_call_(fmtprint print,
		struct format_operator *str, unsigned radix)
{
	Execute ptr;
	fixnum mincol, range;
	unicode padchar, comma;

	ptr = print->ptr;
	/*
	 *  (let ((*print-escape* nil)
	 *        (*print-radix* nil)
	 *        (*print-base* [radix]))
	 *    ...)
	 */
	push_escape_print(ptr, 0);
	push_radix_print(ptr, 0);
	push_base_print(ptr, radix);
	Return(fmtint_default_(print, str, 0, &mincol, 0));
	Return(fmtchar_default_(print, str, 1, &padchar, ' '));
	Return(fmtchar_default_(print, str, 2, &comma, ','));
	Return(fmtint_default_(print, str, 3, &range, 3));
	if (mincol < 0) {
		return fmtargs_abort_(print, str, 0,
				"The parameter must be a positive integer.", NULL);
	}
	if (range < 1) {
		return fmtargs_abort_(print, str, 3,
				"The parameter must be greate than 1.", NULL);
	}

	return format_radix_parameter(print, str, radix, mincol, padchar, range, comma);
}

static int format_radix_integer_(fmtprint print,
		struct format_operator *str, unsigned radix)
{
	Execute ptr;
	addr control;

	Check(4 < str->args_size, "size error");
	ptr = print->ptr;
	push_control(ptr, &control);
	(void)format_radix_integer_call_(print, str, radix);
	return pop_control_(ptr, control);
}

static int format_call_Binary(fmtprint print, struct format_operator *str)
{
	return format_radix_integer_(print, str, 2);
}
static int format_call_Octal(fmtprint print, struct format_operator *str)
{
	return format_radix_integer_(print, str, 8);
}
static int format_call_Decimal(fmtprint print, struct format_operator *str)
{
	return format_radix_integer_(print, str, 10);
}
static int format_call_Hexadecimal(fmtprint print, struct format_operator *str)
{
	return format_radix_integer_(print, str, 16);
}


/*
 *  Radix
 */
static int format_call_Radix_call_(fmtprint print, struct format_operator *str)
{
	Execute ptr;
	fixnum mincol, range;
	unicode padchar, comma;
	unsigned radix;

	ptr = print->ptr;
	/*
	 *  (let ((*print-escape* nil)
	 *        (*print-radix* nil)
	 *        (*print-base* [radix]))
	 *    ...)
	 */
	push_escape_print(ptr, 0);
	push_radix_print(ptr, 0);

	Return(fmtint_default_(print, str, 0, &range, 10));
	if (! isBaseChar(range)) {
		return fmtargs_abort_(print, str, 0,
				"The parameter must be an integer between 2 and 36.", NULL);
	}
	radix = (unsigned)range;
	push_base_print(ptr, radix);

	Return(fmtint_default_(print, str, 1, &mincol, 0));
	Return(fmtchar_default_(print, str, 2, &padchar, ' '));
	Return(fmtchar_default_(print, str, 3, &comma, ','));
	Return(fmtint_default_(print, str, 4, &range, 3));
	if (mincol < 0) {
		return fmtargs_abort_(print, str, 1,
				"The paramter must be a positive integer.", NULL);
	}
	if (range < 2) {
		return fmtargs_abort_(print, str, 4,
				"The parameter must be greater than 1.", NULL);
	}

	return format_radix_parameter(print, str, radix, mincol, padchar, range, comma);
}

static int format_call_Radix(fmtprint print, struct format_operator *str)
{
	Execute ptr;
	addr control;

	Check(5 < str->args_size, "size error");
	ptr = print->ptr;
	push_control(ptr, &control);
	(void)format_call_Radix_call_(print, str);
	return pop_control_(ptr, control);
}


/*
 *  RadixText
 */
static int format_call_RadixText_roma_(fmtprint print,
		struct format_operator *str, addr pos)
{
	addr stream;
	fixnum value;

	if (! fixnump(pos)) {
		return fmtprop_abort_(print, str,
				"~~@R argument ~S must be an integer between 1 and 3999.", pos, NULL);
	}
	GetFixnum(pos, &value);
	if (! (1 <= value && value <= 3999)) {
		return fmtprop_abort_(print, str,
				"~~@R argument ~S must be an integer between 1 and 3999.", pos, NULL);
	}

	Return(fmtprint_stream_(print, &stream));
	Return(roma_integer_(stream, value, str->colon));
	return fmtprint_stream_output_(print);
}

static int format_call_RadixText_english_(fmtprint print,
		struct format_operator *str, addr pos)
{
	addr stream;

	if (! integerp(pos)) {
		return fmtprop_abort_(print, str,
				"~~R argument ~S must be an integer.", pos, NULL);
	}

	Return(fmtprint_stream_(print, &stream));
	Return(english_integer_(print->local, stream, pos, str->colon == 0));
	return fmtprint_stream_output_(print);
}

static int format_call_RadixText_call_(fmtprint print, struct format_operator *str)
{
	Execute ptr;
	addr pos;

	ptr = print->ptr;
	/*
	 *  (let ((*print-escape* nil)
	 *        (*print-radix* nil)
	 *        (*print-base* 10))
	 *    ...)
	 */
	push_escape_print(ptr, 0);
	push_radix_print(ptr, 0);
	push_base_print(ptr, 10);
	/* output */
	Return(fmtprint_pop_(print, str, &pos));
	if (str->atsign)
		return format_call_RadixText_roma_(print, str, pos);
	else
		return format_call_RadixText_english_(print, str, pos);
}

static int format_call_RadixText(fmtprint print, struct format_operator *str)
{
	Execute ptr;
	addr control;

	Check(0 < str->args_size, "size error");
	ptr = print->ptr;
	push_control(ptr, &control);
	(void)format_call_RadixText_call_(print, str);
	return pop_control_(ptr, control);
}


/*
 *  Plural
 */
static int format_call_Plural(fmtprint print, struct format_operator *str)
{
	addr pos;

	Check(0 < str->args_size, "size error");
	/* ~:* */
	if (str->colon) {
		Return(fmtprint_rollback_(print, str, 1));
	}

	/* plural */
	Return(fmtprint_pop_(print, str, &pos));
	if (! str->atsign) {
		if (! eql_function(pos, fixnumh(1))) {
			Return(fmtprint_putc_(print, 's'));
		}
	}
	else {
		if (eql_function(pos, fixnumh(1))) {
			Return(fmtprint_putc_(print, 'y'));
		}
		else {
			Return(fmtprint_putc_(print, 'i'));
			Return(fmtprint_putc_(print, 'e'));
			Return(fmtprint_putc_(print, 's'));
		}
	}

	return 0;
}


/*
 *  Character
 */
static int format_call_Character_atsign_(fmtprint print, addr pos)
{
	addr stream;

	CheckType(pos, LISPTYPE_CHARACTER);
	Return(fmtprint_stream_(print, &stream));
	Return(prin1_print(print->ptr, stream, pos));
	Return(string_stream_local_(print->local, stream, &pos));
	clear_output_string_stream(stream);
	return fmtprint_string_(print, pos);
}

static int format_call_Character(fmtprint print, struct format_operator *str)
{
	addr pos, name;

	Check(0 < str->args_size, "size error");
	Return(fmtprint_pop_(print, str, &pos));
	if (! characterp(pos)) {
		return fmtprop_abort_(print, str,
				"The argument ~S must be a character.", pos, NULL);
	}
	if (! str->colon && str->atsign) {
		/* ~@C */
		return format_call_Character_atsign_(print, pos);
	}
	if (str->colon || str->atsign) {
		/* ~:C or ~:@C */
		Return(findtable_char_name_(&name, pos));
		if (name != Nil) {
			Return(fmtprint_string_(print, name));
		}
		else {
			Return(fmtprint_putc_(print, RefCharacter(pos)));
		}
	}
	else {
		/* ~C */
		Return(fmtprint_putc_(print, RefCharacter(pos)));
	}

	return 0;
}


/*
 *  Fixed
 */
static int fmtfloat_w(fmtprint print,
		struct format_operator *str, fmtfloat ff, unsigned index)
{
	int check;
	fixnum value;

	Return(fmtint_nilp_(print, str, index, &value, &check));
	if (! check && value < 0) {
		return fmtargs_abort_(print, str, index,
				"The parameter must be a positive integer.", NULL);
	}
	ff->wp = ! check;
	ff->w = check? 0: (size_t)value;

	return 0;
}

static int fmtfloat_d(fmtprint print,
		struct format_operator *str, fmtfloat ff, unsigned index)
{
	int check;
	fixnum value;

	Return(fmtint_nilp_(print, str, index, &value, &check));
	if (! check && value < 0) {
		return fmtargs_abort_(print, str, index,
				"The parameter must be a positive integer.", NULL);
	}
	ff->dp = ! check;
	ff->d = check? 0: (size_t)value;

	return 0;
}

static int fmtfloat_e(fmtprint print,
		struct format_operator *str, fmtfloat ff, unsigned index)
{
	fixnum value;

	Return(fmtint_default_(print, str, index, &value, 1));
	if (value < 0) {
		return fmtargs_abort_(print, str, index,
				"The parameter must be a positive integer.", NULL);
	}
	if (value == 0)
		value = 1;
	ff->ep = 1;
	ff->e = (size_t)value;

	return 0;
}

static int fmtfloat_k(fmtprint print,
		struct format_operator *str, fmtfloat ff, unsigned index, fixnum value)
{
	return fmtint_default_(print, str, index, &(ff->k), value);
}

static int fmtfloat_overflowchar_(fmtprint print,
		struct format_operator *str, fmtfloat ff, unsigned index)
{
	int check;
	unicode u;

	Return(fmtchar_nilp_(print, str, index, &u, &check));
	ff->overflowp = ! check;
	ff->overflow = check? 0: u;

	return 0;
}

static int fmtfloat_padchar_(fmtprint print,
		struct format_operator *str, fmtfloat ff, unsigned index)
{
	return fmtchar_default_(print, str, index, &(ff->pad), ' ');
}

static int format_fixed_single_(fmtprint print, fmtfloat ff, addr pos)
{
	addr stream;
	single_float value;
	struct fmtdecimal_struct dec;

	GetSingleFloat(pos, &value);
	ff->u.value_single = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_single_float(&dec, value, FMTFLOAT_ROUND_SINGLE))
		return fmte_("Invalid single-float.", NULL);

	Return(fmtprint_stream_(print, &stream));
	Return(fmtfloat_fixed_(stream, ff, &dec));
	return fmtprint_stream_output_(print);
}

static int format_fixed_double_(fmtprint print, fmtfloat ff, addr pos)
{
	addr stream;
	double_float value;
	struct fmtdecimal_struct dec;

	GetDoubleFloat(pos, &value);
	ff->u.value_double = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_double_float(&dec, value, FMTFLOAT_ROUND_DOUBLE))
		return fmte_("Invalid double-float.", NULL);

	Return(fmtprint_stream_(print, &stream));
	Return(fmtfloat_fixed_(stream, ff, &dec));
	return fmtprint_stream_output_(print);
}

static int format_fixed_long_(fmtprint print, fmtfloat ff, addr pos)
{
	addr stream;
	long_float value;
	struct fmtdecimal_struct dec;

	GetLongFloat(pos, &value);
	ff->u.value_long = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_long_float(&dec, value, FMTFLOAT_ROUND_LONG))
		return fmte_("Invalid long-float.", pos, NULL);

	Return(fmtprint_stream_(print, &stream));
	Return(fmtfloat_fixed_(stream, ff, &dec));
	return fmtprint_stream_output_(print);
}

static int format_fixed_argument_(fmtprint print,
		struct format_operator *str, fmtfloat ff)
{
	clearpoint(ff);
	Return(fmtfloat_w(print, str, ff, 0));
	Return(fmtfloat_d(print, str, ff, 1));
	Return(fmtfloat_k(print, str, ff, 2, 0));
	Return(fmtfloat_overflowchar_(print, str, ff, 3));
	Return(fmtfloat_padchar_(print, str, ff, 4));
	ff->k_bias = 0; /* 0 if fixed */
	ff->sign = str->atsign;

	return 0;
}

static int format_fixed_fixnum_(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	single_float_fixnum_local(print->local, &pos, pos);
	return format_fixed_single_(print, ff, pos);
}

static int format_fixed_bignum_(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	Return(single_float_bignum_local_(print->local, &pos, pos));
	return format_fixed_single_(print, ff, pos);
}

static int format_fixed_ratio_(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	Return(single_float_ratio_local_(print->local, &pos, pos));
	return format_fixed_single_(print, ff, pos);
}

static int format_fixed_float_(fmtprint print,
		struct format_operator *str, fmtfloat ff)
{
	addr pos;

	Return(fmtprint_pop_(print, str, &pos));
	switch (GetType(pos)) {
		case LISPTYPE_DOUBLE_FLOAT:
			return format_fixed_double_(print, ff, pos);

		case LISPTYPE_SINGLE_FLOAT:
			return format_fixed_single_(print, ff, pos);

		case LISPTYPE_LONG_FLOAT:
			return format_fixed_long_(print, ff, pos);

		case LISPTYPE_FIXNUM:
			return format_fixed_fixnum_(print, ff, pos);

		case LISPTYPE_BIGNUM:
			return format_fixed_bignum_(print, ff, pos);

		case LISPTYPE_RATIO:
			return format_fixed_ratio_(print, ff, pos);

		default:
			return fmtprop_abort_(print, str,
					"~~F argument ~S must be a real type.", pos, NULL);
	}
}

static int format_call_Fixed_call_(fmtprint print, struct format_operator *str)
{
	Execute ptr;
	struct fmtfloat_struct ff;

	ptr = print->ptr;
	/* (let ((*print-escape* nil)) ...) */
	push_escape_print(ptr, 0);
	Return(format_fixed_argument_(print, str, &ff));
	Return(format_fixed_float_(print, str, &ff));

	return 0;
}

static int format_call_Fixed(fmtprint print, struct format_operator *str)
{
	addr control;
	Execute ptr;

	Check(5 < str->args_size, "size error");
	ptr = print->ptr;
	push_control(ptr, &control);
	(void)format_call_Fixed_call_(print, str);
	return pop_control_(ptr, control);
}


/*
 *  Exponential
 */
static int format_exponent_single_(fmtprint print, fmtfloat ff, addr pos)
{
	addr stream;
	single_float value;
	struct fmtdecimal_struct dec;

	GetSingleFloat(pos, &value);
	ff->u.value_single = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_single_float(&dec, value, FMTFLOAT_ROUND_SINGLE))
		return fmte_("Invalid single-float.", pos, NULL);

	Return(fmtprint_stream_(print, &stream));
	Return(fmtfloat_exponent_(stream, ff, &dec));
	return fmtprint_stream_output_(print);
}

static int format_exponent_double_(fmtprint print, fmtfloat ff, addr pos)
{
	addr stream;
	double_float value;
	struct fmtdecimal_struct dec;

	GetDoubleFloat(pos, &value);
	ff->u.value_double = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_double_float(&dec, value, FMTFLOAT_ROUND_DOUBLE))
		return fmte_("Invalid double-float.", pos, NULL);

	Return(fmtprint_stream_(print, &stream));
	Return(fmtfloat_exponent_(stream, ff, &dec));
	return fmtprint_stream_output_(print);
}

static int format_exponent_long_(fmtprint print, fmtfloat ff, addr pos)
{
	addr stream;
	long_float value;
	struct fmtdecimal_struct dec;

	GetLongFloat(pos, &value);
	ff->u.value_long = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_long_float(&dec, value, FMTFLOAT_ROUND_LONG))
		return fmte_("Invalid long-float.", pos, NULL);

	Return(fmtprint_stream_(print, &stream));
	Return(fmtfloat_exponent_(stream, ff, &dec));
	return fmtprint_stream_output_(print);
}

static int format_exponent_argument_(fmtprint print,
		struct format_operator *str, fmtfloat ff)
{
	memset(ff, 0, sizeoft(struct fmtfloat_struct));
	Return(fmtfloat_w(print, str, ff, 0));
	Return(fmtfloat_d(print, str, ff, 1));
	Return(fmtfloat_e(print, str, ff, 2));
	Return(fmtfloat_k(print, str, ff, 3, 1));
	Return(fmtfloat_overflowchar_(print, str, ff, 4));
	Return(fmtfloat_padchar_(print, str, ff, 5));
	ff->k_bias = 1; /* 1 if exponent */
	ff->markerp = 1;
	ff->sign_exponent = 1;
	ff->sign = str->atsign;

	return 0;
}

static int fmtfloat_default_marker_(Execute ptr, unicode *ret)
{
	addr pos, check;

	/* default */
	GetConst(SPECIAL_READ_DEFAULT_FLOAT_FORMAT, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	/* single-flaot */
	GetConst(COMMON_SINGLE_FLOAT, &check);
	if (check == pos)
		return Result(ret, 'F');
	/* double-float */
	GetConst(COMMON_DOUBLE_FLOAT, &check);
	if (check == pos)
		return Result(ret, 'D');
	/* long-float */
	GetConst(COMMON_LONG_FLOAT, &check);
	if (check == pos)
		return Result(ret, 'L');
	/* short-float */
	GetConst(COMMON_SHORT_FLOAT, &check);
	if (check == pos)
		return Result(ret, 'S');
	/* error */
	*ret = 0;
	return fmte_("Invalid *read-default-float-format* value ~S.", pos, NULL);
}

static unicode fmtfloat_type_marker(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT: return 'F';
		case LISPTYPE_DOUBLE_FLOAT: return 'D';
		case LISPTYPE_LONG_FLOAT: return 'L';
		default: return 'E';
	}
}

static int fmtfloat_marker_(fmtprint print,
		struct format_operator *str, fmtfloat ff, addr pos)
{
	int check;
	unicode u, marker1, marker2;

	Return(fmtchar_nilp_(print, str, 6, &u, &check));
	if (! check) {
		ff->marker = u;
	}
	else {
		Return(fmtfloat_default_marker_(print->ptr, &marker1));
		marker2 = fmtfloat_type_marker(pos);
		ff->marker = (marker1 == marker2)? 'E': marker2;
	}

	return 0;
}

static int format_exponent_fixnum_(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	single_float_fixnum_local(print->local, &pos, pos);
	return format_exponent_single_(print, ff, pos);
}

static int format_exponent_bignum_(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	Return(single_float_bignum_local_(print->local, &pos, pos));
	return format_exponent_single_(print, ff, pos);
}

static int format_exponent_ratio_(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	Return(single_float_ratio_local_(print->local, &pos, pos));
	return format_exponent_single_(print, ff, pos);
}

static int format_exponent_float_(fmtprint print,
		struct format_operator *str, fmtfloat ff, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_DOUBLE_FLOAT:
			return format_exponent_double_(print, ff, pos);

		case LISPTYPE_SINGLE_FLOAT:
			return format_exponent_single_(print, ff, pos);

		case LISPTYPE_LONG_FLOAT:
			return format_exponent_long_(print, ff, pos);

		case LISPTYPE_FIXNUM:
			return format_exponent_fixnum_(print, ff, pos);

		case LISPTYPE_BIGNUM:
			return format_exponent_bignum_(print, ff, pos);

		case LISPTYPE_RATIO:
			return format_exponent_ratio_(print, ff, pos);

		default:
			return fmtprop_abort_(print, str,
					"~~E argument ~S must be a real type.", pos, NULL);
	}
}

static int format_call_Exponential_call_(fmtprint print, struct format_operator *str)
{
	Execute ptr;
	addr pos;
	struct fmtfloat_struct ff;

	ptr = print->ptr;
	/* (let ((*print-escape* nil)) ...) */
	push_escape_print(ptr, 0);
	Return(format_exponent_argument_(print, str, &ff));
	Return(fmtprint_pop_(print, str, &pos));
	Return(fmtfloat_marker_(print, str, &ff, pos));
	Return(format_exponent_float_(print, str, &ff, pos));

	return 0;
}

static int format_call_Exponential(fmtprint print, struct format_operator *str)
{
	Execute ptr;
	addr control;

	Check(7 < str->args_size, "size error");
	ptr = print->ptr;
	push_control(ptr, &control);
	(void)format_call_Exponential_call_(print, str);
	return pop_control_(ptr, control);
}


/*
 *  General
 */
static int fmtfloat_e_general_(fmtprint print,
		struct format_operator *str, fmtfloat ff, unsigned index)
{
	int check;
	fixnum value;

	Return(fmtint_nilp_(print, str, index, &value, &check));
	if (check) {
		ff->ep = 0;
		ff->e = 1;
	}
	else {
		if (value < 0) {
			return fmtargs_abort_(print, str, index,
					"The parameter must be a positive integer.", NULL);
		}
		ff->ep = 1;
		ff->e = (size_t)(value? value: 1);
	}

	return 0;
}

static int format_general_argument_(fmtprint print,
		struct format_operator *str, fmtfloat ff)
{
	memset(ff, 0, sizeoft(struct fmtfloat_struct));
	Return(fmtfloat_w(print, str, ff, 0));
	Return(fmtfloat_d(print, str, ff, 1));
	Return(fmtfloat_e_general_(print, str, ff, 2));
	Return(fmtfloat_k(print, str, ff, 3, 1));
	Return(fmtfloat_overflowchar_(print, str, ff, 4));
	Return(fmtfloat_padchar_(print, str, ff, 5));
	ff->sign = str->atsign;

	return 0;
}

static int format_general_single_(fmtprint print, fmtfloat ff, addr pos)
{
	addr stream;
	single_float value;
	struct fmtdecimal_struct dec;

	GetSingleFloat(pos, &value);
	ff->u.value_single = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_single_float(&dec, value, FMTFLOAT_ROUND_SINGLE))
		return fmte_("Invalid single-float.", pos, NULL);

	Return(fmtprint_stream_(print, &stream));
	Return(fmtfloat_general_(stream, ff, &dec));
	return fmtprint_stream_output_(print);
}

static int format_general_double_(fmtprint print, fmtfloat ff, addr pos)
{
	addr stream;
	double_float value;
	struct fmtdecimal_struct dec;

	GetDoubleFloat(pos, &value);
	ff->u.value_double = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_double_float(&dec, value, FMTFLOAT_ROUND_DOUBLE))
		return fmte_("Invalid double-float.", pos, NULL);

	Return(fmtprint_stream_(print, &stream));
	Return(fmtfloat_general_(stream, ff, &dec));
	return fmtprint_stream_output_(print);
}

static int format_general_long_(fmtprint print, fmtfloat ff, addr pos)
{
	addr stream;
	long_float value;
	struct fmtdecimal_struct dec;

	GetLongFloat(pos, &value);
	ff->u.value_long = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_long_float(&dec, value, FMTFLOAT_ROUND_LONG))
		return fmte_("Invalid long-float.", pos, NULL);

	Return(fmtprint_stream_(print, &stream));
	Return(fmtfloat_general_(stream, ff, &dec));
	return fmtprint_stream_output_(print);
}

static int format_general_fixnum_(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	single_float_fixnum_local(print->local, &pos, pos);
	return format_general_single_(print, ff, pos);
}

static int format_general_bignum_(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	Return(single_float_bignum_local_(print->local, &pos, pos));
	return format_general_single_(print, ff, pos);
}

static int format_general_ratio_(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	Return(single_float_ratio_local_(print->local, &pos, pos));
	return format_general_single_(print, ff, pos);
}

static int format_general_float_(fmtprint print,
		struct format_operator *str, fmtfloat ff, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_DOUBLE_FLOAT:
			return format_general_double_(print, ff, pos);

		case LISPTYPE_SINGLE_FLOAT:
			return format_general_single_(print, ff, pos);

		case LISPTYPE_LONG_FLOAT:
			return format_general_long_(print, ff, pos);

		case LISPTYPE_FIXNUM:
			return format_general_fixnum_(print, ff, pos);

		case LISPTYPE_BIGNUM:
			return format_general_bignum_(print, ff, pos);

		case LISPTYPE_RATIO:
			return format_general_ratio_(print, ff, pos);

		default:
			return fmtprop_abort_(print, str,
					"~~G argument ~S must be a real type.", pos, NULL);
	}
}

static int format_call_General_call_(fmtprint print, struct format_operator *str)
{
	Execute ptr;
	addr pos;
	struct fmtfloat_struct ff;

	ptr = print->ptr;
	/* (let ((*print-escape* nil)) ...) */
	push_escape_print(ptr, 0);
	Return(format_general_argument_(print, str, &ff));
	Return(fmtprint_pop_(print, str, &pos));
	Return(fmtfloat_marker_(print, str, &ff, pos));
	Return(format_general_float_(print, str, &ff, pos));

	return 0;
}

static int format_call_General(fmtprint print, struct format_operator *str)
{
	Execute ptr;
	addr control;

	Check(7 < str->args_size, "size error");
	ptr = print->ptr;
	push_control(ptr, &control);
	(void)format_call_General_call_(print, str);
	return pop_control_(ptr, control);
}


/*
 *  Monetary
 */
static int format_monetary_single_(fmtprint print, fmtfloat ff, addr pos)
{
	addr stream;
	single_float value;
	struct fmtdecimal_struct dec;

	GetSingleFloat(pos, &value);
	ff->u.value_single = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_single_float(&dec, value, FMTFLOAT_ROUND_SINGLE))
		return fmte_("Invalid single-float.", pos, NULL);

	Return(fmtprint_stream_(print, &stream));
	Return(fmtfloat_monetary_(stream, ff, &dec));
	return fmtprint_stream_output_(print);
}

static int format_monetary_double_(fmtprint print, fmtfloat ff, addr pos)
{
	addr stream;
	double_float value;
	struct fmtdecimal_struct dec;

	GetDoubleFloat(pos, &value);
	ff->u.value_double = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_double_float(&dec, value, FMTFLOAT_ROUND_DOUBLE))
		return fmte_("Invalid double-float.", pos, NULL);

	Return(fmtprint_stream_(print, &stream));
	Return(fmtfloat_monetary_(stream, ff, &dec));
	return fmtprint_stream_output_(print);
}

static int format_monetary_long_(fmtprint print, fmtfloat ff, addr pos)
{
	addr stream;
	long_float value;
	struct fmtdecimal_struct dec;

	GetLongFloat(pos, &value);
	ff->u.value_long = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_long_float(&dec, value, FMTFLOAT_ROUND_LONG))
		return fmte_("Invalid long-float.", pos, NULL);

	Return(fmtprint_stream_(print, &stream));
	Return(fmtfloat_monetary_(stream, ff, &dec));
	return fmtprint_stream_output_(print);
}

static int format_monetary_fixnum_(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	single_float_fixnum_local(print->local, &pos, pos);
	return format_monetary_single_(print, ff, pos);
}

static int format_monetary_bignum_(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	Return(single_float_bignum_local_(print->local, &pos, pos));
	return format_monetary_single_(print, ff, pos);
}

static int format_monetary_ratio_(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	Return(single_float_ratio_local_(print->local, &pos, pos));
	return format_monetary_single_(print, ff, pos);
}

static int format_monetary_default_(fmtprint print, fmtfloat ff, addr pos)
{
	return format_call_print(print, pos, 0, 1, ff->w, 1, 0, ' ', 0);
}

static int format_monetary_float_(fmtprint print,
		struct format_operator *str, fmtfloat ff)
{
	addr pos;

	Return(fmtprint_pop_(print, str, &pos));
	switch (GetType(pos)) {
		case LISPTYPE_DOUBLE_FLOAT:
			return format_monetary_double_(print, ff, pos);

		case LISPTYPE_SINGLE_FLOAT:
			return format_monetary_single_(print, ff, pos);

		case LISPTYPE_LONG_FLOAT:
			return format_monetary_long_(print, ff, pos);

		case LISPTYPE_FIXNUM:
			return format_monetary_fixnum_(print, ff, pos);

		case LISPTYPE_BIGNUM:
			return format_monetary_bignum_(print, ff, pos);

		case LISPTYPE_RATIO:
			return format_monetary_ratio_(print, ff, pos);

		default:
			return format_monetary_default_(print, ff, pos);
	}
}

static int fmtfloat_n_(fmtprint print,
		struct format_operator *str, fmtfloat ff, unsigned index)
{
	int check;
	fixnum value;

	Return(fmtint_nilp_(print, str, index, &value, &check));
	if (! check && value < 0) {
		return fmtargs_abort_(print, str, index,
				"The parameter must be a positive integer.", NULL);
	}
	ff->np = ! check;
	ff->n = check? 0: (size_t)value;

	return 0;
}

static int format_monetary_argument_(fmtprint print,
		struct format_operator *str, fmtfloat ff)
{
	memset(ff, 0, sizeoft(struct fmtfloat_struct));
	Return(fmtfloat_d(print, str, ff, 0));
	Return(fmtfloat_n_(print, str, ff, 1));
	Return(fmtfloat_w(print, str, ff, 2));
	Return(fmtfloat_padchar_(print, str, ff, 3));
	ff->sign = str->atsign;
	ff->markerp = str->colon;

	return 0;
}

static int format_call_Monetary_call_(fmtprint print, struct format_operator *str)
{
	Execute ptr;
	struct fmtfloat_struct ff;

	ptr = print->ptr;
	/* (let ((*print-escape* nil)) ...) */
	push_escape_print(ptr, 0);
	Return(format_monetary_argument_(print, str, &ff));
	Return(format_monetary_float_(print, str, &ff));
	return 0;
}

static int format_call_Monetary(fmtprint print, struct format_operator *str)
{
	Execute ptr;
	addr control;

	Check(4 < str->args_size, "size error");
	ptr = print->ptr;
	push_control(ptr, &control);
	(void)format_call_Monetary_call_(print, str);
	return pop_control_(ptr, control);
}


/*
 *  Newline
 */
static int format_call_Newline(fmtprint print, struct format_operator *str)
{
	addr stream;
	fixnum i, size;

	Check(1 < str->args_size, "size error");
	Return(fmtint_default_(print, str, 0, &size, 1));
	if (size < 0) {
		return fmtargs_abort_(print, str, 0,
				"The parameter must be a positive integer.", NULL);
	}

	Return(fmtprint_stream_(print, &stream));
	for (i = 0; i < size; i++) {
		Return(terpri_stream_(stream));
	}

	return fmtprint_stream_output_(print);
}


/*
 *  FreshLine
 */
static int format_call_FreshLine(fmtprint print, struct format_operator *str)
{
	addr stream;
	fixnum i, size;

	Check(1 < str->args_size, "size error");
	Return(fmtint_default_(print, str, 0, &size, 1));
	if (size < 0) {
		return fmtargs_abort_(print, str, 0,
				"The parameter must be a positive integer.", NULL);
	}
	if (size) {
		Return(fmtprint_stream_(print, &stream));
		Return(fresh_line_stream_(stream, NULL));
		for (i = 1; i < size; i++) {
			Return(terpri_stream_(stream));
		}
		Return(fmtprint_stream_output_(print));
	}

	return 0;
}


/*
 *  Page
 */
static int format_call_Page(fmtprint print, struct format_operator *str)
{
	addr stream;
	fixnum i, size;

	Check(1 < str->args_size, "size error");
	Return(fmtint_default_(print, str, 0, &size, 1));
	if (size < 0) {
		return fmtargs_abort_(print, str, 0,
				"The parameter must be a positive integer.", NULL);
	}

	Return(fmtprint_stream_(print, &stream));
	for (i = 0; i < size; i++) {
		Return(pageout_stream_(stream));
	}

	return fmtprint_stream_output_(print);
}


/*
 *  Tilde
 */
static int format_call_Tilde(fmtprint print, struct format_operator *str)
{
	fixnum size;

	Check(1 < str->args_size, "size error");
	Return(fmtint_default_(print, str, 0, &size, 1));
	if (size < 0) {
		return fmtargs_abort_(print, str, 0,
				"The parameter must be a positive integer.", NULL);
	}

	return fmtprint_putc_times_(print, '~', size);
}


/*
 *  IgnoredNewline
 */
static int format_call_IgnoredNewline(fmtprint print, struct format_operator *str)
{
	/*
	 * ~\n   -> delete space
	 * ~:\n  -> do-nothing
	 * ~@\n  -> output newline, delete space
	 * ~:@\n -> output newline
	 */
	addr stream;

	Check(0 < str->args_size, "size error");
	if (str->atsign) {
		/* output newline */
		Return(fmtprint_stream_(print, &stream));
		Return(terpri_stream_(stream));
		Return(fmtprint_stream_output_(print));
	}
	if (! str->colon) {
		/* delete space */
		print->delete_space = 1;
	}

	return 0;
}


/*
 *  Tabulate
 */
static int format_call_Tabulate(fmtprint print, struct format_operator *str)
{
	addr stream;
	fixnum column, colinc, now;
	size_t size;

	Check(2 < str->args_size, "size error");
	Return(fmtint_default_(print, str, 0, &column, 1));
	Return(fmtint_default_(print, str, 1, &colinc, 1));
	if (column < 0) {
		return fmtargs_abort_(print, str, 0,
				"The parameter must be greater than equal to 0.", NULL);
	}
	if (colinc < 0) {
		return fmtargs_abort_(print, str, 1,
				"The parameter must be greater than equal to 0.", NULL);
	}

	if (str->colon) {
		Return(fmtprint_stream_(print, &stream));
		if (str->atsign) {
			Return(pprint_tab_section_relative_(print->ptr, stream, column, colinc));
		}
		else {
			Return(pprint_tab_section_(print->ptr, stream, column, colinc));
		}
		Return(fmtprint_stream_output_(print));
	}
	else {
		Return(getleft_stream_(print->stream, &size));
		now = (fixnum)size;
		Check(now < 0, "cast error");
		if (str->atsign) {
			Return(pprint_tab_relative_force_(print->stream, column, colinc, now));
		}
		else {
			Return(pprint_tab_absolute_force_(print->stream, column, colinc, now));
		}
	}

	return 0;
}


/*
 *  GoTo
 */
static int format_call_GoTo(fmtprint print, struct format_operator *str)
{
	fixnum count;

	Check(1 < str->args_size, "size error");
	if (! str->atsign) {
		Return(fmtint_default_(print, str, 0, &count, 1));
		if (count < 0) {
			return fmtargs_abort_(print, str, 0,
					"The parameter must be greater than equal to 0.", NULL);
		}
		if (! str->colon) {
			Return(fmtprint_forward_(print, str, (size_t)count));
		}
		else {
			Return(fmtprint_rollback_(print, str, (size_t)count));
		}
	}
	else {
		Return(fmtint_default_(print, str, 0, &count, 0));
		if (count < 0) {
			return fmtargs_abort_(print, str, 0,
					"The parameter must be greater than equal to 0.", NULL);
		}
		if (! str->colon) {
			Return(fmtprint_absolute_(print, str, (size_t)count));
		}
		else {
			return fmtprop_abort_(print, str,
					"The parameter don't accept both : and @ parameter (~~:@*).", NULL);
		}
	}

	return 0;
}


/*
 *  Recursive
 */
static void fmtprint_format_forward(fmtprint print)
{
	struct format_operator *str;

	str = fmtprint_operator(print);
	Check(str->type != FormatType_Format, "format error");
	print->now += str->size;
}

static int format_call_Recursive_call_(fmtprint print,
		addr format, addr args, addr *ret)
{
	Execute ptr;
	addr stream, pos;

	ptr = print->ptr;
	Return(fmtprint_stream_(print, &stream));
	Return(format_execute(ptr, stream, format, args, ret));
	Return(string_stream_local_(ptr->local, stream, &pos));
	clear_output_string_stream(stream);
	return fmtprint_string_(print, pos);
}

static int format_call_Recursive_function(fmtprint print,
		struct format_operator *str, addr format)
{
	addr args;
	size_t size1, size2;

	args = print->rest->front;
	Return(length_list_safe_(args, &size1));
	Return(format_call_Recursive_call_(print, format, args, &args));
	/* result */
	Return(length_list_safe_(args, &size2));
	if (size1 < size2) {
		gchold_push_local(print->local, args);
		return fmtprint_abort_(print, str->colon_pos,
				"The FORMATTER ~S return illegal arguments ~S.", format, args, NULL);
	}
	for (; size2 < size1; size2++) {
		Return(fmtprint_pop_(print, str, &args));
	}

	return 0;
}

static int fmtcall(fmtprint print, int *loop);
static int format_call_Recursive_format(fmtprint print, addr format)
{
	addr backup_format;
	size_t backup_now;

	/* backup */
	backup_format = print->format;
	backup_now = print->now;
	/* execute */
	print->format = format;
	print->now = 0;
	fmtprint_format_forward(print);
	Return(fmtcall(print, NULL));
	print->loop = 1;
	/* rollback */
	print->format = backup_format;
	print->now = backup_now;

	return 0;
}

static int format_call_Recursive_atsign(fmtprint print,
		struct format_operator *str, addr format)
{
	if (functionp(format))
		return format_call_Recursive_function(print, str, format);
	if (formatp(format))
		return format_call_Recursive_format(print, format);
	if (stringp(format)) {
		Return(format_parse_local_(print->local, &format, format));
		return format_call_Recursive_format(print, format);
	}
	return fmtprop_abort_(print, str, "Invalid control-string ~S.", format, NULL);
}

static int format_call_Recursive(fmtprint print, struct format_operator *str)
{
	addr format, args;

	Check(0 < str->args_size, "size error");
	Check(str->colon, "Invalid argument [colon].");
	if (str->atsign) {
		Return(fmtprint_pop_(print, str, &format));
		return format_call_Recursive_atsign(print, str, format);
	}
	else {
		Return(fmtprint_pop_(print, str, &format));
		Return(fmtprint_pop_(print, str, &args));
		return format_call_Recursive_call_(print, format, args, &args);
	}
}


/*
 *  ConditionalNewline
 */
static int format_call_ConditionalNewline(fmtprint print, struct format_operator *str)
{
	addr stream;
	Execute ptr;

	Check(0 < str->args_size, "size error");
	ptr = print->ptr;
	stream = print->stream;
	if (str->colon && str->atsign)
		return pprint_newline_print_(ptr, pprint_newline_mandatory, stream);
	else if (str->colon)
		return pprint_newline_print_(ptr, pprint_newline_fill, stream);
	else if (str->atsign)
		return pprint_newline_print_(ptr, pprint_newline_miser, stream);
	else
		return pprint_newline_print_(ptr, pprint_newline_linear, stream);
}


/*
 *  Write
 */
static int format_call_Write_call_(fmtprint print, struct format_operator *str)
{
	Execute ptr;
	addr pos, stream;

	ptr = print->ptr;
	if (str->colon) {
		push_pretty_print(ptr, 1);
	}
	if (str->atsign) {
		push_level_nil_print(ptr);
		push_length_nil_print(ptr);
	}
	Return(fmtprint_pop_(print, str, &pos));
	Return(fmtprint_stream_(print, &stream));
	Return(write_print(ptr, stream, pos));
	Return(fmtprint_stream_output_(print));

	return 0;
}

static int format_call_Write(fmtprint print, struct format_operator *str)
{
	Execute ptr;
	addr control;

	Check(0 < str->args_size, "size error");
	ptr = print->ptr;
	push_control(ptr, &control);
	(void)format_call_Write_call_(print, str);
	return pop_control_(ptr, control);
}


/*
 *  Indent
 */
static int format_call_Indent(fmtprint print, struct format_operator *str)
{
	fixnum n;

	Check(1 < str->args_size, "size error");
	Check(str->atsign, "Invalid argument [atsign].");
	Return(fmtint_default_(print, str, 0, &n, 0));

	return pprint_indent_print_(print->ptr, ! str->colon, n, print->stream);
}


/*
 *  Case
 */
static int format_call_Case(fmtprint print, struct format_operator *str)
{
	enum fmtcase case1, case2;
	size_t now;

	Check(0 < str->args_size, "size error");
	/*
	 *  ~(		downcase
	 *  ~:(		all capitalize
	 *  ~@(		first capitalize
	 *  ~:@(	upcase
	 */
	case1 = print->conversion;
	now = print->now;

	if (str->colon && str->atsign)
		case2 = fmtcase_upcase;
	else if (str->colon) {
		print->word = 0;
		case2 = fmtcase_capitalize_all;
	}
	else if (str->atsign) {
		print->word = 0;
		print->first = 1;
		case2 = fmtcase_capitalize_first;
	}
	else
		case2 = fmtcase_downcase;

	/* update */
	print->conversion = case2;
	print->now += format_bytesize(0);
	/* call */
	Return(fmtcall(print, NULL));
	/* rollback */
	print->conversion = case1;
	print->now = now;

	return 0;
}


/*
 *  Condition
 */
static size_t *format_condition_array(struct format_operator *str)
{
	return (size_t *)format_getargs(str, (str->colon || str->atsign)? 0: 1);
}

static int format_condition_execute(
		fmtprint print, struct format_operator *str, size_t clause)
{
	size_t now;

	/* update */
	now = print->now;
	print->now += clause;
	/* call */
	Return(fmtcall(print, NULL));
	print->loop = 1;
	/* rollback */
	print->now = now;

	return 0;
}

static int format_condition_index(fmtprint print,
		struct format_operator *str, fixnum index, int ignore)
{
	size_t *array, pos, size;

	array = format_condition_array(str);
	size = *(array++);
	if (ignore || index < 0)
		goto clause_else;
	pos = (size_t)index;
	if (size <= pos)
		goto clause_else;
	return format_condition_execute(print, str, array[pos]);

clause_else:
	if (str->option_check == 0)
		return 0;
	return format_condition_execute(print, str, array[size - 1]);
}

static int format_condition_select(fmtprint print, struct format_operator *str)
{
	int check, ignore;
	addr pos;
	fixnum index;

	Check(1 < str->args_size, "size error");
	/* index */
	ignore = 0;
	Return(fmtint_nilp_(print, str, 0, &index, &check));
	if (check) {
		Return(fmtprint_pop_(print, str, &pos));
		if (! integerp(pos)) {
			return fmtprop_abort_(print, str,
					"The argument ~S must be an integer.", pos, NULL);
		}
		if (fixnump(pos)) {
			GetFixnum(pos, &index);
		}
		else {
			index = 0;
			ignore = 1;
		}
	}

	/* print */
	return format_condition_index(print, str, index, ignore);
}

static int format_condition_boolean(fmtprint print, struct format_operator *str)
{
	addr pos;

	Check(0 < str->args_size, "size error");
	Return(fmtprint_pop_(print, str, &pos));
	return format_condition_index(print, str, (pos != Nil), 0);
}

static int format_condition_true(fmtprint print, struct format_operator *str)
{
	addr pos;

	Check(0 < str->args_size, "size error");
	Return(fmtprint_peek_(print, str, &pos));
	if (pos != Nil)
		return format_condition_index(print, str, 0, 0);
	Return(fmtprint_pop_(print, str, &pos));
	return 0; /* do nothing */
}

static int format_call_Condition(fmtprint print, struct format_operator *str)
{
	/*
	 *  ~[		select
	 *  ~:[		boolean
	 *  ~@[		true
	 *  ~:@[	error
	 */
	Check(str->colon && str->atsign, "Invalid arguemnt [colon && atsing].");
	if (str->colon)
		return format_condition_boolean(print, str);
	else if (str->atsign)
		return format_condition_true(print, str);
	else
		return format_condition_select(print, str);
}


/*
 *  Iteration
 */
static int format_call_Iteration_exit(
		int intp, fixnum i, fixnum index, addr list, int *forcep)
{
	if (*forcep) {
		*forcep = 0;
		return 0;
	}
	if ((intp == 0) && (index <= i))
		return 1;
	if (list == Nil)
		return 1;

	return 0;
}

static int format_call_Iteration_list(fmtprint print,
		struct format_operator *str, int forcep)
{
	int intp, check, result;
	addr pos, stream, root;
	fixnum index, i;
	struct fmtstack args;
	size_t now;

	/* argument */
	Return(fmtprint_pop_(print, str, &pos));
	Return(fmtint_nilp_(print, str, 0, &index, &intp));

	/* save */
	args.root = pos;
	args.front = pos;
	args.index = 0;
	print->rest = &args;
	now = print->now;

	stream = print->stream;
	root = Nil;
	check = pretty_stream_p(stream);
	if (check) {
		Return(root_pretty_stream_(stream, &root));
		gchold_push_local(print->local, root);
		Return(setroot_pretty_stream_(stream, pos));
	}

	/* loop */
	result = 0;
	for (i = 0; print->loop; i++) {
		if (format_call_Iteration_exit(intp, i, index, args.front, &forcep))
			break;
		print->now = now;
		if (fmtcall(print, NULL)) {
			result = 1;
			goto finish;
		}
	}

	/* rollback */
finish:
	if (check) {
		Return(setroot_pretty_stream_(stream, root));
	}

	return result;
}

static int format_call_Iteration_rest(fmtprint print,
		struct format_operator *str, int forcep)
{
	int intp;
	fixnum index, i;
	struct fmtstack *rest;
	size_t now;

	Return(fmtint_nilp_(print, str, 0, &index, &intp));
	now = print->now;
	rest = print->rest;
	for (i = 0; print->loop; i++) {
		if (format_call_Iteration_exit(intp, i, index, rest->front, &forcep))
			break;
		print->now = now;
		Return(fmtcall(print, NULL));
	}

	return 0;
}

static int format_call_Iteration_listargs(fmtprint print,
		struct format_operator *str, int forcep)
{
	int intp, check, result, loop_check;
	fixnum index, i;
	addr stream, root, car, cdr;
	struct fmtstack args;
	size_t now;

	/* argument */
	Return(fmtprint_pop_(print, str, &cdr));
	Return(fmtint_nilp_(print, str, 0, &index, &intp));

	/* save */
	stream = print->stream;
	root = Nil;
	check = pretty_stream_p(stream);
	if (check) {
		Return(root_pretty_stream_(stream, &root));
		gchold_push_local(print->local, root);
	}

	/* loop */
	now = print->now;
	result = 0;
	for (i = 0; ; i++) {
		if (format_call_Iteration_exit(intp, i, index, cdr, &forcep))
			break;
		Return_getcons(cdr, &car, &cdr);
		args.root = car;
		args.front = car;
		args.index = 0;
		print->now = now;
		print->rest = &args;
		print->last = (cdr == Nil);
		if (check) {
			Return(setroot_pretty_stream_(stream, car));
		}
		if (fmtcall(print, &loop_check)) {
			result = 1;
			goto finish;
		}
		if (loop_check)
			break;
		print->loop = 1;
	}

	/* rollback */
finish:
	if (check) {
		Return(setroot_pretty_stream_(stream, root));
	}

	return result;
}

static int format_call_Iteration_restargs(fmtprint print,
		struct format_operator *str, int forcep)
{
	int intp, check, result, loop_check;
	addr stream, pos, root;
	fixnum index, i;
	struct fmtstack *rest, args;
	size_t now;

	Return(fmtint_nilp_(print, str, 0, &index, &intp));
	now = print->now;
	rest = print->rest;

	/* save */
	stream = print->stream;
	check = pretty_stream_p(stream);

	root = Nil;
	for (i = 0; ; i++) {
		if (format_call_Iteration_exit(intp, i, index, rest->front, &forcep))
			break;
		print->rest = rest;
		Return(fmtprint_pop_(print, str, &pos));
		if (check) {
			Return(root_pretty_stream_(stream, &root));
		}

		args.root = pos;
		args.front = pos;
		args.index = 0;
		print->now = now;
		print->rest = &args;
		print->last = (rest->front == Nil);
		if (check) {
			Return(setroot_pretty_stream_(stream, pos));
		}
		result = fmtcall(print, &loop_check);
		if (check) {
			Return(setroot_pretty_stream_(stream, root));
		}
		if (result)
			return 1;
		if (loop_check)
			break;
		print->loop = 1;
	}

	return 0;
}

static int format_call_Iteration_function(fmtprint print, struct format_operator *str)
{
	if (str->colon && str->atsign)
		return format_call_Iteration_restargs(print, str, str->close_colon);
	else if (str->colon)
		return format_call_Iteration_listargs(print, str, str->close_colon);
	else if (str->atsign)
		return format_call_Iteration_rest(print, str, str->close_colon);
	else
		return format_call_Iteration_list(print, str, str->close_colon);
}

static int format_call_Iteration_call(fmtprint print, struct format_operator *str)
{
	int result;
	struct fmtstack *rest;
	size_t now;

	/* backup */
	now = print->now;
	rest = print->rest;
	/* execute */
	print->now += format_bytesize(1);
	result = format_call_Iteration_function(print, str);
	/* rollback */
	print->now = now;
	print->rest = rest;
	print->loop = 1;

	return result;
}

/* empty */
static int format_call_Iteration2_list(fmtprint print,
		struct format_operator *str, addr format)
{
	int intp, forcep, result, check;
	addr pos, stream, root;
	fixnum index, i;
	struct fmtstack args;

	/* argument */
	Return(fmtprint_pop_(print, str, &pos));
	Return(fmtint_nilp_(print, str, 0, &index, &intp));
	forcep = str->close_colon;

	/* save */
	args.root = pos;
	args.front = pos;
	args.index = 0;
	print->rest = &args;

	root = Nil;
	stream = print->stream;
	check = pretty_stream_p(stream);
	if (check) {
		Return(root_pretty_stream_(stream, &root));
		gchold_push_local(print->local, root);
		Return(setroot_pretty_stream_(stream, pos));
	}

	/* loop */
	result = 0;
	for (i = 0; ; i++) {
		if (format_call_Iteration_exit(intp, i, index, args.front, &forcep))
			break;
		if (format_call_Recursive_function(print, str, format)) {
			result = 1;
			goto finish;
		}
	}

	/* rollback */
finish:
	if (check) {
		Return(setroot_pretty_stream_(stream, root));
	}

	return result;
}

static int format_call_Iteration2_rest(fmtprint print,
		struct format_operator *str, addr format)
{
	int intp, forcep;
	fixnum index, i;
	struct fmtstack *rest;

	Return(fmtint_nilp_(print, str, 0, &index, &intp));
	forcep = str->close_colon;
	rest = print->rest;
	for (i = 0; ; i++) {
		if (format_call_Iteration_exit(intp, i, index, rest->front, &forcep))
			break;
		Return(format_call_Recursive_function(print, str, format));
	}

	return 0;
}

static int format_call_Iteration2_listargs(fmtprint print,
		struct format_operator *str, addr format)
{
	int intp, forcep, result, check;
	fixnum index, i;
	addr stream, root, car, cdr;
	struct fmtstack args;

	/* argument */
	Return(fmtprint_pop_(print, str, &cdr));
	Return(fmtint_nilp_(print, str, 0, &index, &intp));

	/* save */
	stream = print->stream;
	root = Nil;
	check = pretty_stream_p(stream);
	if (check) {
		Return(root_pretty_stream_(stream, &root));
		gchold_push_local(print->local, root);
	}

	/* loop */
	forcep = str->close_colon;
	result = 0;
	for (i = 0; ; i++) {
		if (format_call_Iteration_exit(intp, i, index, cdr, &forcep))
			break;
		Return_getcons(cdr, &car, &cdr);
		args.root = car;
		args.front = car;
		args.index = 0;
		print->rest = &args;
		print->last = (cdr == Nil);
		if (check) {
			Return(setroot_pretty_stream_(stream, car));
		}
		if (format_call_Recursive_call_(print, format, car, &car)) {
			result = 1;
			goto finish;
		}
	}

	/* rollback */
finish:
	if (check) {
		Return(setroot_pretty_stream_(stream, root));
	}

	return result;
}

static int format_call_Iteration2_restargs(fmtprint print,
		struct format_operator *str, addr format)
{
	int intp, forcep, check, result;
	addr stream, pos, root;
	fixnum index, i;
	struct fmtstack *rest, args;

	Return(fmtint_nilp_(print, str, 0, &index, &intp));
	forcep = str->close_colon;
	rest = print->rest;

	/* save */
	stream = print->stream;
	check = pretty_stream_p(stream);

	for (i = 0; ; i++) {
		if (format_call_Iteration_exit(intp, i, index, rest->front, &forcep))
			break;
		print->rest = rest;
		Return(fmtprint_pop_(print, str, &pos));
		root = Nil;
		if (check) {
			Return(root_pretty_stream_(stream, &root));
		}

		args.root = pos;
		args.front = pos;
		args.index = 0;
		print->rest = &args;
		print->last = (rest->front == Nil);
		if (check) {
			Return(setroot_pretty_stream_(stream, pos));
		}
		result = format_call_Recursive_call_(print, format, pos, &pos);
		if (check) {
			Return(setroot_pretty_stream_(stream, root));
		}
		if (result)
			return 1;
	}

	return 0;
}

static int format_call_Iteration2_call(
		fmtprint print, struct format_operator *str, addr format)
{
	if (str->colon && str->atsign)
		return format_call_Iteration2_restargs(print, str, format);
	else if (str->colon)
		return format_call_Iteration2_listargs(print, str, format);
	else if (str->atsign)
		return format_call_Iteration2_rest(print, str, format);
	else
		return format_call_Iteration2_list(print, str, format);
}

static int format_call_Iteration2_function(
		fmtprint print, struct format_operator *str, addr format)
{
	struct fmtstack *rest;

	rest = print->rest;
	Return(format_call_Iteration2_call(print, str, format));
	print->rest = rest;

	return 0;
}

static int format_call_Iteration2_format(
		fmtprint print, struct format_operator *str, addr format)
{
	int result;
	addr backup_format;
	struct fmtstack *backup_rest;
	size_t backup_now;

	/* structure */
	backup_format = print->format;
	backup_now = print->now;
	backup_rest = print->rest;
	print->now = 0;
	print->format = format;
	fmtprint_format_forward(print);
	/* call */
	result = format_call_Iteration_function(print, str);
	/* rollback */
	print->format = backup_format;
	print->now = backup_now;
	print->rest = backup_rest;
	print->loop = 1;

	return result;
}

static int format_call_Iteration2(fmtprint print, struct format_operator *str)
{
	addr format;

	/* first */
	Return(fmtprint_pop_(print, str, &format));
	if (functionp(format))
		return format_call_Iteration2_function(print, str, format);
	if (formatp(format))
		return format_call_Iteration2_format(print, str, format);
	if (stringp(format)) {
		Return(format_parse_local_(print->ptr->local, &format, format));
		return format_call_Iteration2_format(print, str, format);
	}
	return fmtprop_abort_(print, str, "Invalid control-string ~S.", format, NULL);
}

static int format_call_Iteration(fmtprint print, struct format_operator *str)
{
	/*
	 *  "~{"		"~{ ~S ~S~}" '(a 1 b 2 c 3)
	 *  "~:{"		"~{ ~S ~S~}" '((a 1) (b 2) (c 3))
	 *  "~@{"		"~{ ~S ~S~}" 'a '1 'b '2 'c '3
	 *  "~:@{"		"~{ ~S ~S~}" '(a 1) '(b 2) '(c 3)
	 */
	int result;
	unsigned escape;

	Check(1 < str->args_size, "size error");
	escape = print->escape;
	print->escape = 1;
	if (str->option_check)
		result = format_call_Iteration2(print, str);
	else
		result = format_call_Iteration_call(print, str);
	print->escape = escape;

	return result;
}


/*
 *  Justification
 */
struct format_justification {
	int second;
	Execute ptr;
	LocalRoot local;
	fmtprint print;
	struct format_operator *str;
	size_t *array, size, allsize, div, space, count, width;
	size_t mincol, colinc, minpad;
	unicode padchar;
	addr vector;
};

static size_t *format_justification_array(struct format_operator *str)
{
	return (size_t *)format_getargs(str, 4);
}

static int format_call_Justification_vector1(struct format_justification *just)
{
	LocalRoot local;
	addr vector, pos;

	local = just->local;
	vector_local(local, &vector, 1);
	strvect_local(local, &pos, 0);
	Return(vector_set_(vector, 0, pos));
	just->vector = vector;
	just->size = 1;
	just->allsize = 0;

	return 0;
}

static int format_call_Justificaion_separator(
		fmtprint print, struct format_operator *str,
		size_t *rcount, size_t *rwidth)
{
	int check;
	fixnum count, width;
	size_t value;

	/* NULL */
	if (rcount == NULL)
		return 0;

	/* first */
	Return(fmtint_default_(print, str, 0, &count, 0));
	if (count < 0) {
		return fmtargs_abort_(print, str, 0,
				"The paramter must be a non-negative integer.", NULL);
	}
	*rcount = (size_t)count;

	/* second */
	Return(fmtint_nilp_(print, str, 1, &width, &check));
	if (check) {
		Return(termsize_stream_(print->stream, &value, &check));
		if (check)
			value = PRINT_DEFAULT_WIDTH;
		*rwidth = value;
	}
	else {
		if (width < 0) {
			return fmtargs_abort_(print, str, 1,
					"The parameter must be non-negative integer.", NULL);
		}
		*rwidth = (size_t)width;
	}

	return 0;
}

static int format_call_Justificaion_fmtcall(fmtprint print,
		size_t *count, size_t *width)
{
	enum FormatType type;
	addr stream, backup_string;
	struct format_operator *str;
	fmtcalltype call;

	/* string-stream */
	Return(fmtprint_make_string_(print, &stream, &backup_string));
	/* loop */
	str = fmtprint_operator(print);
	type = str->type;
	while (print->loop) {
		/* End */
		if (type == FormatType_End)
			break;
		/* ~:; */
		if (type == FormatType_ClauseSeparator) {
			Return(format_call_Justificaion_separator(print, str, count, width));
			break;
		}
		/* delete-space */
		if (type != FormatType_Output)
			print->delete_space = 0;
		/* call */
		call = FormatCallTable[type];
		Check(call == NULL, "Invalid format type.");
		Return((*call)(print, str));
		/* next */
		print->now += str->size;
		str = fmtprint_operator(print);
		type = str->type;
	}
	/* close */
	close_output_string_stream(stream);
	print->string = backup_string;

	return 0;
}

static int format_call_Justificaion_call(struct format_justification *just,
		size_t now, addr *ret, size_t *count, size_t *width)
{
	addr stream;
	fmtprint print;
	LocalRoot local;
	struct fmtprint_struct loop;

	print = just->print;
	local = print->local;

	fmtprint_copy(&loop, print);
	loop.pretty = 0;
	loop.escape = 0;
	loop.fill = print->fill;
	loop.fill_white = print->fill_white;
	loop.fill_ignore = print->fill_ignore;
	loop.conversion = print->conversion;
	loop.now = now;
	loop.rest = print->rest;

	Return(fmtprint_stream_(print, &stream));
	loop.stream = stream;
	Return(format_call_Justificaion_fmtcall(&loop, count, width));
	if (loop.loop) {
		print->loop = 1;
		Return(string_stream_local_(local, stream, ret));
	}
	else {
		print->loop = 0;
		strvect_local(local, ret, 0);
	}
	clear_output_string_stream(stream);

	return 0;
}

static int format_call_Justificaion_prefix(
		struct format_justification *just, size_t now, addr *ret)
{
	size_t count, width;

	Return(format_call_Justificaion_call(just, now, ret, &count, &width));
	just->count = count;
	just->width = width;

	return 0;
}

static int format_call_Justificaion_clause(
		struct format_justification *just, size_t now, addr *ret)
{
	return format_call_Justificaion_call(just, now, ret, NULL, NULL);
}

static int format_call_Justification_vector2(struct format_justification *just)
{
	LocalRoot local;
	fmtprint print;
	addr vector, pos;
	size_t *array, size, allsize, value, i, now;

	print = just->print;
	local = just->local;
	size = just->size;
	array = just->array;

	vector_local(local, &vector, size);
	allsize = 0;
	now = print->now;
	for (i = 0; i < size; i++) {
		Return(format_call_Justificaion_clause(just, now + array[i], &pos));
		if (print->loop == 0) {
			just->size = i;
			break;
		}
		Return(vector_set_(vector, i, pos));
		/* string */
		Return(eastasian_length_(pos, &value, NULL));
		allsize += value;
	}
	print->loop = 1;
	just->vector = vector;
	just->allsize = allsize;

	return 0;
}

static int format_call_Justification_vector(struct format_justification *just)
{
	size_t *array, size;

	array = format_justification_array(just->str);
	size = *(array++);
	if (just->second) {
		Check(size == 0, "size error");
		size--;
		array++;
	}
	just->array = array;
	just->size = size;
	if (size)
		Return(format_call_Justification_vector2(just));
	if (just->size == 0)
		return format_call_Justification_vector1(just);

	return 0;
}

static void format_call_Justification_count(struct format_justification *just)
{
	struct format_operator *str;
	size_t width;

	str = just->str;
	Check(just->size == 0, "size zero error");
	if (just->size == 1) {
		just->div = 1;
		if (str->colon && str->atsign)
			just->div++;
	}
	else {
		just->div = just->size - 1;
		if (str->colon)
			just->div++;
		if (str->atsign)
			just->div++;
	}
	width = just->div * just->minpad;
	if (just->mincol < just->allsize + width) {
		width = just->allsize + width - just->mincol;
		just->mincol += format_colinc_division(width, just->colinc);
	}
	Check(just->mincol < just->allsize, "mincol error");
	just->space = just->mincol - just->allsize;
}

static int format_call_Justification_space_(
		struct format_justification *just, size_t index)
{
	size_t space, div;

	space = just->space;
	div = just->div;
	space = ((index + 1) * space / div) - (index * space / div);
	return fmtprint_putc_times_(just->print, just->padchar, space);
}

static int format_call_Justification_output_(struct format_justification *just)
{
	addr pos;
	size_t index, i;
	struct format_operator *str;

	just->print->loop = 1;
	str = just->str;
	index = 0;
	/* prefix */
	if (str->colon || (just->size == 1 && (str->colon || str->atsign == 0))) {
		Return(format_call_Justification_space_(just, index++));
	}
	/* body */
	for (i = 0; i < just->size; i++) {
		if (i) {
			Return(format_call_Justification_space_(just, index++));
		}
		Return(vector_get_(just->vector, i, &pos));
		Return(fmtprint_string_(just->print, pos));
	}
	/* suffix */
	if (str->atsign) {
		Return(format_call_Justification_space_(just, index++));
	}

	return 0;
}

static int format_call_Justification1_(struct format_justification *just)
{
	Return(format_call_Justification_vector(just));
	format_call_Justification_count(just);
	return format_call_Justification_output_(just);
}

static int format_call_Justification2_(struct format_justification *just)
{
	fmtprint print;
	addr prefix, stream;
	size_t *array, size, now;

	print = just->print;
	array = format_justification_array(just->str);
	size = *(array++);
	Check(size < 1, "size error");
	now = *(array++);
	size--;
	just->array = array;
	just->size = size;
	/* execute first */
	Return(format_call_Justificaion_prefix(just, now + print->now, &prefix));
	if (print->loop) {
		Return(format_call_Justification_vector(just));
	}
	else {
		just->size = 0;
		Return(format_call_Justification_vector1(just));
	}
	format_call_Justification_count(just);
	/* terminal */
	stream = print->stream;
	Return(getleft_stream_(stream, &now));
	if (just->width < now + just->mincol + just->count) {
		Return(fmtprint_string_(print, prefix));
	}
	/* output */
	return format_call_Justification_output_(just);
}

static int format_call_Justification(fmtprint print, struct format_operator *str)
{
	struct format_justification just;
	fixnum mincol, colinc, minpad;
	unicode padchar;

	Check(4 < str->args_size, "size error");
	Return(fmtint_default_(print, str, 0, &mincol, 0));
	Return(fmtint_default_(print, str, 1, &colinc, 1));
	Return(fmtint_default_(print, str, 2, &minpad, 0));
	Return(fmtchar_default_(print, str, 3, &padchar, ' '));
	if (mincol < 0) {
		return fmtargs_abort_(print, str, 0,
				"The parameter must be a positive integer.", NULL);
	}
	if (colinc < 1) {
		return fmtargs_abort_(print, str, 1,
				"The parameter must be greater than 1.", NULL);
	}
	if (minpad < 0) {
		return fmtargs_abort_(print, str, 2,
				"The parameter must be a positive integer.", NULL);
	}

	cleartype(just);
	just.ptr = print->ptr;
	just.local = print->local;
	just.print = print;
	just.str = str;
	just.mincol = (size_t)mincol;
	just.colinc = (size_t)colinc;
	just.minpad = (size_t)minpad;
	just.padchar = padchar;
	just.second = str->option_check;

	if (str->option_check == 0)
		return format_call_Justification1_(&just);
	else
		return format_call_Justification2_(&just);
}


/*
 *  LogicalBlock
 */
struct format_pretty_struct {
	struct fmtprint_struct print;
	size_t index;
};

enum FormatPrettyIndex {
	FormatPrettyIndex_Stream,
	FormatPrettyIndex_Root,
	FormatPrettyIndex_Front,
	FormatPrettyIndex_Size
};

static void set_format_pretty(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPSYSTEM_FORMAT_PRETTY);
	SetArraySS(pos, index, value);
}
static void get_format_pretty(addr pos, size_t index, addr *ret)
{
	CheckType(pos, LISPSYSTEM_FORMAT_PRETTY);
	GetArraySS(pos, index, ret);
}
static void *pointer_format_pretty(addr pos)
{
	CheckType(pos, LISPSYSTEM_FORMAT_PRETTY);
	return (void *)PtrBodySS(pos);
}

static void getstream_format_pretty(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_FORMAT_PRETTY);
	get_format_pretty(pos, FormatPrettyIndex_Stream, ret);
}
static void setstream_format_pretty(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_FORMAT_PRETTY);
	set_format_pretty(pos, FormatPrettyIndex_Stream, value);
}

static void getroot_format_pretty(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_FORMAT_PRETTY);
	get_format_pretty(pos, FormatPrettyIndex_Root, ret);
}
static void setroot_format_pretty(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_FORMAT_PRETTY);
	set_format_pretty(pos, FormatPrettyIndex_Root, value);
}

static void getfront_format_pretty(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_FORMAT_PRETTY);
	get_format_pretty(pos, FormatPrettyIndex_Front, ret);
}
static void setfront_format_pretty(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_FORMAT_PRETTY);
	set_format_pretty(pos, FormatPrettyIndex_Front, value);
}

static struct format_pretty_struct *struct_format_pretty(addr pos)
{
	CheckType(pos, LISPSYSTEM_FORMAT_PRETTY);
	return (struct format_pretty_struct *)pointer_format_pretty(pos);
}
static fmtprint fmtprint_format_pretty(addr pos)
{
	CheckType(pos, LISPSYSTEM_FORMAT_PRETTY);
	return (fmtprint)&(struct_format_pretty(pos)->print);
}

static void read_format_pretty(addr pos, fmtprint print, struct fmtstack *rest)
{
	addr root, front;
	struct format_pretty_struct *str;

	/* structure */
	str = struct_format_pretty(pos);
	*print = str->print;
	print->rest = rest;

	/* list */
	getroot_format_pretty(pos, &root);
	getfront_format_pretty(pos, &front);
	rest->root = root;
	rest->front = front;
	rest->index = str->index;
}

static void format_pretty_heap(addr *ret, addr stream, struct fmtstack *rest)
{
	addr pos;
	struct format_pretty_struct *str;

	heap_smallsize(&pos, LISPSYSTEM_FORMAT_PRETTY,
			FormatPrettyIndex_Size,
			sizeoft(struct format_pretty_struct));
	str = struct_format_pretty(pos);
#ifdef LISP_DEBUG
	aamemory(str, sizeoft(struct format_pretty_struct));
#endif
	setstream_format_pretty(pos, stream);
	setroot_format_pretty(pos, rest->root);
	setfront_format_pretty(pos, rest->front);
	str->index = rest->index;
	*ret = pos;
}

static int format_logicalblock2(Execute ptr)
{
	addr pos, stream;
	struct fmtprint_struct print;
	struct fmtstack rest;

	getdata_control(ptr, &pos);
	getstream_format_pretty(pos, &stream);
	Return(check_pretty_stream(ptr, stream));
	read_format_pretty(pos, &print, &rest);
	return fmtcall(&print, NULL);
}

static int format_logicalblock1_call_(Execute ptr, addr pretty, addr stream)
{
	addr gensym;

	Return(gensym_pretty_stream_(stream, &gensym));
	(void)catch_clang(ptr, p_format_logicalblock2, gensym, pretty);
	return close_pretty_stream_unwind_protect_(ptr, stream);
}

static int format_logicalblock1(Execute ptr)
{
	addr pretty, stream, control;

	/* stream */
	getdata_control(ptr, &pretty);
	getstream_format_pretty(pretty, &stream);
	Check(! pretty_stream_p(stream), "type error");
	/* unwind-protect */
	push_control(ptr, &control);
	(void)format_logicalblock1_call_(ptr, pretty, stream);
	return pop_control_(ptr, control);
}

static int format_make_strvect_alloc_(LocalRoot local,
		addr *value, byte *ptr, size_t *ret)
{
	unicode *data;
	size_t size;

	size = *(size_t *)ptr;
	data = (unicode *)(ptr + IdxSize);
	Return(strvect_sizeu_alloc_(local, value, data, size));

	return Result(ret, IdxSize + size * sizeoft(unicode));
}
static int format_make_strvect_heap_(addr *value, byte *ptr, size_t *ret)
{
	return format_make_strvect_alloc_(NULL, value, ptr, ret);
}

static int format_call_LogicalBlock_prefix_(struct format_operator *str,
		addr *rprefix, addr *rperline, addr *rsuffix, size_t *ret)
{
	byte *body;
	addr prefix, perline, suffix;
	size_t now, plus;

	/* Initial value */
	now = sizeoft(struct format_operator);
	if (str->colon) {
		strvect_char_heap(&prefix, "(");
		strvect_char_heap(&suffix, ")");
		perline = Nil;
	}
	else {
		prefix = perline = suffix = Nil;
	}

	/* :prefix, :per-line-prefix */
	body = (byte *)str;
	if (str->prefix) {
		Return(format_make_strvect_heap_(&prefix, body + now, &plus));
		now += plus;
		/* perline */
		if (str->option_check) {
			perline = prefix;
			prefix = Nil;
		}
	}

	/* :suffix */
	if (str->suffix) {
		Return(format_make_strvect_heap_(&suffix, body + now, &plus));
		now += plus;
	}

	/* result */
	*rprefix = prefix;
	*rperline = perline;
	*rsuffix = suffix;
	*ret = now;
	return 0;
}

static int format_call_LogicalBlock_lambda_(
		fmtprint print, struct format_operator *str, addr pos,
		addr *rstream, addr *rlambda)
{
	addr data, prefix, perline, suffix, stream, lambda;
	LocalRoot local;
	Execute ptr;
	size_t now;
	fmtprint save;

	/* make-pprint-stream */
	ptr = print->ptr;
	local = ptr->local;
	Return(format_call_LogicalBlock_prefix_(str, &prefix, &perline, &suffix, &now));
	gchold_pushva_local(local, prefix, perline, suffix, NULL);

	/* object */
	Return(open_pretty_stream_(ptr,
				&stream, print->stream, pos, prefix, perline, suffix));
	format_pretty_heap(&data, stream, print->rest);
	save = fmtprint_format_pretty(data);
	*save = *print;
	save->stream = stream;
	save->now += now;
	save->pretty = 1;
	save->fill = str->close_atsign;
	save->rest = NULL;

	/* function */
	compiled_heap(&lambda, Nil);
	setcompiled_empty(lambda, p_format_logicalblock1);
	SetDataFunction(lambda, data);
	gchold_push_local(local, lambda);

	/* result */
	*rstream = stream;
	*rlambda = lambda;
	return 0;
}

static int format_call_LogicalBlock_call1_(fmtprint print, struct format_operator *str)
{
	addr pos, stream;
	struct fmtstack *rest, args;

	/* argument */
	Return(fmtprint_pop_(print, str, &pos));
	copyheap(&pos, pos);
	rest = print->rest;
	print->rest = &args;
	args.root = pos;
	args.front = pos;
	args.index = 0;
	Return(format_call_LogicalBlock_lambda_(print, str, pos, &stream, &pos));

	/* call */
	Return(call_pretty_stream(print->ptr, stream, pos));

	/* rest */
	print->rest = rest;

	return 0;
}

static int format_call_LogicalBlock_call2_(fmtprint print, struct format_operator *str)
{
	addr pos, stream;
	struct fmtstack *rest;
	size_t index;

	/* argument */
	rest = print->rest;
	index = rest->index;
	copyheap(&(rest->root), rest->root);
	rest->front = rest->root;
	rest->index = 0;
	Return(fmtprint_absolute_(print, str, index));
	Return(format_call_LogicalBlock_lambda_(print, str, rest->front, &stream, &pos));

	/* call */
	Return(call_pretty_stream(print->ptr, stream, pos));

	/* atsign */
	return fmtprint_clear_(print);
}

static int format_call_LogicalBlock_call_(fmtprint print, struct format_operator *str)
{
	if (str->atsign == 0)
		return format_call_LogicalBlock_call1_(print, str);
	else
		return format_call_LogicalBlock_call2_(print, str);
}

static int format_call_LogicalBlock(fmtprint print, struct format_operator *str)
{
	Execute ptr;
	addr control;

	Check(0 < str->args_size, "size error");
	ptr = print->ptr;
	push_control(ptr, &control);
	(void)format_call_LogicalBlock_call_(print, str);
	return pop_control_(ptr, control);
}


/*
 *  EscapeUpward
 */
static int format_call_EscapeUpward(fmtprint print, struct format_operator *str)
{
	int ex1, ex2, ex3;
	fixnum v1, v2, v3;

	Check(3 < str->args_size, "size error");
	Check(str->atsign, "atsign error");
	v1 = v2 = v3 = 0;
	Return(fmtint_nilp_(print, str, 0, &v1, &ex1));
	Return(fmtint_nilp_(print, str, 1, &v2, &ex2));
	Return(fmtint_nilp_(print, str, 2, &v3, &ex3));
	ex1 = ! ex1;
	ex2 = ! ex2;
	ex3 = ! ex3;

	if (ex1 && ex2 && ex3) {
		if (v1 <= v2 && v2 <= v3)
			goto break_outside;
		return 0;
	}
	if (ex1 && ex2) {
		if (v1 == v2)
			goto break_outside;
		return 0;
	}
	if (ex1) {
		if (v1 == 0)
			goto break_outside;
		return 0;
	}
	else if (str->colon) {
		if (print->last)
			goto break_outside;
		return 0;
	}
	else {
		if (print->rest->front == Nil)
			goto break_outside;
		return 0;
	}

break_outside:
	if (print->escape == 0 && print->pretty)
		return pprint_throw(print->ptr, print->stream);
	print->loop = 0;
	print->loop_colon = str->colon;

	return 0;
}


/*
 *  ClauseSeparator
 */
static int format_call_ClauseSeparator(fmtprint print, struct format_operator *str)
{
	return fmtprop_abort_(print, str, "Invalid ~~; operator.", NULL);
}


/*
 *  CallFunction
 */
static int format_call_CallFunction_object(fmtprint print,
		struct format_operator *str, size_t index, addr *ret)
{
	fixnum value;
	struct format_argument *arg;
	LocalRoot local;

	local = print->local;
	arg = format_getargs(str, index);
	switch (arg->type) {
		case fmtargs_nil:
			*ret = Nil;
			break;

		case fmtargs_integer:
			fixnum_local(local, ret, arg->u.value);
			break;

		case fmtargs_character:
			character_local(local, ret, arg->u.character);
			break;

		case fmtargs_argument:
			return fmtprint_pop_(print, str, ret);

		case fmtargs_count:
			Return(fmtint_count_(print, &value));
			fixnum_local(local, ret, value);
			break;

		default:
			return fmtprop_abort_(print, str, "Invalid format parameter.", NULL);
	}

	return 0;
}

static int format_call_CallFunction_call_(fmtprint print,
		struct format_operator *str, addr *ret)
{
	byte *ptr;
	addr package, name;
	size_t size;

	ptr = (byte *)format_getargs(str, str->args_size);
	Return(format_make_strvect_heap_(&package, ptr, &size));
	ptr += size;
	Return(format_make_strvect_heap_(&name, ptr, &size));
	return intern_package_(package, name, ret, NULL);
}

static int format_call_CallFunction(fmtprint print, struct format_operator *str)
{
	addr pos, root;
	LocalRoot local;
	LocalStack stack;
	size_t size, i;

	/* (symbol stream pos colon atsign ...) */
	local = print->local;
	Return(fmtprint_pop_(print, str, &pos));
	/* list */
	push_local(local, &stack);
	root = Nil;
	cons_local(local, &root, print->stream, root);
	cons_local(local, &root, pos, root);
	cons_local(local, &root, str->colon? T: Nil, root);
	cons_local(local, &root, str->atsign? T: Nil, root);
	/* arguments */
	size = str->args_size;
	for (i = 0; i < size; i++) {
		Return(format_call_CallFunction_object(print, str, i, &pos));
		copyheap(&pos, pos);
		cons_local(local, &root, pos, root);
	}
	nreverse(&root, root);
	/* call */
	Return(format_call_CallFunction_call_(print, str, &pos));
	Return(apply1_control_(print->ptr, &pos, pos, root));
	rollback_local(local, stack);

	return 0;
}


/*
 *  parse-format
 */
static int fmtcall_end(fmtprint print, enum FormatType type)
{
	return print->loop == 0
		|| type == FormatType_End
		|| type == FormatType_ClauseSeparator;
}

static int fmtcall(fmtprint print, int *loop)
{
	enum FormatType type;
	addr stream, backup_string;
	struct format_operator *str;
	fmtcalltype call;

	/* string-stream */
	Return(fmtprint_make_string_(print, &stream, &backup_string));
	/* loop */
	str = fmtprint_operator(print);
	type = str->type;
	while (! fmtcall_end(print, type)) {
		/* delete-space */
		if (type != FormatType_Output)
			print->delete_space = 0;
		/* call */
		call = FormatCallTable[type];
		Check(call == NULL, "Invalid format type.");
		Return((*call)(print, str));
		/* next */
		print->now += str->size;
		str = fmtprint_operator(print);
		type = str->type;
	}
	if (loop)
		*loop = print->loop_colon;
	/* close */
	close_output_string_stream(stream);
	print->string = backup_string;

	return 0;
}

static int format_execute_call(Execute ptr,
		addr stream, addr format, struct fmtstack *args)
{
	int check;
	struct fmtprint_struct print;

	fmtprint_make(&print, ptr, stream, format);
	print.rest = args;
	fmtprint_format_forward(&print);

	return fmtcall(&print, &check);
}

static int format_execute_format(Execute ptr,
		addr stream, addr format, addr args, addr *ret)
{
	struct fmtstack stack;

	CheckType(stream, LISPTYPE_STREAM);
	CheckType(format, LISPTYPE_FORMAT);
	Check(! listp(args), "type error");
	stack.root = args;
	stack.front = args;
	stack.index = 0;
	Return(format_execute_call(ptr, stream, format, &stack));
	*ret = stack.front;

	return 0;
}

static int format_execute_function(Execute ptr,
		addr stream, addr format, addr args, addr *ret)
{
	cons_local(ptr->local, &args, stream, args);
	return apply1_control_(ptr, ret, format, args);
}

static int format_execute_type(Execute ptr,
		addr stream, addr format, addr args, addr *ret)
{
	if (functionp(format))
		return format_execute_function(ptr, stream, format, args, ret);
	if (formatp(format))
		return format_execute_format(ptr, stream, format, args, ret);
	if (stringp(format)) {
		Return(format_parse_local_(ptr->local, &format, format));
		return format_execute_format(ptr, stream, format, args, ret);
	}
	return fmte_("Invalid control-string ~S.", format, NULL);
}

int format_execute(Execute ptr, addr stream, addr format, addr args, addr *ret)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	gchold_pushva_force_local(local, stream, format, args, NULL);
	Return(format_execute_type(ptr, stream, format, args, &args));
	rollback_local(local, stack);
	Return(exitpoint_stream_(stream));
	copyheap(ret, args);

	return 0;
}


/*
 *  initialize
 */
#define SetFormatCallTable(x) (FormatCallTable[FormatType_##x] = format_call_##x)
void init_format_function(void)
{
	cleartype(FormatCallTable);
	SetFormatCallTable(Error);
	SetFormatCallTable(End);
	SetFormatCallTable(Format);
	SetFormatCallTable(Output);
	SetFormatCallTable(Aesthetic);
	SetFormatCallTable(Standard);
	SetFormatCallTable(Binary);
	SetFormatCallTable(Octal);
	SetFormatCallTable(Decimal);
	SetFormatCallTable(Hexadecimal);
	SetFormatCallTable(Radix);
	SetFormatCallTable(RadixText);
	SetFormatCallTable(Plural);
	SetFormatCallTable(Character);
	SetFormatCallTable(Fixed);
	SetFormatCallTable(Exponential);
	SetFormatCallTable(General);
	SetFormatCallTable(Monetary);
	SetFormatCallTable(Newline);
	SetFormatCallTable(FreshLine);
	SetFormatCallTable(Page);
	SetFormatCallTable(Tilde);
	SetFormatCallTable(IgnoredNewline);
	SetFormatCallTable(Tabulate);
	SetFormatCallTable(GoTo);
	SetFormatCallTable(Recursive);
	SetFormatCallTable(ConditionalNewline);
	SetFormatCallTable(Write);
	SetFormatCallTable(Indent);
	SetFormatCallTable(Case);
	SetFormatCallTable(Condition);
	SetFormatCallTable(Iteration);
	SetFormatCallTable(Justification);
	SetFormatCallTable(LogicalBlock);
	SetFormatCallTable(EscapeUpward);
	SetFormatCallTable(ClauseSeparator);
	SetFormatCallTable(CallFunction);

	/* LogicalBlock */
	SetPointerType(empty, format_logicalblock1);
	SetPointerType(empty, format_logicalblock2);
}

