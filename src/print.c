#include "bignum.h"
#include "character.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "control_execute.h"
#include "control_object.h"
#include "heap.h"
#include "integer.h"
#include "print.h"
#include "print_dispatch.h"
#include "print_function.h"
#include "print_object.h"
#include "print_write.h"
#include "reader.h"
#include "stream.h"
#include "stream_string.h"
#include "symbol.h"
#include "type_object.h"
#include "type_value.h"

/*
 *  special variable
 */
static int getbool_print(Execute ptr, constindex index)
{
	addr symbol;
	GetConstant(index, &symbol);
	getspecialcheck_local(ptr, symbol, &symbol);
	return symbol != Nil;
}

static void pushbool_print(Execute ptr, constindex index, int value)
{
	addr symbol;
	GetConstant(index, &symbol);
	pushspecial_control(ptr, symbol, value? T: Nil);
}

/* print-array */
_g int array_print(Execute ptr)
{
	return getbool_print(ptr, CONSTANT_SPECIAL_PRINT_ARRAY);
}

_g void push_array_print(Execute ptr, int value)
{
	pushbool_print(ptr, CONSTANT_SPECIAL_PRINT_ARRAY, value);
}

/* print-base */
_g unsigned base_print(Execute ptr)
{
	addr pos;
	fixnum value;

	GetConst(SPECIAL_PRINT_BASE, &pos);
	getspecialcheck_local(ptr, pos, &pos);
	if (! fixnump(pos))
		TypeError(pos, FIXNUM);
	GetFixnum(pos, &value);
	if (! isBaseChar(value))
		fmte("The value ~S must be a number between 2 and 36.", pos, NULL);

	return (unsigned)value;
}

_g void push_base_print(Execute ptr, unsigned base)
{
	addr pos, value;

	Check(! isBaseChar(base), "base value error");
	fixnum_heap(&value, (fixnum)base);
	GetConst(SPECIAL_PRINT_BASE, &pos);
	pushspecial_control(ptr, pos, value);
}

/* print-radix */
_g int radix_print(Execute ptr)
{
	return getbool_print(ptr, CONSTANT_SPECIAL_PRINT_RADIX);
}

_g void push_radix_print(Execute ptr, int value)
{
	pushbool_print(ptr, CONSTANT_SPECIAL_PRINT_RADIX, value);
}

/* print-case */
_g enum PrintCase case_print(Execute ptr)
{
	addr pos, value;

	/* special */
	GetConst(SPECIAL_PRINT_CASE, &pos);
	getspecialcheck_local(ptr, pos, &pos);
	/* upcase */
	GetConst(KEYWORD_UPCASE, &value);
	if (pos == value)
		return PrintCase_upcase;
	/* downcase */
	GetConst(KEYWORD_DOWNCASE, &value);
	if (pos == value)
		return PrintCase_downcase;
	/* capitalize */
	GetConst(KEYWORD_CAPITALIZE, &value);
	if (pos == value)
		return PrintCase_capitalize;
	/* error */
	fmte("type error", NULL);
	return PrintCase_unread;
}

_g void push_case_print(Execute ptr, enum PrintCase pcase)
{
	addr pos, value;

	switch (pcase) {
		case PrintCase_upcase:
			GetConst(KEYWORD_UPCASE, &value);
			break;

		case PrintCase_downcase:
			GetConst(KEYWORD_DOWNCASE, &value);
			break;

		case PrintCase_capitalize:
			GetConst(KEYWORD_CAPITALIZE, &value);
			break;

		default:
			fmte("type error", NULL);
			return;
	}
	GetConst(SPECIAL_PRINT_CASE, &pos);
	pushspecial_control(ptr, pos, value);
}

/* print-circle */
_g int circle_print(Execute ptr)
{
	return getbool_print(ptr, CONSTANT_SPECIAL_PRINT_CIRCLE);
}

_g void push_circle_print(Execute ptr, int value)
{
	pushbool_print(ptr, CONSTANT_SPECIAL_PRINT_CIRCLE, value);
}

/* print-escape */
_g int escape_print(Execute ptr)
{
	return getbool_print(ptr, CONSTANT_SPECIAL_PRINT_ESCAPE);
}

_g void push_escape_print(Execute ptr, int value)
{
	pushbool_print(ptr, CONSTANT_SPECIAL_PRINT_ESCAPE, value);
}

/* print-gensym */
_g int gensym_print(Execute ptr)
{
	return getbool_print(ptr, CONSTANT_SPECIAL_PRINT_GENSYM);
}

_g void push_gensym_print(Execute ptr, int value)
{
	pushbool_print(ptr, CONSTANT_SPECIAL_PRINT_GENSYM, value);
}

/* print-readably */
_g int readably_print(Execute ptr)
{
	return getbool_print(ptr, CONSTANT_SPECIAL_PRINT_READABLY);
}

_g void push_readably_print(Execute ptr, int value)
{
	pushbool_print(ptr, CONSTANT_SPECIAL_PRINT_READABLY, value);
}

/* print-pretty */
_g int pretty_print(Execute ptr)
{
	return getbool_print(ptr, CONSTANT_SPECIAL_PRINT_PRETTY);
}

_g void push_pretty_print(Execute ptr, int value)
{
	pushbool_print(ptr, CONSTANT_SPECIAL_PRINT_PRETTY, value);
}

/* print-level */
static int getindex_print(Execute ptr, constindex index, size_t *ret)
{
	addr pos;

	/* special */
	GetConstant(index, &pos);
	getspecialcheck_local(ptr, pos, &pos);
	/* value */
	if (pos == Nil) {
		return 0;
	}
	else {
		getindex_fixnum(pos, ret);
		return 1;
	}
}

static void push_integer_print(Execute ptr, constindex index, size_t value)
{
	addr symbol, pos;

	GetConstant(index, &symbol);
	fixnum_index_heap(&pos, value);
	pushspecial_control(ptr, symbol, pos);
}

static void push_nil_print(Execute ptr, constindex index)
{
	addr pos;
	GetConstant(index, &pos);
	pushspecial_control(ptr, pos, Nil);
}

_g int level_print(Execute ptr, size_t *ret)
{
	return getindex_print(ptr, CONSTANT_SPECIAL_PRINT_LEVEL, ret);
}

_g void push_level_print(Execute ptr, size_t value)
{
	push_integer_print(ptr, CONSTANT_SPECIAL_PRINT_LEVEL, value);
}

_g void push_level_nil_print(Execute ptr)
{
	push_nil_print(ptr, CONSTANT_SPECIAL_PRINT_LEVEL);
}

/* print-length */
_g int length_print(Execute ptr, size_t *ret)
{
	return getindex_print(ptr, CONSTANT_SPECIAL_PRINT_LENGTH, ret);
}

_g void push_length_print(Execute ptr, size_t value)
{
	push_integer_print(ptr, CONSTANT_SPECIAL_PRINT_LENGTH, value);
}

_g void push_length_nil_print(Execute ptr)
{
	push_nil_print(ptr, CONSTANT_SPECIAL_PRINT_LENGTH);
}

/* print-lines */
_g int lines_print(Execute ptr, size_t *ret)
{
	return getindex_print(ptr, CONSTANT_SPECIAL_PRINT_LINES, ret);
}

_g void push_lines_print(Execute ptr, size_t value)
{
	push_integer_print(ptr, CONSTANT_SPECIAL_PRINT_LINES, value);
}

_g void push_lines_nil_print(Execute ptr)
{
	push_nil_print(ptr, CONSTANT_SPECIAL_PRINT_LINES);
}

/* print-miser-width */
_g int miser_width_print(Execute ptr, size_t *ret)
{
	return getindex_print(ptr, CONSTANT_SPECIAL_PRINT_MISER_WIDTH, ret);
}

_g void push_miser_width_print(Execute ptr, size_t value)
{
	push_integer_print(ptr, CONSTANT_SPECIAL_PRINT_MISER_WIDTH, value);
}

_g void push_miser_width_nil_print(Execute ptr)
{
	push_nil_print(ptr, CONSTANT_SPECIAL_PRINT_MISER_WIDTH);
}

/* print-right-margin */
_g int right_margin_print_(Execute ptr, addr stream, size_t *ret)
{
	int check;
	addr pos;
	size_t size;

	GetConst(SPECIAL_PRINT_RIGHT_MARGIN, &pos);
	getspecialcheck_local(ptr, pos, &pos);
	if (pos == Nil) {
		Return(termsize_stream_(stream, &size, &check));
		if (check)
			size = PRINT_DEFAULT_WIDTH;
		*ret = size;
	}
	else {
		getindex_fixnum(pos, ret);
	}

	return 0;
}

_g void push_right_margin_print(Execute ptr, size_t value)
{
	push_integer_print(ptr, CONSTANT_SPECIAL_PRINT_RIGHT_MARGIN, value);
}

_g void push_right_margin_nil_print(Execute ptr)
{
	push_nil_print(ptr, CONSTANT_SPECIAL_PRINT_RIGHT_MARGIN);
}

/* print-dispatch */
_g void pprint_dispatch_print(Execute ptr, addr *ret)
{
	addr pos;
	GetConst(SPECIAL_PRINT_PPRINT_DISPATCH, &pos);
	getspecialcheck_local(ptr, pos, ret);
}

_g void push_pprint_dispatch(Execute ptr, addr value)
{
	addr pos;
	GetConst(SPECIAL_PRINT_PPRINT_DISPATCH, &pos);
	pushspecial_control(ptr, pos, value);
}


/*
 *  print-unreadable-object
 */
static int print_unreadable_call_(Execute ptr, addr stream, addr pos,
		int type, int identity, calltype_print call, addr body)
{
	char buffer[32];
	int first;
	addr value;

	/* begin */
	first = 1;
	Return(print_ascii_stream_(stream, "#<"));
	/* type */
	if (type) {
		type_value(&value, pos);
		type_object(&value, value);
		if (princ_print(ptr, stream, value))
			return 1;
		first = 0;
	}
	/* call */
	if (call) {
		if (first == 0) {
			Return(write_char_stream_(stream, ' '));
		}
		if ((*call)(ptr, stream, pos))
			return 1;
		first = 0;
	}
	/* body */
	if (body) {
		if (first == 0) {
			Return(write_char_stream_(stream, ' '));
		}
		if (callclang_apply(ptr, &body, body, Nil))
			return 1;
		first = 0;
	}
	/* identity */
	if (identity) {
		if (first == 0) {
			Return(write_char_stream_(stream, ' '));
		}
		snprintf(buffer, 32, "#x%zx", (size_t)pos);
		Return(print_ascii_stream_(stream, buffer));
	}
	/* end */
	return write_char_stream_(stream, '>');
}

_g int print_unreadable_object_(Execute ptr, addr stream, addr pos,
		int type, int identity, calltype_print call)
{
	return print_unreadable_call_(ptr, stream, pos, type, identity, call, NULL);
}

_g int print_unreadable_common_(Execute ptr, addr stream, addr pos,
		int type, int identity, addr body)
{
	return print_unreadable_call_(ptr, stream, pos, type, identity, NULL, body);
}


/*
 *  initialize
 */
_g void build_print(Execute ptr)
{
	build_print_object(ptr);
	build_print_dispatch();
}

_g void init_print(void)
{
	init_print_function();
	init_print_object();
	init_print_write();
}

