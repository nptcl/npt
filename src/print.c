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
#include "stream_function.h"
#include "stream_string.h"
#include "symbol.h"
#include "type_object.h"
#include "type_value.h"

/*
 *  special variable
 */
static int getbool_print_(Execute ptr, int *ret, constindex index)
{
	addr pos;

	GetConstant(index, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));

	return Result(ret, pos != Nil);
}

static void pushbool_print(Execute ptr, constindex index, int value)
{
	addr symbol;
	GetConstant(index, &symbol);
	pushspecial_control(ptr, symbol, value? T: Nil);
}

/* print-array */
int array_print_(Execute ptr, int *ret)
{
	return getbool_print_(ptr, ret, CONSTANT_SPECIAL_PRINT_ARRAY);
}

void push_array_print(Execute ptr, int value)
{
	pushbool_print(ptr, CONSTANT_SPECIAL_PRINT_ARRAY, value);
}

/* print-base */
int base_print_(Execute ptr, unsigned *ret)
{
	addr pos;
	fixnum value;

	GetConst(SPECIAL_PRINT_BASE, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	if (! fixnump(pos))
		return TypeError_(pos, FIXNUM);
	GetFixnum(pos, &value);
	if (! isBaseChar(value))
		return fmte_("The value ~S must be a number between 2 and 36.", pos, NULL);

	return Result(ret, (unsigned)value);
}

void push_base_print(Execute ptr, unsigned base)
{
	addr pos, value;

	Check(! isBaseChar(base), "base value error");
	fixnum_heap(&value, (fixnum)base);
	GetConst(SPECIAL_PRINT_BASE, &pos);
	pushspecial_control(ptr, pos, value);
}

/* print-radix */
int radix_print_(Execute ptr, int *ret)
{
	return getbool_print_(ptr, ret, CONSTANT_SPECIAL_PRINT_RADIX);
}

void push_radix_print(Execute ptr, int value)
{
	pushbool_print(ptr, CONSTANT_SPECIAL_PRINT_RADIX, value);
}

/* print-case */
int case_print_(Execute ptr, enum PrintCase *ret)
{
	addr pos, value;

	/* special */
	GetConst(SPECIAL_PRINT_CASE, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	/* upcase */
	GetConst(KEYWORD_UPCASE, &value);
	if (pos == value)
		return Result(ret, PrintCase_upcase);
	/* downcase */
	GetConst(KEYWORD_DOWNCASE, &value);
	if (pos == value)
		return Result(ret, PrintCase_downcase);
	/* capitalize */
	GetConst(KEYWORD_CAPITALIZE, &value);
	if (pos == value)
		return Result(ret, PrintCase_capitalize);

	/* error */
	*ret = PrintCase_unread;
	return fmte_("type error", NULL);
}

int push_case_print_(Execute ptr, enum PrintCase pcase)
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
			return fmte_("type error", NULL);
	}
	GetConst(SPECIAL_PRINT_CASE, &pos);
	pushspecial_control(ptr, pos, value);
	return 0;
}

/* print-circle */
int circle_print_(Execute ptr, int *ret)
{
	return getbool_print_(ptr, ret, CONSTANT_SPECIAL_PRINT_CIRCLE);
}

void push_circle_print(Execute ptr, int value)
{
	pushbool_print(ptr, CONSTANT_SPECIAL_PRINT_CIRCLE, value);
}

/* print-escape */
int escape_print_(Execute ptr, int *ret)
{
	return getbool_print_(ptr, ret, CONSTANT_SPECIAL_PRINT_ESCAPE);
}

void push_escape_print(Execute ptr, int value)
{
	pushbool_print(ptr, CONSTANT_SPECIAL_PRINT_ESCAPE, value);
}

/* print-gensym */
int gensym_print_(Execute ptr, int *ret)
{
	return getbool_print_(ptr, ret, CONSTANT_SPECIAL_PRINT_GENSYM);
}

void push_gensym_print(Execute ptr, int value)
{
	pushbool_print(ptr, CONSTANT_SPECIAL_PRINT_GENSYM, value);
}

/* print-readably */
int readably_print_(Execute ptr, int *ret)
{
	return getbool_print_(ptr, ret, CONSTANT_SPECIAL_PRINT_READABLY);
}

void push_readably_print(Execute ptr, int value)
{
	pushbool_print(ptr, CONSTANT_SPECIAL_PRINT_READABLY, value);
}

/* print-pretty */
int pretty_print_(Execute ptr, int *ret)
{
	return getbool_print_(ptr, ret, CONSTANT_SPECIAL_PRINT_PRETTY);
}

void push_pretty_print(Execute ptr, int value)
{
	pushbool_print(ptr, CONSTANT_SPECIAL_PRINT_PRETTY, value);
}

/* print-level */
static int getindex_print_(Execute ptr, size_t *value, int *ret, constindex index)
{
	addr pos;

	/* special */
	GetConstant(index, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	/* value */
	if (pos == Nil) {
		*value = 0;
		return Result(ret, 0);
	}
	else {
		Return(getindex_fixnum_(pos, value));
		return Result(ret, 1);
	}
}

static int push_integer_print_(Execute ptr, constindex index, size_t value)
{
	addr symbol, pos;

	GetConstant(index, &symbol);
	Return(fixnum_index_heap_(&pos, value));
	pushspecial_control(ptr, symbol, pos);

	return 0;
}

static void push_nil_print(Execute ptr, constindex index)
{
	addr pos;
	GetConstant(index, &pos);
	pushspecial_control(ptr, pos, Nil);
}

int level_print_(Execute ptr, size_t *value, int *ret)
{
	return getindex_print_(ptr, value, ret, CONSTANT_SPECIAL_PRINT_LEVEL);
}

int push_level_print_(Execute ptr, size_t value)
{
	return push_integer_print_(ptr, CONSTANT_SPECIAL_PRINT_LEVEL, value);
}

void push_level_nil_print(Execute ptr)
{
	push_nil_print(ptr, CONSTANT_SPECIAL_PRINT_LEVEL);
}

/* print-length */
int length_print_(Execute ptr, size_t *value, int *ret)
{
	return getindex_print_(ptr, value, ret, CONSTANT_SPECIAL_PRINT_LENGTH);
}

int push_length_print_(Execute ptr, size_t value)
{
	return push_integer_print_(ptr, CONSTANT_SPECIAL_PRINT_LENGTH, value);
}

void push_length_nil_print(Execute ptr)
{
	push_nil_print(ptr, CONSTANT_SPECIAL_PRINT_LENGTH);
}

/* print-lines */
int lines_print_(Execute ptr, size_t *value, int *ret)
{
	return getindex_print_(ptr, value, ret, CONSTANT_SPECIAL_PRINT_LINES);
}

int push_lines_print_(Execute ptr, size_t value)
{
	return push_integer_print_(ptr, CONSTANT_SPECIAL_PRINT_LINES, value);
}

void push_lines_nil_print(Execute ptr)
{
	push_nil_print(ptr, CONSTANT_SPECIAL_PRINT_LINES);
}

/* print-miser-width */
int miser_width_print_(Execute ptr, size_t *value, int *ret)
{
	return getindex_print_(ptr, value, ret, CONSTANT_SPECIAL_PRINT_MISER_WIDTH);
}

int push_miser_width_print_(Execute ptr, size_t value)
{
	return push_integer_print_(ptr, CONSTANT_SPECIAL_PRINT_MISER_WIDTH, value);
}

void push_miser_width_nil_print(Execute ptr)
{
	push_nil_print(ptr, CONSTANT_SPECIAL_PRINT_MISER_WIDTH);
}

/* print-right-margin */
int right_margin_print_(Execute ptr, addr stream, size_t *ret)
{
	int check;
	addr pos;
	size_t size;

	GetConst(SPECIAL_PRINT_RIGHT_MARGIN, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	if (pos == Nil) {
		Return(termsize_stream_(stream, &size, &check));
		if (check)
			size = PRINT_DEFAULT_WIDTH;
		*ret = size;
	}
	else {
		Return(getindex_fixnum_(pos, ret));
	}

	return 0;
}

int push_right_margin_print_(Execute ptr, size_t value)
{
	return push_integer_print_(ptr, CONSTANT_SPECIAL_PRINT_RIGHT_MARGIN, value);
}

void push_right_margin_nil_print(Execute ptr)
{
	push_nil_print(ptr, CONSTANT_SPECIAL_PRINT_RIGHT_MARGIN);
}

/* print-dispatch */
int pprint_dispatch_print_(Execute ptr, addr *ret)
{
	addr pos;
	GetConst(SPECIAL_PRINT_PPRINT_DISPATCH, &pos);
	return getspecialcheck_local_(ptr, pos, ret);
}

void push_pprint_dispatch(Execute ptr, addr value)
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
		Return(type_value_(&value, pos));
		Return(type_object_(&value, value));
		Return(princ_print(ptr, stream, value));
		first = 0;
	}
	/* call */
	if (call) {
		if (first == 0) {
			Return(write_char_stream_(stream, ' '));
		}
		Return((*call)(ptr, stream, pos));
		first = 0;
	}
	/* body */
	if (body) {
		if (first == 0) {
			Return(write_char_stream_(stream, ' '));
		}
		Return(callclang_apply(ptr, &body, body, Nil));
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

int print_unreadable_object_(Execute ptr, addr stream, addr pos,
		int type, int identity, calltype_print call)
{
	return print_unreadable_call_(ptr, stream, pos, type, identity, call, NULL);
}

int print_unreadable_common_(Execute ptr, addr stream, addr pos,
		int type, int identity, addr body)
{
	return print_unreadable_call_(ptr, stream, pos, type, identity, NULL, body);
}


/*
 *  initialize
 */
void build_print(Execute ptr)
{
	Error(build_print_object_(ptr));
	Error(build_print_dispatch_());
}

void init_print(void)
{
	init_print_function();
	init_print_object();
	init_print_write();
}

