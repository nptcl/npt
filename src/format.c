#include <stdarg.h>
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "execute.h"
#include "format.h"
#include "format_function.h"
#include "format_parse.h"
#include "gc.h"
#include "local.h"
#include "stream.h"
#include "stream_string.h"
#include "strtype.h"
#include "strvect.h"

/*****************************************************************************
 *  format
 *****************************************************************************/
/*
 * ~args opt operator
 *
 * arg -> [+-]?[0-9]+ | '. | [vV] | #
 * args -> (arg (, arg)*)?
 * opt -> : | @ | :@ | @:
 * operator -> aAsSdDbBoOxXrRpPcCfFeEgG$%&|~\ntT*?_wWiI()[];{}<>^
 *
 * vV		argument
 * #		count
 *
 * aA		Aesthetic
 * sS		Standard
 * dD		Decimal
 * bB		Binary
 * oO		Octal
 * xX		Hexadecimal
 * rR		Radix
 * pP		Plural
 * cC		Character
 * fF		Fixed-format floating-point
 * eE		Exponential floating-point
 * gG		General floating-point
 * $		Monetary floating-point
 * %		Newline
 * &		Fresh-Line
 * |		Page
 * ~		Tilde
 * \n		Ignored Newline
 * tT		Tabulate
 * *		Go-To
 * ?		Recursive Processing
 * _		Conditional Newline
 * wW		Write
 * iI		Indent
 * ()		Case Conversion
 * []		Conditional Expression
 * ;		Clause Separator
 * {}		Iteration
 * <>		Justification, Logical Block
 * ^		Escape Upward
 */
int format_stream_lisp_(Execute ptr, addr stream, addr format, addr args)
{
	return format_execute_(ptr, stream, format, args, &args);
}

int format_string_lisp_(Execute ptr, addr format, addr args, addr *ret)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	if (format_stream_lisp_(ptr, stream, format, args)) {
		close_output_string_stream(stream);
		return 1;
	}
	Return(string_stream_heap_(stream, ret));
	close_output_string_stream(stream);

	return 0;
}

static int format_array_lisp(Execute ptr, addr array, addr format, addr args, addr *ret)
{
	int result;
	addr stream;

	open_extend_output_stream(&stream, array);
	result = format_stream_lisp_(ptr, stream, format, args);
	close_output_string_stream(stream);
	*ret = array;

	return result;
}

int format_lisp_(Execute ptr, addr stream, addr format, addr args, addr *ret)
{
	if (stream == Nil)
		return format_string_lisp_(ptr, format, args, ret);

	if (stream == T) {
		Return(standard_output_stream_(ptr, &stream));
		return format_stream_lisp_(ptr, stream, format, args);
	}

	if (stringp(stream))
		return format_array_lisp(ptr, stream, format, args, ret);

	return format_stream_lisp_(ptr, stream, format, args);
}


/*
 *  format clang
 */
static int format_stdarg_(Execute ptr,
		addr stream, const char *str, va_list args, addr *ret)
{
	addr format, list;
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	strvect_char_local(local, &format, str);
	list_stdarg_alloc(local, &list, args);
	if (format_lisp_(ptr, stream, format, list, ret))
		return 1;
	rollback_local(local, stack);

	return 0;
}

int format_stream_(Execute ptr, addr stream, const char *str, ...)
{
	int check;
	va_list args;

	va_start(args, str);
	check = format_stdarg_(ptr, stream, str, args, NULL);
	va_end(args);

	return check;
}

int format_string_(Execute ptr, addr *ret, const char *str, ...)
{
	int check;
	va_list args;

	va_start(args, str);
	check = format_stdarg_(ptr, Nil, str, args, ret);
	va_end(args);

	return check;
}

int format_stdout_(Execute ptr, const char *str, ...)
{
	int check;
	va_list args;

	va_start(args, str);
	check = format_stdarg_(ptr, T, str, args, NULL);
	va_end(args);

	return check;
}

void formatf(const char *str, ...)
{
	int check;
	va_list args;

	va_start(args, str);
	check = format_stdarg_(Execute_Thread, T, str, args, NULL);
	va_end(args);
	if (check) {
		Abort("format error.");
	}
}


/*
 *  initialize
 */
void init_format(void)
{
	init_format_parse();
	init_format_function();
}

