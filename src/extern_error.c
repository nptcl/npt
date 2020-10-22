#include <stdarg.h>
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "execute.h"
#include "extern_init.h"
#include "extern_sequence.h"
#include "format.h"
#include "hold.h"
#include "stream.h"
#include "stream_function.h"
#include "strvect.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  abort
 */
void lisp_abort(void)
{
	abort_execute();
}

void lisp_abortf(const char *fmt, ...)
{
	va_list args;

	va_start(args, fmt);
	lisperror_va(fmt, args);
	va_end(args);

	/* abort */
	lisp_abort();
}

static void lisp_abort_call(addr format, addr list)
{
	int check;
	addr stream;
	Execute ptr;

	ptr = Execute_Thread;
	GetConst(SPECIAL_ERROR_OUTPUT, &stream);
	getspecial_local(ptr, stream, &stream);
	if (stream == Unbound || (! streamp(stream))) {
		lisp_abortf("lisp_abort: Invalid error-output stream.");
		return;
	}
	/* output */
	check = fresh_line_stream_(stream, NULL);
	check |= format_stream_lisp(ptr, stream, format, list);
	if (check) {
		lisp_abortf("lisp_abort: format error.");
		return;
	}

	/* abort */
	lisp_abort();
}

void lisp_abort8(const void *fmt, ...)
{
	addr format, list;
	va_list va;

	if (lisp0_string8_(&format, fmt)) {
		lisp_abortf("Invalid unicode format");
		return;
	}
	va_start(va, fmt);
	lisp0_list_va(&list, va);
	va_end(va);

	lisp_abort_call(format, list);
}

void lisp_abort16(const void *fmt, ...)
{
	addr format, list;
	va_list va;

	if (lisp0_string16_(&format, fmt)) {
		lisp_abortf("Invalid unicode format");
		return;
	}
	va_start(va, fmt);
	lisp0_list_va(&list, va);
	va_end(va);

	lisp_abort_call(format, list);
}

void lisp_abort32(const void *fmt, ...)
{
	addr format, list;
	va_list va;

	if (lisp0_string32_(&format, fmt)) {
		lisp_abortf("Invalid unicode format");
		return;
	}
	va_start(va, fmt);
	lisp0_list_va(&list, va);
	va_end(va);

	lisp_abort_call(format, list);
}

lisp_abort_calltype lisp_set_abort_handler(lisp_abort_calltype call)
{
	return set_abort_handler(call);
}

lisp_abort_calltype lisp_set_abort_setjmp_handler(void)
{
	return set_abort_setjmp_handler();
}


/*
 *  signal
 */
int lisp_signal_(addr condition)
{
	hold_value(condition, &condition);
	return signal_function_(Execute_Thread, condition);
}

int lisp_error_(addr condition)
{
	hold_value(condition, &condition);
	return error_function_(Execute_Thread, condition);
}


/*
 *  error
 */
int lisp_error8_(const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string8_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return call_simple_error_(NULL, format, args);
}

int lisp_error16_(const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string16_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return call_simple_error_(NULL, format, args);
}

int lisp_error32_(const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string32_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return call_simple_error_(NULL, format, args);
}


/*
 *  warn
 */
int lisp_warn8_(const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string8_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return call_simple_warning_(NULL, format, args);
}

int lisp_warn16_(const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string16_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return call_simple_warning_(NULL, format, args);
}

int lisp_warn32_(const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string32_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return call_simple_warning_(NULL, format, args);
}

