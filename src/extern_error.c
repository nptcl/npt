#include <stdarg.h>
#include "cons.h"
#include "constant.h"
#include "execute.h"
#include "extern_init.h"
#include "format.h"
#include "stream.h"
#include "strvect.h"
#include "typedef.h"

/*
 *  lisp_abort
 */
void lisp_exit_error(void)
{
	abortthis();
}

void lisp_abort(const char *fmt, ...)
{
	int check;
	addr format, list, stream;
	va_list va;
	Execute ptr;

	/* format */
	strvect_char_heap(&format, fmt);

	/* list */
	va_start(va, fmt);
	list_stdarg_alloc(NULL, &list, va);
	va_end(va);

	/* format */
	ptr = Execute_Thread;
	error_output_stream(ptr, &stream);
	check = fresh_line_stream_(stream, NULL);
	check |= format_stream_lisp(ptr, stream, format, list);
	if (check) {
		lisperror("lisp_abort error: %s.", fmt);
		lisp_exit_error();
		return;
	}

	/* abort */
	lisp_exit_error();
}

void lisp_abort_type(addr value, constindex index)
{
	addr type;

	GetConstant(index, &type);
	lisp_abort("type error: ~S must be a ~A type.", value, type, NULL);
}

