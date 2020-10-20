#include <stdarg.h>
#include "condition.h"
#include "extern_control.h"
#include "extern_error.h"
#include "extern_sequence.h"
#include "extern_stream.h"
#include "format.h"
#include "integer.h"
#include "hold.h"
#include "stream.h"
#include "stream_error.h"
#include "typedef.h"

/*
 *  format
 */
static int lisp_format_call_(addr stream, addr format, addr args)
{
	addr control;
	Execute ptr;
	LocalHold hold;

	if (stream == NULL)
		stream = T;
	ptr = Execute_Thread;
	lisp_push_control(&control);
	hold = LocalHold_local(ptr);
	localhold_pushva_force(hold, stream, format, args, NULL);
	Return(format_lisp(ptr, stream, format, args, &args));
	localhold_end(hold);

	return lisp_pop_control_(control);
}

int lisp_format8_(addr stream, const void *str, ...)
{
	addr format, args;
	va_list va;

	hold_value(stream, &stream);
	Return(lisp0_string8_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return lisp_format_call_(stream, format, args);
}

int lisp_format16_(addr stream, const void *str, ...)
{
	addr format, args;
	va_list va;

	hold_value(stream, &stream);
	Return(lisp0_string16_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return lisp_format_call_(stream, format, args);
}

int lisp_format32_(addr stream, const void *str, ...)
{
	addr format, args;
	va_list va;

	hold_value(stream, &stream);
	Return(lisp0_string32_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return lisp_format_call_(stream, format, args);
}


/*
 *  stdout
 */
int lisp_stdout8_(const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string8_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return lisp_format_call_(NULL, format, args);
}

int lisp_stdout16_(const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string16_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return lisp_format_call_(NULL, format, args);
}

int lisp_stdout32_(const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string32_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return lisp_format_call_(NULL, format, args);
}


/*
 *  stderr
 */
int lisp_stderr8_(const void *str, ...)
{
	addr format, args, stream;
	va_list va;

	Return(lisp0_string8_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	Return(error_output_stream_(Execute_Thread, &stream));
	return lisp_format_call_(stream, format, args);
}

int lisp_stderr16_(const void *str, ...)
{
	addr format, args, stream;
	va_list va;

	Return(lisp0_string16_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	Return(error_output_stream_(Execute_Thread, &stream));
	return lisp_format_call_(NULL, format, args);
}

int lisp_stderr32_(const void *str, ...)
{
	addr format, args, stream;
	va_list va;

	Return(lisp0_string32_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	Return(error_output_stream_(Execute_Thread, &stream));
	return lisp_format_call_(NULL, format, args);
}


/*
 *  stringf
 */
static int lisp_format_string_call_(addr format, addr args, addr *ret)
{
	addr control;
	Execute ptr;
	LocalHold hold;

	ptr = Execute_Thread;
	lisp_push_control(&control);
	hold = LocalHold_local(ptr);
	localhold_pushva_force(hold, format, args, NULL);
	Return(format_lisp(ptr, Nil, format, args, ret));
	localhold_end(hold);

	return lisp_pop_control_(control);
}

int lisp0_stringf8_(addr *ret, const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string8_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return lisp_format_string_call_(format, args, ret);
}

int lisp0_stringf16_(addr *ret, const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string16_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return lisp_format_string_call_(format, args, ret);
}

int lisp0_stringf32_(addr *ret, const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string32_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return lisp_format_string_call_(format, args, ret);
}

int lisp_stringf8_(addr x, const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string8_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	Return(lisp_format_string_call_(format, args, &args));
	hold_set(x, args);
	return 0;
}

int lisp_stringf16_(addr x, const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string16_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	Return(lisp_format_string_call_(format, args, &args));
	hold_set(x, args);
	return 0;
}

int lisp_stringf32_(addr x, const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string32_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	Return(lisp_format_string_call_(format, args, &args));
	hold_set(x, args);
	return 0;
}

