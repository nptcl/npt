#include "condition.h"
#include "copy.h"
#include "terme_arch.h"
#include "terme_error.h"
#include "terme_input.h"
#include "terme_output.h"
#include "strvect.h"
#include "typedef.h"

/*
 *  error
 */
static int terme_fmte_va_(addr format, addr args)
{
	int mode, check;

	if (terme_arch_textmode(&mode)) {
		Abort("terme_arch_textmode error.");
		return 0;
	}
	check = call_simple_error_(NULL, format, args);
	if (mode && terme_arch_rawmode(NULL)) {
		Abort("terme_arch_rawmode error.");
		return 0;
	}

	return check;
}

int terme_fmte_(const char *str, ...)
{
	addr format, args;
	va_list va;

	if (terme_fresh_line()) {
		Abort("terme_fresh_line error.");
		return 0;
	}
	if (terme_finish_output()) {
		Abort("terme_finish_output error.");
		return 0;
	}
	strvect_char_heap(&format, str);
	va_start(va, str);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);
	return terme_fmte_va_(format, args);
}

