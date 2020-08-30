#include "define.h"

#ifdef LISP_DEGRADE
#include <stdio.h>
#include <stdarg.h>
#include "constant.h"
#include "control_object.h"
#include "degrade.h"
#include "execute.h"
#include "build.h"
#include "stream_broadcast.h"
#include "symbol.h"
#include "typedef.h"

void degrade_execute(void);
static FILE *file = NULL;
int DegradeCount = 0;
int DegradeError = 0;

#define DEGRADE_WIDTH 60
static int DegradeSwitch = 1;
static int DegradePosition;


/* degrade */
int degrade_code(void (*init)(Execute), int (*call)(void))
{
	int errorp, finish;
	lisp_abort_calltype handler;
	Execute ptr;

	freelisp();
	alloclisp(0, 0);

	lisp_info_enable = 1;
	ptr = Execute_Thread;
	errorp = 0;
	finish = 0;
	handler = set_degrade_setjmp_handler();
	Lisp_degrade_Begin {
		lisp_initialize = 1;
		if (init)
			(*init)(ptr);
		errorp = (*call)();
		finish = 1;
	}
	Lisp_degrade_End;
	(void)set_abort_handler(handler);
	freelisp();
	lisp_info_enable = 1;

	/* result */
	if (finish == 0) {
		errorp = 1;
	}

	return errorp;
}

int degrade_printf(const char *fmt, ...)
{
	int result;
	va_list args;

	va_start(args, fmt);
	result = vfprintf(file, fmt, args);
	va_end(args);
	fflush(file);

	return result;
}

int degrade_test(int check, const char *name)
{
	DegradeCount++;
	if (check) {
		if (DegradeSwitch) {
			degrade_printf(".");
			DegradePosition++;
			if (DEGRADE_WIDTH <= DegradePosition) {
				degrade_printf("\n");
				DegradePosition = 0;
			}
		}
		else {
			degrade_printf("[OK] %7d: %s\n", DegradeCount, name);
		}
		return 0;
	}
	else {
		if (DegradeSwitch) {
			if (DegradePosition != 0) {
				degrade_printf("\n");
				DegradePosition = 0;
			}
		}
		degrade_printf("[ERROR] %7d: %s\n", DegradeCount, name);
		DegradeError++;
		return 1;
	}
}

static void degrade_freshline(void)
{
	if (DegradeSwitch && DegradePosition) {
		degrade_printf("\n");
	}
}

void degrade_title(const char *name)
{
	degrade_freshline();
	degrade_printf("[%s]\n", name);
}

int degrade_testcheck(int check)
{
	if (check) {
		DegradeError++;
	}

	return check;
}

void degrade_increment(void)
{
	DegradeError++;
}

void degrade_output_null(Execute ptr)
{
	addr stream, null;

	Error(open_broadcast_stream_(&null, Nil));
	GetConst(SPECIAL_STANDARD_OUTPUT, &stream);
	setspecial_local(ptr, stream, null);
	GetConst(SPECIAL_ERROR_OUTPUT, &stream);
	setspecial_local(ptr, stream, null);
	GetConst(SPECIAL_DEBUG_IO, &stream);
	setspecial_local(ptr, stream, null);
}

static void degrade_execute_call(void)
{
	int finish;
	lisp_abort_calltype handler;

	handler = set_abort_setjmp_handler();
	finish = 0;
	Lisp_abort_Begin {
		degrade_execute();
		finish = 1;
	}
	Lisp_abort_End;
	(void)set_abort_handler(handler);

	if (finish == 0)
		DegradeError++;
}

int degradelisp(void)
{
	file = stdout;
	DegradeCount = 0;
	DegradeError = 0;
	DegradePosition = 0;

#ifdef LISP_DEBUG_FORCE_GC
	GcCounterForce = 0;
#endif
	degrade_printf("DEGRADE - start: %s\n", LISP_INFO);
	degrade_execute_call();
	degrade_freshline();
	degrade_printf("---\n");
	degrade_printf("DEGRADE - end: %s\n", LISP_INFO);

	if (DegradeError) {
		degrade_printf("ERROR!! (%d).\n", DegradeError);
		return 1;
	}
	else {
		degrade_printf("OK.\n");
		return 0;
	}
}

#else
#include "build.h"
#include "info.h"

int degradelisp(void)
{
	info("degrade-mode is not implemented.");
	return 0;
}
#endif

