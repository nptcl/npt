#include "define.h"

#ifdef LISP_DEGRADE
#include <stdio.h>
#include <stdarg.h>
#include "constant.h"
#include "control.h"
#include "degrade.h"
#include "execute.h"
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

int degrade_testnormal(int (*call)(void))
{
	int result;
	codejump jump;
	Execute ptr;

	ptr = Execute_Thread;
	begin_switch(ptr, &jump);
	result = 1;
	if (codejump_run_p(&jump)) {
		result = (*call)();
	}
	end_switch(&jump);
	if (result && codejump_error_p(&jump)) {
		DegradeError++;
		return 1;
	}

	return 0;
}

void degrade_increment(void)
{
	DegradeError++;
}

void degrade_output_null(Execute ptr)
{
	addr stream, null;

	open_broadcast_stream(&null, Nil);
	GetConst(SPECIAL_STANDARD_OUTPUT, &stream);
	setspecial_local(ptr, stream, null);
	GetConst(SPECIAL_ERROR_OUTPUT, &stream);
	setspecial_local(ptr, stream, null);
	GetConst(SPECIAL_DEBUG_IO, &stream);
	setspecial_local(ptr, stream, null);
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
	degrade_execute();
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
#include "info.h"

int degradelisp(void)
{
	info("degrade-mode is not implemented.");
	return 0;
}
#endif

