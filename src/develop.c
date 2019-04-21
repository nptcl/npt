#ifdef LISP_DEGRADE
#include <stdio.h>
#include <stdarg.h>
#include "degrade.h"
#include "execute.h"
#include "typedef.h"

void degrade_execute(void);
static FILE *file = NULL;
static int DegradeCount = 1;
static int DegradeError = 0;
int DegradeArgc = 0;
char **DegradeArgv = NULL;
char **DegradeEnv = NULL;

#define DEGRADE_WIDTH 40
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
			degrade_printf("[OK] %7d: %s\n", DegradeCount++, name);
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
		degrade_printf("[ERROR] %7d: %s\n", DegradeCount++, name);
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

int degradelisp(int argc, char *argv[], char *env[])
{
	DegradeArgc = argc;
	DegradeArgv = argv;
	DegradeEnv = env;
	file = stdout;
	DegradeCount = 1;
	DegradeError = 0;
	DegradePosition = 0;

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

int degradelisp(int argc, char *argv[], char *env[])
{
	info("degrade-mode is not implemented.");
	return 0;
}
#endif

