#ifndef __DEGRADE_HEADER__
#define __DEGRADE_HEADER__

#include "define.h"

#ifdef LISP_DEGRADE
#include "execute.h"

extern int DegradeCount;
extern int DegradeError;
int degrade_code(void (*init)(Execute), int (*call)(void));
int degrade_printf(const char *fmt, ...);
int degrade_test(int check, const char *name);
void degrade_title(const char *name);
int degrade_testcheck(int check);
void degrade_increment(void);
void degrade_output_null(Execute ptr);

#define DegradeTitle		degrade_title(__FILE__)
#define DegradeCode(x)		degrade_code(testinit_##x, testcase_##x)
#define DegradeCheck(x)		{ if (x()) { degrade_increment(); return; } }
#define TestCheck(x)		{ if (degrade_testcheck((x) != 0)) return 1; }
#define TestBreak(x)		TestCheck(x())
#define RETURN				{ return 0; error: return 1; }

#define test(x, y) { \
	if (degrade_test((x) != 0, (y))) { \
		goto error; \
	} \
}

#endif
#endif

