#ifndef __EXTERN_INIT_HEADER__
#define __EXTERN_INIT_HEADER__

#include <stdio.h>
#include <stdarg.h>
#include "define.h"

FILE *lisperror_stream(void);
int lisperror_noeol(const char *fmt, ...);
int lisperror_va(const char *fmt, va_list args);
int lisperror(const char *fmt, ...);

#endif

