#ifndef __C99_HEADER__
#define __C99_HEADER__

#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#ifdef _WIN32

/* Visual C++ 2010 Express */
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#pragma warning(disable:4996)
#define __func__	__FUNCTION__
/* #define strtoll		_strtoi64 */
#endif
#include <inttypes.h>

#else

/* gcc or clang */
#include <inttypes.h>

#endif
#endif

const char *setlocale_c(int category);
int vsnprintc(char *, size_t, const char *, va_list);
int snprintc(char *, size_t, const char *, ...);

