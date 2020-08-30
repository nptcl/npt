#ifndef __C99_HEADER__
#define __C99_HEADER__

#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include "define.h"

#define setlocale_c _n(setlocale_c)
#define vsnprintc _n(vsnprintc)
#define snprintc _n(snprintc)
#define sscanc _n(sscanc)

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

_g const char *setlocale_c(int category);
_g int vsnprintc(char *, size_t, const char *, va_list);
_g int snprintc(char *, size_t, const char *, ...);
_g int sscanc(const char *buffer, const char *fmt, ...);

#endif

