#ifndef __LOCALTIME_HEADER__
#define __LOCALTIME_HEADER__

#include "define.h"
#include "typedef.h"

#if defined(LISP_WINDOWS)
/* Windows */
#define __STDC_WANT_LIB_EXT1__ 1
#include <time.h>
#elif defined(LISP_POSIX)
/* Posix */
#define _BSD_SOURCE 1
#include <time.h>
#elif defined(__STDC_LIB_EXT1__)
/* C11 */
#define __STDC_WANT_LIB_EXT1__ 1
#include <time.h>
#else
/* C99 */
#include <time.h>
#endif

_g void init_localtime(void);
_g int gmtime_arch(struct tm *ret, const time_t *time);
_g int localtime_arch(struct tm *ret, const time_t *time);
_g int nowtime_string(char *ptr, size_t size);

#endif

