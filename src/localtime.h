#ifndef __LOCALTIME_HEADER__
#define __LOCALTIME_HEADER__

#include "define.h"
#include "typedef.h"

#define init_localtime _n(init_localtime)
#define gmtime_arch _n(gmtime_arch)
#define localtime_arch _n(localtime_arch)
#define nowtime_string _n(nowtime_string)

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

void init_localtime(void);
int gmtime_arch(struct tm *ret, const time_t *time);
int localtime_arch(struct tm *ret, const time_t *time);
int nowtime_string(char *ptr, size_t size);

#endif

