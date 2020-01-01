#include <stdio.h>
#include "localtime.h"

#if defined(LISP_WINDOWS)
/*****************************************************************************
 *  Windows
 *****************************************************************************/
#include <windows.h>

_g void init_localtime(void)
{
	_tzset();
}
_g int gmtime_arch(struct tm *ret, const time_t *time)
{
	return gmtime_s(ret, time) != 0;
}
_g int localtime_arch(struct tm *ret, const time_t *time)
{
	return localtime_s(ret, time) != 0;
}

#elif defined(LISP_POSIX)
/*****************************************************************************
 *  Posix
 *****************************************************************************/
#define _BSD_SOURCE 1
#include <time.h>
_g void init_localtime(void)
{
	tzset();
}
_g int gmtime_arch(struct tm *ret, const time_t *time)
{
	return gmtime_r(time, ret) == NULL;
}
_g int localtime_arch(struct tm *ret, const time_t *time)
{
	return localtime_r(time, ret) == NULL;
}

#elif defined(__STDC_LIB_EXT1__)
/*****************************************************************************
 *  C11
 *****************************************************************************/
#define __STDC_WANT_LIB_EXT1__ 1
#include <time.h>
_g void init_localtime(void)
{
	tzset();
}
_g int gmtime_arch(struct tm *ret, const time_t *time)
{
	return gmtime_s(time, ret) == NULL;
}
_g int localtime_arch(struct tm *ret, const time_t *time)
{
	return localtime_s(time, ret) == NULL;
}

#else
/*****************************************************************************
 *  C99
 *****************************************************************************/
#include <time.h>
_g void init_localtime(void)
{
	/* Don't execute tzset() */
}
_g int gmtime_arch(struct tm *ret, const time_t *time)
{
	struct tm *check;

	check = gmtime(time);
	if (check == NULL)
		return 1;
	*ret = *check;

	return 0;
}
_g int localtime_arch(struct tm *ret, const time_t *time)
{
	struct tm *check;

	check = localtime(time);
	if (check == NULL)
		return 1;
	*ret = *check;

	return 0;
}
#endif


/*
 *  localtime
 */
#ifdef LISP_WINDOWS
_g int nowtime_string(char *ptr, size_t size)
{
	SYSTEMTIME st;

	GetLocalTime(&st);
	snprintf(ptr, size, "%04d/%02d/%02d-%2d:%02d:%02d",
			st.wYear, st.wMonth, st.wDay, st.wHour, st.wMinute, st.wSecond);

	return 0;
}
#else
_g int nowtime_string(char *ptr, size_t size)
{
	time_t now;
	struct tm str;

	now = time(NULL);
	if (now == (time_t)-1)
		return 1;
	if (localtime_arch(&str, &now))
		return 1;
	snprintf(ptr, size, "%04d/%02d/%02d-%2d:%02d:%02d",
			str.tm_year + 1900, str.tm_mon + 1, str.tm_mday,
			str.tm_hour, str.tm_min, str.tm_sec);

	return 0;
}
#endif

