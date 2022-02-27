#ifndef __WINDOWS_ERROR_HEADER__
#define __WINDOWS_ERROR_HEADER__

#include "typedef.h"

#define windows_error _n(windows_error)
#define windows_abort _n(windows_abort)

int windows_error(const char *str);
int windows_abort(const char *str);

#endif
