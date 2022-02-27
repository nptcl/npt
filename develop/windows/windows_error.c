#include "windows_error.h"
#include "windows_window.h"
#include <Windows.h>

int windows_error(const char *str)
{
	windows_window_error(str);
	return 1;
}

int windows_abort(const char *str)
{
	windows_error(str);
	exit(1);
	return 1;
}
