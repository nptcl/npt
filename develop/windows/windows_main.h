#ifndef __WINDOWS_MAIN_HEADER__
#define __WINDOWS_MAIN_HEADER__

#include "main_argv.h"
#include "typedef.h"

#define windows_main _n(windows_main)
#define windows_main_start _n(windows_main_start)

int windows_main(struct lispargv *ptr);
int windows_main_start(void);

#endif

