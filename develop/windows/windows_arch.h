#ifndef __WINDOWS_ARCH_HEADER__
#define __WINDOWS_ARCH_HEADER__

#include <stddef.h>
#include "typedef.h"

#define exit_windows _n(exit_windows)
#define stdout_windows _n(stdout_windows)
#define stderr_windows _n(stderr_windows)
#define getwidth_windows _n(getwidth_windows)

void exit_windows(int code);
void stdout_windows(const char *msg);
void stderr_windows(const char *msg);
int getwidth_windows(unsigned *rx, unsigned *ry);

#endif

