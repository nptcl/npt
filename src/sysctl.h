#ifndef __SYSCTL_HEADER__
#define __SYSCTL_HEADER__

#include "execute.h"
#include "typedef.h"

#define sysctl_values_ _n(sysctl_values_)

int sysctl_values_(Execute ptr, addr pos, addr args);

#endif

