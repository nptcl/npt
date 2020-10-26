#ifndef __SYSCALL_COMMON_HEADER__
#define __SYSCALL_COMMON_HEADER__

#include "define.h"

#define init_syscall_common _n(init_syscall_common)
#define build_syscall_common _n(build_syscall_common)

_g void init_syscall_common(void);
_g void build_syscall_common(void);

#endif

