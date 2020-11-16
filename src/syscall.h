#ifndef __SYSCALL_HEADER__
#define __SYSCALL_HEADER__

#include "define.h"

#define init_syscall _n(init_syscall)
#define build_syscall _n(build_syscall)

void init_syscall(void);
void build_syscall(void);

#endif

