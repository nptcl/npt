#ifndef __PROCESS_ARCH_HEADER__
#define __PROCESS_ARCH_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define run_process_arch_ _n(run_process_arch_)

int run_process_arch_(Execute ptr, addr instance, addr *ret);

#endif

