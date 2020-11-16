#ifndef __PROCESS_HEADER__
#define __PROCESS_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define run_process_ _n(run_process_)

int run_process_(Execute ptr, addr var, addr args, addr rest, addr *ret);

#endif

