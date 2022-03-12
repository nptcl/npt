#ifndef __PROCESS_HEADER__
#define __PROCESS_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define run_process_ _n(run_process_)
#define dlfile_process_ _n(dlfile_process_)
#define dlcall_process_ _n(dlcall_process_)

int run_process_(Execute ptr, addr var, addr args, addr rest, addr *ret);
int dlfile_process_(Execute ptr, addr type, addr args, addr *ret, addr *retp);
int dlcall_process_(Execute ptr, addr paper, addr args);

#endif

