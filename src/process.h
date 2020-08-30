#ifndef __PROCESS_HEADER__
#define __PROCESS_HEADER__

#include "define.h"
#include "typedef.h"

#define run_process_ _n(run_process_)
#define ed_process_ _n(ed_process_)

_g int run_process_(LocalRoot local, addr var, addr args, addr rest, addr *ret);
_g int ed_process_(Execute ptr, addr file);

#endif

