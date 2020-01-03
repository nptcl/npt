#ifndef __PROCESS_HEADER__
#define __PROCESS_HEADER__

#include "define.h"
#include "typedef.h"

_g void run_process(LocalRoot local, addr var, addr args, addr rest, addr *ret);
_g void ed_process(Execute ptr, addr file);

#endif

