#ifndef __LOOP_BIND_HEADER__
#define __LOOP_BIND_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

_g int loop_bind_initial_list_(Execute ptr, addr var, addr type, addr *ret);
_g int loop_bind_common(Execute ptr, addr pos, addr type, addr value, addr *ret);

#endif

