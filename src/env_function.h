#ifndef __ENV_FUNCTION_HEADER__
#define __ENV_FUNCTION_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

_g int apropos_common(Execute ptr, addr var, addr package);
_g int apropos_list_common(Execute ptr, addr var, addr package, addr *ret);
_g void time_common(Execute ptr, addr form, addr env, addr *ret);
_g void room_common(Execute ptr, addr var);
_g int ed_common(Execute ptr, addr var);

#endif

