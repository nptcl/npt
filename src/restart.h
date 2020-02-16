#ifndef __RESTART_HEADER__
#define __RESTART_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

_g void function_global_restart(Execute ptr, addr symbol, addr *ret);
_g void function_local_restart(Execute ptr, addr symbol, addr *ret);
_g void setf_global_restart(Execute ptr, addr symbol, addr *ret);
_g void setf_local_restart(Execute ptr, addr symbol, addr *ret);
_g int symbol_global_restart(Execute ptr, addr symbol, addr *ret);
_g int symbol_lexical_restart(Execute ptr, addr symbol, addr *ret);
_g int symbol_special_restart(Execute ptr, addr symbol, addr *ret);

_g void init_restart(void);

#endif

