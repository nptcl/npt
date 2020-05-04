#ifndef __RESTART_VALUE_HEADER__
#define __RESTART_VALUE_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

_g int symbol_global_restart(Execute ptr, addr symbol, addr *ret);
_g int symbol_lexical_restart(Execute ptr, addr symbol, addr *ret);
_g int symbol_special_restart(Execute ptr, addr symbol, addr *ret);
_g int function_global_restart(Execute ptr, addr symbol, addr *ret);
_g int function_local_restart(Execute ptr, addr symbol, addr *ret);
_g int setf_global_restart(Execute ptr, addr symbol, addr *ret);
_g int setf_local_restart(Execute ptr, addr symbol, addr *ret);
_g int callname_global_restart(Execute ptr, addr name, addr *ret);

_g void init_restart_value(void);

#endif

