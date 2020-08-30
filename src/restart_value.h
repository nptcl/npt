#ifndef __RESTART_VALUE_HEADER__
#define __RESTART_VALUE_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define symbol_special_restart _n(symbol_special_restart)
#define function_global_restart _n(function_global_restart)
#define setf_global_restart _n(setf_global_restart)
#define callname_global_restart _n(callname_global_restart)
#define init_restart_value _n(init_restart_value)

_g int symbol_special_restart(Execute ptr, addr symbol, addr *ret);
_g int function_global_restart(Execute ptr, addr symbol, addr *ret);
_g int setf_global_restart(Execute ptr, addr symbol, addr *ret);
_g int callname_global_restart(Execute ptr, addr name, addr *ret);

_g void init_restart_value(void);

#endif

