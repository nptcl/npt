#ifndef __CONDITION_DEBUGGER_HEADER__
#define __CONDITION_DEBUGGER_HEADER__

#include "execute.h"
#include "typedef.h"

_g void handler_warning(Execute ptr);
_g void handler_savecore(Execute ptr);
_g int invoke_debugger(Execute ptr, addr condition);
_g void set_enable_debugger(Execute ptr, int value);

_g void build_condition_debugger(Execute ptr);
_g void init_condition_debugger(void);

#endif

