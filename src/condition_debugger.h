#ifndef __CONDITION_DEBUGGER_HEADER__
#define __CONDITION_DEBUGGER_HEADER__

#include "execute.h"
#include "typedef.h"

#define handler_warning_ _n(handler_warning_)
#define handler_savecore_ _n(handler_savecore_)
#define handler_exit_ _n(handler_exit_)
#define invoke_debugger _n(invoke_debugger)
#define set_enable_debugger _n(set_enable_debugger)
#define build_condition_debugger _n(build_condition_debugger)
#define init_condition_debugger _n(init_condition_debugger)

_g int handler_warning_(Execute ptr);
_g int handler_savecore_(Execute ptr);
_g int handler_exit_(Execute ptr);
_g int invoke_debugger(Execute ptr, addr condition);
_g void set_enable_debugger(Execute ptr, int value);

_g void build_condition_debugger(Execute ptr);
_g void init_condition_debugger(void);

#endif

