#ifndef __CONTROL_CALLBIND_HEADER__
#define __CONTROL_CALLBIND_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define call_compiled_function _n(call_compiled_function)
#define init_callbind_control _n(init_callbind_control)

int call_compiled_function(Execute ptr, addr compiled);
void init_callbind_control(void);

#endif

