#ifndef __CONTROL_CALLBIND_HEADER__
#define __CONTROL_CALLBIND_HEADER__

#include "define.h"
#include "execute.h"
#include "pointer_type.h"
#include "typedef.h"

#define call_compiled_function_ _n(call_compiled_function_)
#define call_callbind_function_ _n(call_callbind_function_)
#define init_callbind_control _n(init_callbind_control)

int call_compiled_function_(Execute ptr, addr compiled);
int call_callbind_function_(Execute ptr, struct callbind_struct *str);
void init_callbind_control(void);

#endif

