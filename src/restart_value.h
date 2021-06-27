#ifndef __RESTART_VALUE_HEADER__
#define __RESTART_VALUE_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define symbol_special_restart _n(symbol_special_restart)
#define function_global_restart _n(function_global_restart)
#define setf_global_restart _n(setf_global_restart)
#define callname_global_restart _n(callname_global_restart)
#define fdefinition_restart_ _n(fdefinition_restart_)
#define abort_restart_char_heap _n(abort_restart_char_heap)
#define abort_restart_char _n(abort_restart_char)
#define init_restart_value _n(init_restart_value)

int symbol_special_restart(Execute ptr, addr symbol, addr *ret);
int function_global_restart(Execute ptr, addr symbol, addr *ret);
int setf_global_restart(Execute ptr, addr symbol, addr *ret);
int callname_global_restart(Execute ptr, addr name, addr *ret);
int fdefinition_restart_(Execute ptr, addr name, addr *ret);
void abort_restart_char_heap(addr *ret, const char *str);
void abort_restart_char_control(Execute ptr, const char *str);

void init_restart_value(void);

#endif

