#ifndef __CONTROL_EXECUTE_HEADER__
#define __CONTROL_EXECUTE_HEADER__

#include "execute.h"
#include "typedef.h"

#define runcode_control_ _n(runcode_control_)
#define execute_control _n(execute_control)
#define apply_control _n(apply_control)
#define applya_control _n(applya_control)
#define funcall_control _n(funcall_control)
#define call_control _n(call_control)
#define callclang_apply _n(callclang_apply)
#define callclang_applya _n(callclang_applya)
#define callclang_funcall _n(callclang_funcall)

int runcode_control_(Execute ptr, addr code);

int execute_control(Execute ptr, addr call);
int apply_control(Execute ptr, addr call, addr args);
int applya_control(Execute ptr, addr call, ...);
int funcall_control(Execute ptr, addr call, ...);
int call_control(Execute ptr, addr args);

int callclang_apply(Execute ptr, addr *ret, addr call, addr cons);
int callclang_applya(Execute ptr, addr *ret, addr call, ...);
int callclang_funcall(Execute ptr, addr *ret, addr call, ...);

#endif

