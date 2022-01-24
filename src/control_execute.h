#ifndef __CONTROL_EXECUTE_HEADER__
#define __CONTROL_EXECUTE_HEADER__

#include "execute.h"
#include "typedef.h"

#define runcode_control_ _n(runcode_control_)
#define revert_control_ _n(revert_control_)
#define revert_goto_control_ _n(revert_goto_control_)
#define execute_control_ _n(execute_control_)
#define apply_control_ _n(apply_control_)
#define apply_named_control_ _n(apply_named_control_)
#define applya_control_ _n(applya_control_)
#define funcall_control_ _n(funcall_control_)
#define apply1_control_ _n(apply1_control_)
#define applya1_control_ _n(applya1_control_)
#define funcall1_control_ _n(funcall1_control_)

int runcode_control_(Execute ptr, addr code);
int revert_control_(Execute ptr);
int revert_goto_control_(Execute ptr, size_t index);
int execute_control_(Execute ptr, addr call);

int apply_control_(Execute ptr, addr call, addr args);
int apply_named_control_(Execute ptr, addr call, addr list);
int applya_control_(Execute ptr, addr call, ...);
int funcall_control_(Execute ptr, addr call, ...);

int apply1_control_(Execute ptr, addr *ret, addr call, addr cons);
int applya1_control_(Execute ptr, addr *ret, addr call, ...);
int funcall1_control_(Execute ptr, addr *ret, addr call, ...);

#endif

