#ifndef __SCOPE_FUNCTION_HEADER__
#define __SCOPE_FUNCTION_HEADER__

#include "execute.h"
#include "typedef.h"

#define make_tablefunction_stack _n(make_tablefunction_stack)
#define update_tablefunction_ _n(update_tablefunction_)
#define push_tablefunction_global_ _n(push_tablefunction_global_)
#define scope_function_call_ _n(scope_function_call_)

int make_tablefunction_stack(Execute ptr, addr *ret, addr stack, addr call);
int update_tablefunction_(Execute ptr, addr stack, addr pos);
int push_tablefunction_global_(Execute ptr, addr stack, addr call, addr *ret);
int scope_function_call_(Execute ptr, addr *ret, addr eval);

#endif

