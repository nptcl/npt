#ifndef __TYPE_CALL_HEADER__
#define __TYPE_CALL_HEADER__

#include "execute.h"
#include "typedef.h"

#define typep_function_ _n(typep_function_)
#define typep_compiled_function_ _n(typep_compiled_function_)

int typep_function_(Execute ptr, addr value, addr type, int *ret);
int typep_compiled_function_(Execute ptr, addr value, addr type, int *ret);

#endif

