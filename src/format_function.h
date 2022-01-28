#ifndef __FORMAT_FUNCTION_HEADER__
#define __FORMAT_FUNCTION_HEADER__

#include "execute.h"
#include "typedef.h"

#define format_execute_ _n(format_execute_)
#define init_format_function _n(init_format_function)

int format_execute_(Execute ptr, addr stream, addr format, addr args, addr *ret);
void init_format_function(void);

#endif

