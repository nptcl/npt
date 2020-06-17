#ifndef __COMPILE_LOAD_HEADER__
#define __COMPILE_LOAD_HEADER__

#include "execute.h"
#include "typedef.h"

_g void eval_compile_init(Execute ptr);
_g void eval_compile_value(Execute ptr, size_t pointer, addr *ret);
_g int eval_compile_load(Execute ptr, addr stream);

#endif

