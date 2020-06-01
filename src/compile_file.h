#ifndef __COMPILE_FILE_HEADER__
#define __COMPILE_FILE_HEADER__

#include "execute.h"
#include "typedef.h"

_g int eval_compile_p(Execute ptr);
_g void init_compile_file(void);
_g int eval_compile_file(Execute ptr, addr pos);

#endif

