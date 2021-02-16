#ifndef __COMPILE_EVAL__
#define __COMPILE_EVAL__

#include "execute.h"
#include "typedef.h"

#define begin_compile_eval_ _n(begin_compile_eval_)
#define compile_execute_ _n(compile_execute_)

int begin_compile_eval_(Execute ptr);
int compile_execute_(Execute ptr, addr pos);

#endif

