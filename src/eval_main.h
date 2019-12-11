#ifndef __EVAL_MAIN_HEADER__
#define __EVAL_MAIN_HEADER__

#include "local.h"
#include "typedef.h"

_g int eval_loop_output(Execute ptr, addr stream, addr control);
_g int eval_main_loop(Execute ptr);
_g void eval_main_string(Execute ptr, addr eval, int *abort);
_g int eval_main_load(Execute ptr, addr file, int exists, int *abort);

/* initialize */
_g void init_eval_main(void);

#endif

