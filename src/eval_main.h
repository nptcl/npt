#ifndef __EVAL_MAIN_HEADER__
#define __EVAL_MAIN_HEADER__

#include "execute.h"
#include "typedef.h"

#define eval_loop_output _n(eval_loop_output)
#define eval_custom_loop_ _n(eval_custom_loop_)
#define eval_main_loop_ _n(eval_main_loop_)
#define eval_main_string_ _n(eval_main_string_)
#define eval_main_load_ _n(eval_main_load_)
#define init_eval_main _n(init_eval_main)

typedef int (*eval_loop_calltype)(Execute, addr, addr, int *exit, int *exec);

_g int eval_loop_output(Execute ptr, addr stream);
_g int eval_custom_loop_(Execute ptr, addr stream, eval_loop_calltype call);
_g int eval_main_loop_(Execute ptr);
_g int eval_main_string_(Execute ptr, addr eval);
_g int eval_main_load_(Execute ptr, addr file, int exists, int *ret);

/* initialize */
_g void init_eval_main(void);

#endif

