#ifndef __EVAL_MAIN_HEADER__
#define __EVAL_MAIN_HEADER__

#include "execute.h"
#include "typedef.h"

#define eval_loop_output _n(eval_loop_output)
#define eval_custom_loop_ _n(eval_custom_loop_)
#define eval_main_loop_ _n(eval_main_loop_)
#define eval_main_loop_toplevel_ _n(eval_main_loop_toplevel_)
#define eval_main_string_ _n(eval_main_string_)
#define eval_main_load_ _n(eval_main_load_)

typedef int (*eval_loop_calltype)(Execute, addr, addr, int *exit, int *exec);

int eval_loop_output(Execute ptr, addr stream);
int eval_custom_loop_(Execute ptr, addr stream, eval_loop_calltype call, int *ret);
int eval_main_loop_(Execute ptr);
int eval_main_loop_toplevel_(Execute ptr);
int eval_main_string_(Execute ptr, addr eval);
int eval_main_load_(Execute ptr, addr file, int exists, int *ret);

#endif

