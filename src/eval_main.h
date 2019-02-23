#ifndef __EVAL_MAIN_HEADER__
#define __EVAL_MAIN_HEADER__

#include "local.h"
#include "typedef.h"

int eval_main_loop(Execute ptr);
void eval_main_string(Execute ptr, addr eval, int *abort);
int eval_main_load(Execute ptr, addr file, int exists, int *abort);

#endif

