#include "eval.h"
#include "eval_copy.h"
#include "eval_main.h"
#include "eval_stack.h"
#include "typedef.h"

_g void init_eval(void)
{
	init_eval_copy();
	init_eval_main();
	init_eval_stack();
}

