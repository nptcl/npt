#include "eval.h"
#include "eval_copy.h"
#include "eval_main.h"
#include "typedef.h"

void init_eval(void)
{
	init_eval_copy();
	init_eval_main();
}

