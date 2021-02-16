#include "control_object.h"
#include "eval_stack.h"
#include "gc.h"
#include "load_time_value.h"
#include "scope.h"
#include "scope_function.h"
#include "scope_object.h"

int begin_scope_(Execute ptr)
{
	init_scope_load_time_value(ptr);
	Return(begin_eval_stack_(ptr));
	free_eval_stack(ptr);

	return 0;
}

int eval_scope_(Execute ptr, addr *ret, addr eval)
{
	return scope_eval_lexical(ptr, ret, eval);
}

void init_scope(void)
{
	init_scope_function();
}

