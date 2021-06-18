#include "control_object.h"
#include "eval_stack.h"
#include "gc.h"
#include "load_time_value.h"
#include "scope.h"
#include "scope_object.h"
#include "scope_table.h"

static int eval_scope_call_(Execute ptr, addr *ret, addr eval)
{
	Return(begin_eval_stack_(ptr));
	return scope_eval_lexical(ptr, ret, eval);
}

int eval_scope_(Execute ptr, addr *ret, addr eval)
{
	addr control;

	push_control(ptr, &control);
	(void)eval_scope_call_(ptr, ret, eval);
	free_eval_stack(ptr);
	return pop_control_(ptr, control);
}

void init_scope(void)
{
	init_scope_function();
}

