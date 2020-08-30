#include "control_object.h"
#include "eval_stack.h"
#include "gc.h"
#include "load_time_value.h"
#include "scope.h"
#include "scope_function.h"
#include "scope_object.h"

static int eval_scope_call_(Execute ptr, LocalHold hold, addr *ret, addr eval)
{
	init_scope_load_time_value(ptr);
	Return(begin_eval_stack_(ptr));
	free_eval_stack(ptr);
	Return(scope_eval_lexical(ptr, ret, eval));
	localhold_set(hold, 0, *ret);

	return 0;
}

_g int eval_scope(Execute ptr, addr *ret, addr eval)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_control(ptr, &control);
	(void)eval_scope_call_(ptr, hold, ret, eval);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

_g int eval_scope_compile(Execute ptr, addr *ret, addr eval)
{
	return scope_eval_lexical(ptr, ret, eval);
}

_g void init_scope(void)
{
	init_scope_function();
}

