#include "control_object.h"
#include "eval_stack.h"
#include "gc.h"
#include "scope_object.h"
#include "scope_function.h"

_g int eval_scope(Execute ptr, addr *ret, addr eval)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_new_control(ptr, &control);
	begin_eval_stack(ptr);
	free_eval_stack(ptr);
	Return(scope_eval(ptr, ret, eval));
	localhold_set(hold, 0, *ret);
	Return(free_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

_g void init_scope(void)
{
	init_scope_function();
}

