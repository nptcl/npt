#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "eval_execute.h"
#include "require.h"
#include "strtype.h"
#include "symbol.h"

/*
 *  provide
 */
_g int provide_common_(Execute ptr, addr var)
{
	addr symbol, list;

	/* string-designer */
	Return(string_designer_heap_(&var, var, NULL));
	/* push *modules */
	GetConst(SPECIAL_MODULES, &symbol);
	Return(getspecialcheck_local_(ptr, symbol, &list));
	Return(pushnew_equal_heap_(list, var, &list));
	setspecial_local(ptr, symbol, list);

	return 0;
}


/*
 *  require
 */
static int require_function_common(Execute ptr, addr var, int *ret)
{
	int check;
	addr list, call, control;

	/* lisp-system::*module-provider-functions* */
	GetConst(SYSTEM_MODULE_PROVIDER_FUNCTIONS, &list);
	Return(getspecialcheck_local_(ptr, list, &list));

	while (list != Nil) {
		Return_getcons(list, &call, &list);
		/* funcall */
		push_new_control(ptr, &control);
		Return(funcall_control(ptr, call, var, NULL));
		getresult_control(ptr, &call);
		check = (call == Nil)? 0: 1;
		Return(free_control_(ptr, control));
		/* check */
		if (check)
			return Result(ret, 1);
	}

	/* error */
	return fmte_("Cannot require ~S.", var, NULL);
}

static int require_list_common(Execute ptr, addr var, addr list, int *ret)
{
	int check;
	addr x;

	while (list != Nil) {
		Return_getcons(list, &x, &list);
		Return(eval_load(ptr, &check, x, Unbound, Unbound, 1, Unbound));
	}

	*ret = 1;
	return 0;
}

_g int require_common(Execute ptr, addr var, addr opt)
{
	int check, push;

	Return(string_designer_heap_(&var, var, NULL));
	push = 0;
	if (opt == Unbound || opt == Nil) {
		check = require_function_common(ptr, var, &push);
	}
	else {
		if (! listp(opt))
			conscar_heap(&opt, opt);
		check = require_list_common(ptr, var, opt, &push);
	}
	if (check)
		return 1;
	if (push) {
		Return(provide_common_(ptr, var));
	}

	return 0;
}


/*
 *  build
 */
_g void build_require(void)
{
	addr symbol;
	GetConst(SYSTEM_MODULE_PROVIDER_FUNCTIONS, &symbol);
	SetValueSymbol(symbol, Nil);
}

