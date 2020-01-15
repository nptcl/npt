#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control.h"
#include "eval.h"
#include "require.h"
#include "strtype.h"
#include "symbol.h"

/*
 *  provide
 */
_g void provide_common(Execute ptr, addr var)
{
	addr symbol, list;

	/* string-designer */
	string_designer_heap(&var, var);
	/* push *modules */
	GetConst(SPECIAL_MODULES, &symbol);
	getspecialcheck_local(ptr, symbol, &list);
	pushnew_equal_heap(list, var, &list);
	setspecial_local(ptr, symbol, list);
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
	getspecialcheck_local(ptr, list, &list);

	while (list != Nil) {
		getcons(list, &call, &list);
		/* funcall */
		push_close_control(ptr, &control);
		if (funcall_control(ptr, call, var, NULL))
			return free_check_control(ptr, control, 1);
		getresult_control(ptr, &call);
		check = (call == Nil)? 0: 1;
		Return1(free_check_control(ptr, control, 0));
		/* check */
		if (check) {
			*ret = 1;
			return 0;
		}
	}

	/* error */
	fmte("Cannot require ~S.", var, NULL);
	return 0;
}

static int require_list_common(Execute ptr, addr var, addr list, int *ret)
{
	int check;
	addr x;

	while (list != Nil) {
		getcons(list, &x, &list);
		Return1(eval_load(ptr, &check, x, Unbound, Unbound, 1, Unbound));
	}

	*ret = 1;
	return 0;
}

_g int require_common(Execute ptr, addr var, addr list)
{
	int check, push;

	string_designer_heap(&var, var);
	push = 0;
	if (list == Unbound || list == Nil)
		check = require_function_common(ptr, var, &push);
	else
		check = require_list_common(ptr, var, list, &push);
	if (check)
		return 1;
	if (push)
		provide_common(ptr, var);

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

