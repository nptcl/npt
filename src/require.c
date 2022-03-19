#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "eval_load.h"
#include "function.h"
#include "require.h"
#include "require_clos.h"
#include "restart_value.h"
#include "strtype.h"
#include "symbol.h"

/*
 *  provide
 */
int provide_common_(Execute ptr, addr var)
{
	addr symbol, list;

	Return(string_designer_heap_(&var, var, NULL));
	GetConst(SPECIAL_MODULES, &symbol);
	Return(getspecialcheck_local_(ptr, symbol, &list));
	Return(pushnew_equal_heap_(list, var, &list));
	setspecial_local(ptr, symbol, list);

	return 0;
}

int modules_find_(Execute ptr, addr var, int *ret)
{
	addr symbol, list;

	Return(string_designer_heap_(&var, var, NULL));
	GetConst(SPECIAL_MODULES, &symbol);
	Return(getspecialcheck_local_(ptr, symbol, &list));
	return find_list_equal_safe_(var, list, ret);
}

int modules_delete_(Execute ptr, addr var)
{
	addr symbol, list;

	Return(string_designer_heap_(&var, var, NULL));
	GetConst(SPECIAL_MODULES, &symbol);
	Return(getspecialcheck_local_(ptr, symbol, &list));
	remove_list_equal_safe_heap_(var, list, &list);
	setspecial_local(ptr, symbol, list);

	return 0;
}


/*
 *  require
 */
static int require_function_common_call_(Execute ptr, addr call, addr var, int *ret)
{
	if (symbolp(call)) {
		Return(function_global_restart_(ptr, call, &call));
	}
	Return(funcall_control_(ptr, call, var, NULL));
	getresult_control(ptr, &var);
	return Result(ret, (var == Nil)? 0: 1);
}

static int require_function_common_(Execute ptr, addr var, int *ret)
{
	int check;
	addr list, call, control;

	/* lisp-system::*module-provider-functions* */
	GetConst(SYSTEM_MODULE_PROVIDER_FUNCTIONS, &list);
	Return(getspecialcheck_local_(ptr, list, &list));

	while (list != Nil) {
		Return_getcons(list, &call, &list);
		/* funcall */
		push_control(ptr, &control);
		(void)require_function_common_call_(ptr, call, var, &check);
		Return(pop_control_(ptr, control));
		/* check */
		if (check)
			return Result(ret, 1);
	}

	/* error */
	return fmte_("Cannot require ~S.", var, NULL);
}

static int require_list_common_(Execute ptr, addr var, addr list, int *ret)
{
	int check;
	addr x;

	while (list != Nil) {
		Return_getcons(list, &x, &list);
		Return(eval_load_(ptr, &check, x, Unbound, Unbound, 1, Unbound));
	}

	*ret = 1;
	return 0;
}

int require_common_(Execute ptr, addr var, addr opt)
{
	int push;

	Return(string_designer_heap_(&var, var, NULL));
	push = 0;
	if (opt == Unbound || opt == Nil) {
		Return(require_function_common_(ptr, var, &push));
	}
	else {
		if (! listp(opt))
			conscar_heap(&opt, opt);
		Return(require_list_common_(ptr, var, opt, &push));
	}
	if (push) {
		Return(provide_common_(ptr, var));
	}

	return 0;
}


/*
 *  require default
 */
int require_append_(Execute ptr, addr var, int forcep, int *ret)
{
	int check;

	/* string-designer */
	Return(string_designer_heap_(&var, var, NULL));

	/* clos */
	Return(require_clos_(ptr, var, forcep, &check));
	if (check)
		return Result(ret, 1);

	/* next function */
	return Result(ret, 0);
}

static int function_require_default_(Execute ptr, addr var)
{
	int check;

	Return(string_designer_heap_(&var, var, NULL));
	Return(require_append_(ptr, var, 0, &check));
	setbool_control(ptr, check);
	return 0;
}

static void make_require_default(addr *ret)
{
	addr pos;

	compiled_heap(&pos, Nil);
	setcompiled_var1(pos, p_function_require_default);
	*ret = pos;
}


/*
 *  unrequire
 */
int require_delete_(Execute ptr, addr var, int forcep, int *ret)
{
	int check;

	/* string-designer */
	Return(string_designer_heap_(&var, var, NULL));

	/* clos */
	Return(unrequire_clos_(ptr, var, forcep, &check));
	if (check)
		return Result(ret, 1);

	/* error */
	return Result(ret, 0);
}


/*
 *  build
 */
void build_require(void)
{
	addr symbol, pos, list;

	GetConst(SYSTEM_MODULE_PROVIDER_FUNCTIONS, &symbol);
	make_require_default(&pos);
	list_heap(&list, pos, NULL);
	SetValueSymbol(symbol, list);
}


/*
 *  init
 */
void init_require(void)
{
	SetPointerType_(var1, function_require_default);
}

