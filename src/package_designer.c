#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "control_object.h"
#include "execute_object.h"
#include "execute_values.h"
#include "function.h"
#include "hold.h"
#include "package.h"
#include "package_designer.h"
#include "prompt_for.h"
#include "restart.h"
#include "strtype.h"
#include "strvect.h"
#include "typedef.h"

/*
 *  check
 */
int package_designer_p(addr pos)
{
	return packagep(pos) || string_designer_p(pos);
}

int package_designer_equal_(addr left, addr right, int *ret)
{
	if (packagep(left))
		GetPackage(left, PACKAGE_INDEX_NAME, &left);
	if (packagep(right))
		GetPackage(right, PACKAGE_INDEX_NAME, &right);

	return string_designer_equal_(left, right, ret);
}


/*
 *  package-designer
 */
static int function_package_designer_interactive(Execute ptr)
{
	addr prompt, pos;

	strvect_char_heap(&prompt, "Input package name: ");
	Return(prompt_for_stream_(ptr, T, prompt, &pos));
	list_heap(&pos, pos, NULL);
	setresult_control(ptr, pos);

	return 0;
}

static void compiled_use_value_interactive_package_designer(addr *ret)
{
	addr pos;

	compiled_heap(&pos, Nil);
	setcompiled_empty(pos, p_defun_package_designer_interactive);
	*ret = pos;
}

static void restart_use_value_package_designer(addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_USE_VALUE, &pos);
	restart_heap(&restart, pos);
	/* report */
	strvect_char_heap(&pos, "Input another package.");
	setreport_restart(restart, pos);
	/* function */
	GetConst(FUNCTION_VALUES, &pos);
	setfunction_restart(restart, pos);
	/* interactive */
	compiled_use_value_interactive_package_designer(&pos);
	setinteractive_restart(restart, pos);
	/* condition */
	GetConst(CONDITION_PACKAGE_ERROR, &pos);
	setcondition_restart(restart, pos);
	/* restart-case */
	setescape_restart(restart, 1);

	*ret = restart;
}

static int restart_package_designer_(addr pos, addr *ret)
{
	addr restart, control;
	Execute ptr;

	ptr = Execute_Thread;
	restart_use_value_package_designer(&restart);
	push_control(ptr, &control);
	gchold_push_force_local(ptr->local, pos);
	pushrestart_control(ptr, restart);

	(void)call_simple_package_error_va_(NULL, "No such a package ~S.", pos, NULL);
	if (ptr->throw_value == throw_normal)
		goto escape;
	if (ptr->throw_control != control)
		goto escape;

	/* use_value */
	normal_throw_control(ptr);
	getresult_control(ptr, ret);

escape:
	return pop_control_(ptr, control);
}

int package_designer_(addr pos, addr *ret)
{
	addr x;

	for (;;) {
		Return(find_package_(pos, &x));
		if (x != Nil)
			break;
		Return(restart_package_designer_(pos, &pos));
	}

	return Result(ret, x);
}

int package_designer_update_p_(addr pos, addr *ret)
{
	Return(package_designer_(pos, &pos));
	if (get_readonly_package(pos))
		return fmte_("~S is a readonly package.", pos, NULL);

	return Result(ret, pos);
}

void init_package_designer(void)
{
	SetPointerCall(defun, empty, package_designer_interactive);
}

