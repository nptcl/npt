#include "condition.h"
#include "cons.h"
#include "execute_values.h"
#include "define.h"
#include "control_object.h"
#include "terme_function.h"
#include "terme_values.h"

#ifdef LISP_TERME_WINDOWS
#include "windows_values.h"
#endif

static int terme_values_input_(Execute ptr, addr args)
{
	addr x, y;

	Return(terme_call_input_(args, &x, &y));
	setvalues_control(ptr, x, y, NULL);
	return 0;
}

static int terme_values_move_(Execute ptr, addr args)
{
	Return(terme_call_move_(args));
	setresult_control(ptr, Nil);
	return 0;
}

static int terme_values_clear_(Execute ptr)
{
	Return(terme_call_clear_());
	setresult_control(ptr, Nil);
	return 0;
}

static int terme_values_begin_(Execute ptr)
{
	addr pos;

	Return(terme_call_begin_(&pos));
	setresult_control(ptr, pos);
	return 0;
}

static int terme_values_end_(Execute ptr, addr args)
{
	addr pos;

	Return_getcar(args, &pos);
	Return(terme_call_end_(pos));
	setresult_control(ptr, Nil);

	return 0;
}

static int terme_values_enable_(Execute ptr)
{
	setbool_control(ptr, terme_call_enable_p());
	return 0;
}

static int terme_values_operator_(Execute ptr, addr var, addr args, int *ret)
{
	addr check;

	*ret = 1;

	/* input */
	GetConst(SYSTEM_TERME_INPUT, &check);
	if (var == check)
		return terme_values_input_(ptr, args);

	/* move */
	GetConst(SYSTEM_TERME_MOVE, &check);
	if (var == check)
		return terme_values_move_(ptr, args);

	/* clear */
	GetConst(SYSTEM_TERME_CLEAR, &check);
	if (var == check)
		return terme_values_clear_(ptr);

	/* begin */
	GetConst(SYSTEM_TERME_BEGIN, &check);
	if (var == check)
		return terme_values_begin_(ptr);

	/* end */
	GetConst(SYSTEM_TERME_END, &check);
	if (var == check)
		return terme_values_end_(ptr, args);

	/* enalbe */
	GetConst(SYSTEM_TERME_ENABLE, &check);
	if (var == check)
		return terme_values_enable_(ptr);

	/* error */
	return Result(ret, 0);
}

int terme_values_(Execute ptr, addr var, addr args)
{
	int check;

#ifdef LISP_TERME_WINDOWS
	Return(windows_values_(ptr, var, args, &check));
	if (check)
		return 0;
#endif

	Return(terme_values_operator_(ptr, var, args, &check));
	if (check)
		return 0;

	return fmte_("Invalid keyword ~S.", var, NULL);
}

